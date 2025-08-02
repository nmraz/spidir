use alloc::{collections::VecDeque, vec::Vec};

use bitflags::bitflags;
use cranelift_entity::SecondaryMap;
use itertools::izip;
use log::trace;
use smallvec::SmallVec;

use ir::{
    builder::Builder,
    cache::{CachingBuilder, Entry, NodeCache},
    function::FunctionBody,
    node::{DepValueKind, NodeKind},
    valgraph::{DepValue, Node, ValGraph},
    valwalk::{PostOrder, RawDefUseSuccs},
};

pub fn reduce_body(
    body: &mut FunctionBody,
    node_cache: &mut NodeCache,
    mut f: impl FnMut(&mut ReduceContext<'_>, Node),
) {
    let mut ctx = ReduceContext {
        body,
        node_cache,
        state: &mut ReduceState {
            queue: VecDeque::new(),
            node_flags: SecondaryMap::new(),
        },
    };

    trace!("starting reduction");

    let walk_info = ctx.body.compute_full_walk_info();

    // Use a "raw" postorder here so we can cull any dead nodes as we walk instead of just skipping
    // them. This is important for maintaining correct use counts.
    let node_postorder: Vec<_> = PostOrder::new(
        RawDefUseSuccs::new(ctx.graph()),
        walk_info.roots.iter().copied(),
    )
    .collect();

    for &node in node_postorder.iter().rev() {
        if walk_info.live_nodes.contains(node) {
            ctx.state.enqueue(node);
        } else {
            // Make sure none of `node`'s inputs show up as live uses if they really aren't used.
            ctx.kill_node(node);
        }
    }

    while let Some(node) = ctx.state.dequeue() {
        // A previous value update may have killed one of this node's outputs. Check if that has
        // made the node completely dead and deal with it now.
        if ctx.state.test_and_clear_output_killed(node) && is_node_dead(&ctx.body.graph, node) {
            trace!("dead: {node}");
            // No need to keep processing at this point, just remove the node completely. Once
            // again, we want to make sure that the use counts of any incoming values are correct.
            ctx.kill_node(node);
            continue;
        }

        let node = ctx.cache_node(node);
        if ctx.state.is_reduced(node) {
            // We may have hash-consed the node into a previously-reduced one. No need to re-reduce
            // it in that case.
            continue;
        }

        trace!("reduce: {node}");

        // Assume the node is now reduced. If `f` ends up replacing any of its outputs, it will be
        // marked as non-reduced once more.
        ctx.state.mark_reduced(node);
        f(&mut ctx, node);
    }

    trace!("finished");
}

pub struct ReduceContext<'f> {
    body: &'f mut FunctionBody,
    node_cache: &'f mut NodeCache,
    state: &'f mut ReduceState,
}

impl<'f> ReduceContext<'f> {
    pub fn graph(&self) -> &ValGraph {
        &self.body.graph
    }

    pub fn set_node_input(&mut self, node: Node, index: u32, new_value: DepValue) {
        let old_input = self.graph().node_inputs(node)[index as usize];
        self.state.value_detached(&self.body.graph, old_input);
        self.body.graph.set_node_input(node, index, new_value);
        node_changed(self.state, self.node_cache, node);
    }

    pub fn replace_value(&mut self, old_value: DepValue, new_value: DepValue) {
        let def_node = self.graph().value_def(old_value).0;
        self.replace_value_raw(old_value, new_value);
        self.state
            .enqueue_killed_def_node(&self.body.graph, def_node);
    }

    pub fn replace_value_and_kill(&mut self, old_value: DepValue, new_value: DepValue) {
        let def_node = self.graph().value_def(old_value).0;
        self.kill_node(def_node);
        self.replace_value_raw(old_value, new_value);
    }

    pub fn kill_node(&mut self, node: Node) {
        trace!("    kill: {node}");
        for input in self.body.graph.node_inputs(node) {
            self.state.value_detached(&self.body.graph, input);
        }
        self.body.graph.detach_node_inputs(node);
        self.node_cache.remove(node);
        self.state.mark_node_dead(node);
    }

    fn cache_node(&mut self, node: Node) -> Node {
        if !self.node_cache.contains_node(node) {
            let kind = self.body.graph.node_kind(node);
            if !kind.is_cacheable() {
                return node;
            }

            match self.node_cache.entry(
                &self.body.graph,
                kind,
                self.body.graph.node_inputs(node).into_iter(),
                self.body
                    .graph
                    .node_outputs(node)
                    .into_iter()
                    .map(|output| self.body.graph.value_kind(output)),
            ) {
                Entry::Occupied(existing_node) => {
                    // This node has become equivalent to another node: replace it.

                    trace!("hash: {node} -> {existing_node}");

                    // Kill the node now that we're redirecting all its uses.
                    self.kill_node(node);

                    let old_outputs: SmallVec<[_; 4]> =
                        self.graph().node_outputs(node).into_iter().collect();
                    let new_outputs: SmallVec<[_; 4]> = self
                        .graph()
                        .node_outputs(existing_node)
                        .into_iter()
                        .collect();

                    for (old_output, new_output) in izip!(old_outputs, new_outputs) {
                        self.replace_value_raw(old_output, new_output);
                    }

                    return existing_node;
                }
                Entry::Vacant(entry) => entry.insert(node),
            }
        }

        node
    }

    fn replace_value_raw(&mut self, old_value: DepValue, new_value: DepValue) {
        trace!("    replace: {old_value} -> {new_value}");

        // The node defining this value can no longer be considered reduced, as we have found a
        // better replacement for it.
        let def_node = self.graph().value_def(old_value).0;
        self.state.mark_unreduced(def_node);

        let mut cursor = self.body.graph.value_use_cursor(old_value);
        while let Some((user, _)) = cursor.current() {
            node_changed(self.state, self.node_cache, user);
            cursor.replace_current_with(new_value);
        }
    }
}

impl Builder for ReduceContext<'_> {
    fn create_node(
        &mut self,
        kind: NodeKind,
        inputs: impl IntoIterator<Item = DepValue>,
        output_kinds: impl IntoIterator<Item = DepValueKind>,
    ) -> Node {
        let node =
            CachingBuilder::new(self.body, self.node_cache).create_node(kind, inputs, output_kinds);
        trace!("    create: {node}");
        // We may have reused an existing, reduced node here. No need to revisit in that case.
        if !self.state.is_reduced(node) {
            self.state.enqueue(node);
        }
        node
    }

    fn body(&self) -> &FunctionBody {
        self.body
    }

    fn body_mut(&mut self) -> &mut FunctionBody {
        self.body
    }
}

bitflags! {
    #[derive(Clone, Copy, Default)]
    struct NodeFlags: u8 {
        const ENQUEUED = 0b01;
        const OUTPUT_KILLED = 0b10;
        const REDUCED = 0b100;
        const DEAD = 0b1000;
    }
}

struct ReduceState {
    queue: VecDeque<Node>,
    node_flags: SecondaryMap<Node, NodeFlags>,
}

impl ReduceState {
    fn value_detached(&mut self, graph: &ValGraph, value: DepValue) {
        if graph.value_uses(value).next().is_none() {
            let def_node = graph.value_def(value).0;
            self.enqueue_killed_def_node(graph, def_node);
        }
    }

    fn enqueue_killed_def_node(&mut self, graph: &ValGraph, def_node: Node) {
        if !graph.node_kind(def_node).has_side_effects() {
            // Removing this value might have completely killed its defining node - remember to
            // check back up on it later.
            self.node_flags[def_node].insert(NodeFlags::OUTPUT_KILLED);
            self.enqueue(def_node);
        }
    }

    fn enqueue(&mut self, node: Node) {
        // Avoid re-enqueueing nodes that are already waiting in the queue, and don't bother at all
        // with things that are already dead.
        if !self.node_flags[node].intersects(NodeFlags::ENQUEUED | NodeFlags::DEAD) {
            trace!("    enqueue: {node}");
            self.node_flags[node].insert(NodeFlags::ENQUEUED);
            self.queue.push_back(node);
        }
    }

    fn dequeue(&mut self) -> Option<Node> {
        loop {
            let node = self.queue.pop_front()?;
            self.node_flags[node].remove(NodeFlags::ENQUEUED);

            // A node in the queue may have been killed while it was waiting - skip it.
            if !self.node_flags[node].contains(NodeFlags::DEAD) {
                return Some(node);
            }
        }
    }

    fn test_and_clear_output_killed(&mut self, node: Node) -> bool {
        let ret = self.node_flags[node].contains(NodeFlags::OUTPUT_KILLED);
        self.node_flags[node].remove(NodeFlags::OUTPUT_KILLED);
        ret
    }

    fn is_reduced(&self, node: Node) -> bool {
        self.node_flags[node].contains(NodeFlags::REDUCED)
    }

    fn mark_reduced(&mut self, node: Node) {
        self.node_flags[node].insert(NodeFlags::REDUCED);
    }

    fn mark_unreduced(&mut self, node: Node) {
        self.node_flags[node].remove(NodeFlags::REDUCED);
    }

    fn mark_node_dead(&mut self, node: Node) {
        self.node_flags[node].insert(NodeFlags::DEAD);
    }
}

fn node_changed(state: &mut ReduceState, node_cache: &mut NodeCache, node: Node) {
    // Forget this node's current state now that it's been changed: we'll need to re-hash and
    // re-reduce it when it is reached again.
    node_cache.remove(node);
    state.mark_unreduced(node);
    state.enqueue(node);
}

fn is_node_dead(graph: &ValGraph, node: Node) -> bool {
    if graph.node_kind(node).has_side_effects() {
        return false;
    }

    graph
        .node_outputs(node)
        .into_iter()
        .all(|output| graph.value_uses(output).next().is_none())
}

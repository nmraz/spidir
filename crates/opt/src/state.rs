use core::fmt;

use alloc::{collections::VecDeque, vec::Vec};

use bitflags::bitflags;
use cranelift_entity::SecondaryMap;
use entity_utils::set::DenseEntitySet;
use itertools::izip;
use log::trace;
use smallvec::SmallVec;

use ir::{
    builder::{Builder, BuilderExt},
    cache::{CachingBuilder, Entry, NodeCache},
    function::FunctionBody,
    module::ModuleMetadata,
    node::{DepValueKind, NodeKind},
    valgraph::{DepValue, Node, ValGraph},
    valwalk::{PostOrder, RawDefUseSuccs},
    write::display_node,
};

pub struct FunctionState {
    node_queue: VecDeque<Node>,
    live_nodes: DenseEntitySet<Node>,
    node_flags: SecondaryMap<Node, NodeFlags>,
}

impl FunctionState {
    pub fn populate(
        module_metadata: &ModuleMetadata,
        body: &mut FunctionBody,
        node_cache: &mut NodeCache,
    ) -> Self {
        trace!("preparing function state");

        let walk_info = body.compute_full_walk_info();

        // Use a "raw" postorder here so we can cull any dead nodes as we walk instead of just skipping
        // them. This is important for maintaining correct use counts.
        let node_postorder: Vec<_> = PostOrder::new(
            RawDefUseSuccs::new(&body.graph),
            walk_info.roots.iter().copied(),
        )
        .collect();

        let mut state = Self {
            node_queue: VecDeque::new(),
            live_nodes: walk_info.live_nodes,
            node_flags: SecondaryMap::new(),
        };

        let mut ctx = EditContext::new(module_metadata, body, node_cache, &mut state);
        for &node in node_postorder.iter().rev() {
            if ctx.state.live_nodes.contains(node) {
                ctx.state.enqueue(node);
            } else {
                // Make sure none of `node`'s inputs show up as live uses if they really aren't used.
                ctx.kill_node(node);
            }
        }

        trace!("state prepared");

        state
    }

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
        let flags = self.node_flags[node];
        if self.live_nodes.contains(node) && !flags.contains(NodeFlags::ENQUEUED) {
            trace!("    enqueue: {node}");
            self.node_flags[node].insert(NodeFlags::ENQUEUED);
            self.node_queue.push_back(node);
        }
    }

    fn dequeue(&mut self) -> Option<Node> {
        loop {
            let node = self.node_queue.pop_front()?;
            self.node_flags[node].remove(NodeFlags::ENQUEUED);

            // A node in the queue may have been killed while it was waiting - skip it.
            if self.live_nodes.contains(node) {
                return Some(node);
            }
        }
    }

    fn test_and_clear_output_killed(&mut self, node: Node) -> bool {
        let ret = self.node_flags[node].contains(NodeFlags::OUTPUT_KILLED);
        self.node_flags[node].remove(NodeFlags::OUTPUT_KILLED);
        ret
    }

    fn is_canonical(&self, node: Node) -> bool {
        self.node_flags[node].contains(NodeFlags::CANONICAL)
    }

    fn mark_canonical(&mut self, node: Node) {
        self.node_flags[node].insert(NodeFlags::CANONICAL);
    }

    fn mark_non_canonical(&mut self, node: Node) {
        self.node_flags[node].remove(NodeFlags::CANONICAL);
    }

    fn mark_node_live(&mut self, node: Node) {
        self.live_nodes.insert(node);
    }

    fn mark_node_dead(&mut self, node: Node) {
        self.live_nodes.remove(node);
    }
}

bitflags! {
    #[derive(Clone, Copy, Default)]
    struct NodeFlags: u8 {
        const ENQUEUED = 0b01;
        const OUTPUT_KILLED = 0b10;
        const CANONICAL = 0b100;
    }
}

pub struct EditContext<'m> {
    module_metadata: &'m ModuleMetadata,
    body: &'m mut FunctionBody,
    node_cache: &'m mut NodeCache,
    state: &'m mut FunctionState,
}

impl<'m> EditContext<'m> {
    pub fn new(
        module_metadata: &'m ModuleMetadata,
        body: &'m mut FunctionBody,
        node_cache: &'m mut NodeCache,
        state: &'m mut FunctionState,
    ) -> Self {
        Self {
            module_metadata,
            body,
            node_cache,
            state,
        }
    }

    pub fn live_nodes(&self) -> &DenseEntitySet<Node> {
        &self.state.live_nodes
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
        trace!("    kill: {node} ({})", self.display_node(node));
        for input in self.body.graph.node_inputs(node) {
            self.state.value_detached(&self.body.graph, input);
        }
        self.body.graph.detach_node_inputs(node);
        self.node_cache.remove(node);
        self.state.mark_node_dead(node);
    }

    pub fn canonicalize_outstanding(&mut self) {
        trace!("starting canonicalization");

        while let Some(node) = self.state.dequeue() {
            // A previous value update may have killed one of this node's outputs. Check if that has
            // made the node completely dead and deal with it now.
            if self.state.test_and_clear_output_killed(node) && is_node_dead(&self.body.graph, node)
            {
                trace!("dead: {node} ({})", self.display_node(node));
                // No need to keep processing at this point, just remove the node completely. Once
                // again, we want to make sure that the use counts of any incoming values are correct.
                self.kill_node(node);
                continue;
            }

            let node = self.cache_node(node);
            if self.state.is_canonical(node) {
                // We may have hash-consed the node into an already-canonical one. No need to
                // re-canonicalize it in that case.
                continue;
            }

            trace!("canonicalize: {node} ({})", self.display_node(node));

            // Assume the node is now canonical. If `f` ends up replacing any of its outputs, it
            // will be marked as non-canonical once more.
            self.state.mark_canonical(node);

            crate::canonicalize::canonicalize_node(self, node);
        }

        trace!("finished");
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

                    trace!(
                        "hash: {node} -> {existing_node} ({})",
                        self.display_node(existing_node)
                    );

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

        // The node defining this value can no longer be considered canonical, as we have found a
        // better replacement for it.
        let def_node = self.graph().value_def(old_value).0;
        self.state.mark_non_canonical(def_node);

        let mut cursor = self.body.graph.value_use_cursor(old_value);
        while let Some((user, _)) = cursor.current() {
            node_changed(self.state, self.node_cache, user);
            cursor.replace_current_with(new_value);
        }
    }

    fn display_node(&self, node: Node) -> impl fmt::Display + '_ {
        display_node(self.module_metadata, self.body, node)
    }
}

impl Builder for EditContext<'_> {
    fn create_node(
        &mut self,
        kind: NodeKind,
        inputs: impl IntoIterator<Item = DepValue>,
        output_kinds: impl IntoIterator<Item = DepValueKind>,
    ) -> Node {
        let node =
            CachingBuilder::new(self.body, self.node_cache).create_node(kind, inputs, output_kinds);
        trace!("    create: {node} ({})", self.display_node(node));
        // We may have reused an existing, canonical node here. No need to revisit in that case.
        if !self.state.is_canonical(node) {
            // Assume we never construct dead nodes and just mark the node as live right now. If
            // some constructed nodes do end up being dead, we can set `OUTPUT_KILLED` here as well.
            self.state.mark_node_live(node);
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

fn node_changed(state: &mut FunctionState, node_cache: &mut NodeCache, node: Node) {
    // Forget this node's current state now that it's been changed: we'll need to re-hash and
    // re-canonicalize it when it is reached again.
    node_cache.remove(node);
    state.mark_non_canonical(node);
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

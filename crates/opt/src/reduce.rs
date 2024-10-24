use alloc::collections::VecDeque;

use entity_set::DenseEntitySet;
use itertools::izip;
use log::trace;
use smallvec::SmallVec;

use ir::{
    builder::Builder,
    cache::{CachingBuilder, Entry, NodeCache},
    function::FunctionBody,
    node::{DepValueKind, NodeKind},
    valgraph::{DepValue, Node, ValGraph},
};

pub fn reduce_body(
    body: &mut FunctionBody,
    node_cache: &mut NodeCache,
    mut f: impl FnMut(&mut ReduceContext<'_>, Node),
) {
    let mut ctx = ReduceContext {
        body,
        state: ReduceState {
            queue: VecDeque::new(),
            enqueued: DenseEntitySet::new(),
            node_cache,
            reduced: DenseEntitySet::new(),
        },
    };

    trace!("starting reduction");

    for root in ctx.body.compute_live_nodes().reverse_postorder(ctx.graph()) {
        ctx.state.enqueue(root);
    }

    while let Some(node) = ctx.state.dequeue() {
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
    state: ReduceState<'f>,
}

impl<'f> ReduceContext<'f> {
    pub fn graph(&self) -> &ValGraph {
        &self.body.graph
    }

    pub fn builder(&mut self) -> ReducerBuilder<'f, '_> {
        ReducerBuilder {
            body: self.body,
            state: &mut self.state,
        }
    }

    pub fn replace_value(&mut self, old_value: DepValue, new_value: DepValue) {
        self.state
            .replace_value(&mut self.body.graph, old_value, new_value);
    }

    pub fn set_node_input(&mut self, node: Node, index: u32, new_value: DepValue) {
        self.body.graph.set_node_input(node, index, new_value);
        self.state.node_changed(node);
    }

    pub fn kill_node(&mut self, node: Node) {
        self.body.graph.detach_node_inputs(node);
        self.state.invalidate_node(node);
    }

    fn cache_node(&mut self, node: Node) -> Node {
        self.state.cache_node(&mut self.body.graph, node)
    }
}

pub struct ReducerBuilder<'f, 'c> {
    body: &'c mut FunctionBody,
    state: &'c mut ReduceState<'f>,
}

impl Builder for ReducerBuilder<'_, '_> {
    fn create_node(
        &mut self,
        kind: NodeKind,
        inputs: impl IntoIterator<Item = DepValue>,
        output_kinds: impl IntoIterator<Item = DepValueKind>,
    ) -> Node {
        let node = CachingBuilder::new(self.body, self.state.node_cache).create_node(
            kind,
            inputs,
            output_kinds,
        );
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

struct ReduceState<'f> {
    node_cache: &'f mut NodeCache,
    queue: VecDeque<Node>,
    enqueued: DenseEntitySet<Node>,
    reduced: DenseEntitySet<Node>,
}

impl ReduceState<'_> {
    fn cache_node(&mut self, graph: &mut ValGraph, node: Node) -> Node {
        if !self.node_cache.contains_node(node) {
            let kind = graph.node_kind(node);
            if !kind.is_cacheable() {
                return node;
            }

            match self.node_cache.entry(
                graph,
                kind,
                graph.node_inputs(node).into_iter(),
                graph
                    .node_outputs(node)
                    .into_iter()
                    .map(|output| graph.value_kind(output)),
            ) {
                Entry::Occupied(existing_node) => {
                    // This node has become equivalent to another node: replace it.

                    trace!("hash: {node} -> {existing_node}");

                    let old_outputs: SmallVec<[_; 4]> =
                        graph.node_outputs(node).into_iter().collect();
                    let new_outputs: SmallVec<[_; 4]> =
                        graph.node_outputs(existing_node).into_iter().collect();

                    for (old_output, new_output) in izip!(old_outputs, new_outputs) {
                        self.replace_value(graph, old_output, new_output);
                    }

                    return existing_node;
                }
                Entry::Vacant(entry) => entry.insert(node),
            }
        }

        node
    }

    fn replace_value(&mut self, graph: &mut ValGraph, old_value: DepValue, new_value: DepValue) {
        trace!("    replace: {old_value} -> {new_value}");

        // The node defining this value can no longer be considered reduced, as we have found a
        // better replacement for it.
        let def_node = graph.value_def(old_value).0;
        self.mark_unreduced(def_node);

        let mut cursor = graph.value_use_cursor(old_value);
        while let Some((user, _)) = cursor.current() {
            self.node_changed(user);
            cursor.replace_current_with(new_value);
        }
    }

    fn node_changed(&mut self, node: Node) {
        // Forget this node's current state now that it's been changed: we'll need to re-hash and
        // re-reduce it when it is reached again.
        self.invalidate_node(node);
        self.enqueue(node);
    }

    fn invalidate_node(&mut self, node: Node) {
        self.node_cache.remove(node);
        self.reduced.remove(node);
    }

    fn enqueue(&mut self, node: Node) {
        if !self.enqueued.contains(node) {
            trace!("    enqueue: {node}");
            self.enqueued.insert(node);
            self.queue.push_back(node);
        }
    }

    fn dequeue(&mut self) -> Option<Node> {
        let node = self.queue.pop_front()?;
        self.enqueued.remove(node);
        Some(node)
    }

    fn is_reduced(&self, node: Node) -> bool {
        self.reduced.contains(node)
    }

    fn mark_reduced(&mut self, node: Node) {
        self.reduced.insert(node);
    }

    fn mark_unreduced(&mut self, node: Node) {
        self.reduced.remove(node);
    }
}

use alloc::collections::VecDeque;

use entity_set::DenseEntitySet;
use itertools::izip;
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
        node_cache,
        worklist: Worklist {
            queue: VecDeque::new(),
            enqueued: DenseEntitySet::new(),
        },
        reduced: DenseEntitySet::new(),
    };

    for root in ctx.body.compute_live_nodes().reverse_postorder(ctx.graph()) {
        ctx.worklist.enqueue(root);
    }

    while let Some(node) = ctx.worklist.dequeue() {
        let node = ctx.cache_node(node);
        if ctx.reduced.contains(node) {
            // We may have hash-consed the node into a previously-reduced one. No need to re-reduce
            // it in that case.
            continue;
        }

        // Assume the node is now reduced. If `f` ends up replacing any of its outputs, it will be
        // marked as non-reduced once more.
        ctx.reduced.insert(node);
        f(&mut ctx, node);
    }
}

pub struct ReduceContext<'f> {
    body: &'f mut FunctionBody,
    node_cache: &'f mut NodeCache,
    worklist: Worklist,
    reduced: DenseEntitySet<Node>,
}

impl<'f> ReduceContext<'f> {
    pub fn graph(&self) -> &ValGraph {
        &self.body.graph
    }

    pub fn builder(&mut self) -> ReducerBuilder<'_> {
        ReducerBuilder {
            inner: CachingBuilder::new(self.body, self.node_cache),
            worklist: &mut self.worklist,
            reduced: &self.reduced,
        }
    }

    pub fn replace_value(&mut self, old_value: DepValue, new_value: DepValue) {
        // The node defining this value can no longer be considered reduced, as we have found a
        // better replacement for it.
        let def_node = self.body.graph.value_def(old_value).0;
        self.reduced.remove(def_node);

        let mut cursor = self.body.graph.value_use_cursor(old_value);
        while let Some((user, _)) = cursor.current() {
            // Forget this user's current state now that we're changing it: we'll need to re-hash
            // and re-reduce it when it is reached again.
            self.node_cache.remove(user);
            self.reduced.remove(user);
            self.worklist.enqueue(user);

            cursor.replace_current_with(new_value);
        }
    }

    fn cache_node(&mut self, node: Node) -> Node {
        if !self.node_cache.contains_node(node) {
            let graph = &mut self.body.graph;

            match self.node_cache.entry(
                graph,
                graph.node_kind(node),
                graph.node_inputs(node).into_iter(),
                graph
                    .node_outputs(node)
                    .into_iter()
                    .map(|output| graph.value_kind(output)),
            ) {
                Some(Entry::Occupied(existing_node)) => {
                    // This node has become equivalent to another node: replace it.

                    let old_outputs: SmallVec<[_; 4]> =
                        graph.node_outputs(node).into_iter().collect();
                    let new_outputs: SmallVec<[_; 4]> =
                        graph.node_outputs(existing_node).into_iter().collect();

                    for (old_output, new_output) in izip!(old_outputs, new_outputs) {
                        self.replace_value(old_output, new_output);
                    }

                    return existing_node;
                }
                Some(Entry::Vacant(entry)) => entry.insert(node),
                None => {
                    // This node can't be cached.
                }
            }
        }

        node
    }
}

pub struct ReducerBuilder<'a> {
    inner: CachingBuilder<'a>,
    worklist: &'a mut Worklist,
    reduced: &'a DenseEntitySet<Node>,
}

impl<'a> Builder for ReducerBuilder<'a> {
    fn create_node(
        &mut self,
        kind: NodeKind,
        inputs: impl IntoIterator<Item = DepValue>,
        output_kinds: impl IntoIterator<Item = DepValueKind>,
    ) -> Node {
        let node = self.inner.create_node(kind, inputs, output_kinds);
        // We may have reused an existing, reduced node here. No need to revisit in that case.
        if !self.reduced.contains(node) {
            self.worklist.enqueue(node);
        }
        node
    }

    fn body(&self) -> &FunctionBody {
        self.inner.body()
    }

    fn body_mut(&mut self) -> &mut FunctionBody {
        self.inner.body_mut()
    }
}

struct Worklist {
    queue: VecDeque<Node>,
    enqueued: DenseEntitySet<Node>,
}

impl Worklist {
    fn enqueue(&mut self, node: Node) {
        if !self.enqueued.contains(node) {
            self.enqueued.insert(node);
            self.queue.push_back(node);
        }
    }

    fn dequeue(&mut self) -> Option<Node> {
        let node = self.queue.pop_front()?;
        self.enqueued.remove(node);
        Some(node)
    }
}

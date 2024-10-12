use alloc::collections::VecDeque;

use itertools::izip;
use smallvec::SmallVec;

use fx_utils::FxHashSet;
use ir::{
    cache::{CachingBuilder, Entry, NodeCache},
    function::FunctionBody,
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
            enqueued: FxHashSet::default(),
        },
    };

    for root in ctx.body.compute_live_nodes().reverse_postorder(ctx.graph()) {
        ctx.worklist.enqueue(root);
    }

    while let Some(node) = ctx.worklist.dequeue() {
        // TODO: This can cause redundant invocations of `f` on nodes that have already been
        // reduced.
        let node = ctx.cache_node(node);
        f(&mut ctx, node);
    }
}

pub struct ReduceContext<'f> {
    body: &'f mut FunctionBody,
    node_cache: &'f mut NodeCache,
    worklist: Worklist,
}

impl<'f> ReduceContext<'f> {
    pub fn graph(&self) -> &ValGraph {
        &self.body.graph
    }

    pub fn builder(&mut self) -> CachingBuilder<'_> {
        CachingBuilder::new(self.body, self.node_cache)
    }

    pub fn replace_value(&mut self, old_value: DepValue, new_value: DepValue) {
        let mut cursor = self.body.graph.value_use_cursor(old_value);
        while let Some((node, _)) = cursor.current() {
            self.node_cache.remove(node);
            self.worklist.enqueue(node);
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

struct Worklist {
    queue: VecDeque<Node>,
    enqueued: FxHashSet<Node>,
}

impl Worklist {
    fn enqueue(&mut self, node: Node) {
        if !self.enqueued.contains(&node) {
            self.queue.push_back(node);
        }
    }

    fn dequeue(&mut self) -> Option<Node> {
        let node = self.queue.pop_front()?;
        self.enqueued.remove(&node);
        Some(node)
    }
}

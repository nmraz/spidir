#![no_std]

extern crate alloc;

use alloc::vec::Vec;
use core::ops::ControlFlow;

use cranelift_entity::EntityRef;
use entity_set::DenseEntitySet;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WalkPhase {
    Pre,
    Post,
}

pub trait GraphRef {
    type Node: Copy;

    fn try_successors(
        &self,
        node: Self::Node,
        f: impl FnMut(Self::Node) -> ControlFlow<()>,
    ) -> ControlFlow<()>;

    fn successors(&self, node: Self::Node, mut f: impl FnMut(Self::Node)) {
        let _ = self.try_successors(node, |succ| {
            f(succ);
            ControlFlow::Continue(())
        });
    }
}

impl<G: GraphRef> GraphRef for &'_ G {
    type Node = G::Node;

    fn try_successors(
        &self,
        node: Self::Node,
        f: impl FnMut(Self::Node) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        (*self).try_successors(node, f)
    }
}
pub trait PredGraphRef: GraphRef {
    fn try_predecessors(
        &self,
        node: Self::Node,
        f: impl FnMut(Self::Node) -> ControlFlow<()>,
    ) -> ControlFlow<()>;

    fn predecessors(&self, node: Self::Node, mut f: impl FnMut(Self::Node)) {
        let _ = self.try_predecessors(node, |pred| {
            f(pred);
            ControlFlow::Continue(())
        });
    }
}

impl<G: PredGraphRef> PredGraphRef for &'_ G {
    fn try_predecessors(
        &self,
        node: Self::Node,
        f: impl FnMut(Self::Node) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        (*self).try_predecessors(node, f)
    }
}

pub trait VisitTracker<N>: Default {
    fn is_visited(&self, node: N) -> bool;
    fn mark_visited(&mut self, node: N);
}

#[derive(Default, Clone, Copy)]
pub struct NopTracker;
impl<N> VisitTracker<N> for NopTracker {
    fn is_visited(&self, _node: N) -> bool {
        false
    }

    fn mark_visited(&mut self, _node: N) {}
}

impl<N: EntityRef> VisitTracker<N> for DenseEntitySet<N> {
    fn is_visited(&self, node: N) -> bool {
        self.contains(node)
    }

    fn mark_visited(&mut self, node: N) {
        self.insert(node);
    }
}

#[derive(Debug)]
pub struct PreOrderContext<N> {
    stack: Vec<N>,
}

impl<N: Copy> PreOrderContext<N> {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn reset(&mut self, roots: impl IntoIterator<Item = N>) {
        self.stack.clear();
        self.stack.extend(roots);
    }

    pub fn next(
        &mut self,
        graph: impl GraphRef<Node = N>,
        visited: &mut impl VisitTracker<N>,
    ) -> Option<N> {
        let node = loop {
            let node = self.stack.pop()?;
            if !visited.is_visited(node) {
                break node;
            }
        };

        visited.mark_visited(node);

        let _ = graph.try_successors(node, |succ| {
            // This extra check here is an optimization to avoid needlessly placing
            // an obviously-visited node on to the stack. Even if the node is not
            // visited now, it may be by the time it is popped off the stack later.
            if !visited.is_visited(succ) {
                self.stack.push(succ);
            }

            ControlFlow::Continue(())
        });

        Some(node)
    }
}

impl<N: Copy> Default for PreOrderContext<N> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct PreOrder<G: GraphRef, V> {
    pub graph: G,
    pub visited: V,
    ctx: PreOrderContext<G::Node>,
}

impl<G: GraphRef, V: VisitTracker<G::Node>> PreOrder<G, V> {
    pub fn new(graph: G, roots: impl IntoIterator<Item = G::Node>) -> Self {
        let mut ctx = PreOrderContext::new();
        ctx.reset(roots);
        Self {
            graph,
            visited: V::default(),
            ctx,
        }
    }
}

impl<G: GraphRef, V: VisitTracker<G::Node>> Iterator for PreOrder<G, V> {
    type Item = G::Node;

    fn next(&mut self) -> Option<G::Node> {
        self.ctx.next(&self.graph, &mut self.visited)
    }
}

pub fn entity_preorder<G: GraphRef>(
    graph: G,
    roots: impl IntoIterator<Item = G::Node>,
) -> PreOrder<G, DenseEntitySet<G::Node>>
where
    G::Node: EntityRef,
{
    PreOrder::new(graph, roots)
}

pub type TreePreOrder<G> = PreOrder<G, NopTracker>;

pub struct PostOrderContext<N> {
    stack: Vec<(WalkPhase, N)>,
}

impl<N: Copy> PostOrderContext<N> {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn reset(&mut self, roots: impl IntoIterator<Item = N>) {
        self.stack.clear();

        // Note: push the roots onto the stack in source order so that this order is preserved in
        // any RPO. Specifically, we want to guarantee that if `u` precedes `v` in `roots` and there
        // isn't a path from `v` to `u` in the graph, then `u` will still precede `v` in any RPO
        // obtained from this graph walk. Pushing the nodes onto the stack in order guarantees this,
        // as it ensures that `v` is always visited before `u`.
        //
        // Some clients depend on this behavior: for example, the live-node RPO of a function graph
        // should always start with its entry node, and the topological sort performed during
        // scheduling is supposed to preserve block headers and terminators.
        self.stack
            .extend(roots.into_iter().map(|node| (WalkPhase::Pre, node)));
    }

    pub fn next(
        &mut self,
        graph: impl GraphRef<Node = N>,
        visited: &mut impl VisitTracker<N>,
    ) -> Option<N> {
        loop {
            let (phase, node) = self.next_event(&graph, visited)?;
            if phase == WalkPhase::Post {
                return Some(node);
            }
        }
    }

    pub fn next_event(
        &mut self,
        graph: impl GraphRef<Node = N>,
        visited: &mut impl VisitTracker<N>,
    ) -> Option<(WalkPhase, N)> {
        loop {
            let (phase, node) = self.stack.pop()?;
            match phase {
                WalkPhase::Pre => {
                    if !visited.is_visited(node) {
                        visited.mark_visited(node);
                        self.stack.push((WalkPhase::Post, node));
                        let _ = graph.try_successors(node, |succ| {
                            // This extra check here is an optimization to avoid needlessly placing
                            // an obviously-visited node on to the stack. Even if the node is not
                            // visited now, it may be by the time it is popped off the stack later.
                            if !visited.is_visited(succ) {
                                self.stack.push((WalkPhase::Pre, succ));
                            }

                            ControlFlow::Continue(())
                        });

                        return Some((WalkPhase::Pre, node));
                    }
                }
                WalkPhase::Post => {
                    return Some((WalkPhase::Post, node));
                }
            }
        }
    }
}

impl<N: Copy> Default for PostOrderContext<N> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct PostOrder<G: GraphRef, V> {
    pub graph: G,
    pub visited: V,
    ctx: PostOrderContext<G::Node>,
}

impl<G: GraphRef, V: VisitTracker<G::Node>> PostOrder<G, V> {
    pub fn new(graph: G, roots: impl IntoIterator<Item = G::Node>) -> Self {
        let mut ctx = PostOrderContext::new();
        ctx.reset(roots);
        Self {
            graph,
            visited: V::default(),
            ctx,
        }
    }

    pub fn next_event(&mut self) -> Option<(WalkPhase, G::Node)> {
        self.ctx.next_event(&self.graph, &mut self.visited)
    }
}

impl<G: GraphRef, V: VisitTracker<G::Node>> Iterator for PostOrder<G, V> {
    type Item = G::Node;

    fn next(&mut self) -> Option<G::Node> {
        self.ctx.next(&self.graph, &mut self.visited)
    }
}

pub fn entity_postorder<G: GraphRef>(
    graph: G,
    roots: impl IntoIterator<Item = G::Node>,
) -> PostOrder<G, DenseEntitySet<G::Node>>
where
    G::Node: EntityRef,
{
    PostOrder::new(graph, roots)
}

pub type TreePostOrder<G> = PostOrder<G, NopTracker>;

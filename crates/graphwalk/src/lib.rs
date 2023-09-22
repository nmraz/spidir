#![no_std]

extern crate alloc;

use alloc::vec::Vec;
use cranelift_entity::{EntityRef, EntitySet};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WalkPhase {
    Pre,
    Post,
}

pub trait Graph {
    type Node: Copy;
    fn successors(&self, node: Self::Node, f: impl FnMut(Self::Node));
}

pub trait PredGraph: Graph {
    fn predecessors(&self, node: Self::Node, f: impl FnMut(Self::Node));
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

impl<N: EntityRef> VisitTracker<N> for EntitySet<N> {
    fn is_visited(&self, node: N) -> bool {
        self.contains(node)
    }

    fn mark_visited(&mut self, node: N) {
        self.insert(node);
    }
}

pub struct PreOrderContext<G: Graph> {
    graph: G,
    stack: Vec<G::Node>,
}

impl<G: Graph> PreOrderContext<G> {
    pub fn new(graph: G, roots: impl IntoIterator<Item = G::Node>) -> Self {
        let mut res = Self {
            graph,
            stack: Vec::new(),
        };
        res.reset(roots);
        res
    }

    pub fn reset(&mut self, roots: impl IntoIterator<Item = G::Node>) {
        self.stack.clear();
        self.stack.extend(roots);
    }

    pub fn next(&mut self, visited: &mut impl VisitTracker<G::Node>) -> Option<G::Node> {
        let node = loop {
            let node = self.stack.pop()?;
            if !visited.is_visited(node) {
                break node;
            }
        };

        visited.mark_visited(node);

        self.graph.successors(node, |succ| {
            // This extra check here is an optimization to avoid needlessly placing
            // an obviously-visited node on to the stack. Even if the node is not
            // visited now, it may be by the time it is popped off the stack later.
            if !visited.is_visited(succ) {
                self.stack.push(succ);
            }
        });

        Some(node)
    }
}

pub struct PreOrder<G: Graph, V> {
    pub visited: V,
    ctx: PreOrderContext<G>,
}

impl<G: Graph, V: VisitTracker<G::Node>> PreOrder<G, V> {
    pub fn new(graph: G, roots: impl IntoIterator<Item = G::Node>) -> Self {
        Self {
            visited: V::default(),
            ctx: PreOrderContext::new(graph, roots),
        }
    }
}

impl<G: Graph, V: VisitTracker<G::Node>> Iterator for PreOrder<G, V> {
    type Item = G::Node;

    fn next(&mut self) -> Option<G::Node> {
        self.ctx.next(&mut self.visited)
    }
}

pub type TreePreOrder<G> = PreOrder<G, NopTracker>;

pub struct PostOrderContext<G: Graph> {
    graph: G,
    stack: Vec<(WalkPhase, G::Node)>,
}

impl<G: Graph> PostOrderContext<G> {
    pub fn new(graph: G, roots: impl IntoIterator<Item = G::Node>) -> Self {
        let mut res = Self {
            graph,
            stack: Vec::new(),
        };
        res.reset(roots);
        res
    }

    pub fn reset(&mut self, roots: impl IntoIterator<Item = G::Node>) {
        self.stack.clear();

        // Note: push the roots onto the stack in source order so that this order is preserved in
        // any RPO. Some clients depend on this: for example, the live-node RPO of a function graph
        // should always start with its entry node.
        self.stack
            .extend(roots.into_iter().map(|node| (WalkPhase::Pre, node)));
    }

    pub fn next(&mut self, visited: &mut impl VisitTracker<G::Node>) -> Option<G::Node> {
        loop {
            let (phase, node) = self.next_event(visited)?;
            if phase == WalkPhase::Post {
                return Some(node);
            }
        }
    }

    pub fn next_event(
        &mut self,
        visited: &mut impl VisitTracker<G::Node>,
    ) -> Option<(WalkPhase, G::Node)> {
        loop {
            let (phase, node) = self.stack.pop()?;
            match phase {
                WalkPhase::Pre => {
                    if !visited.is_visited(node) {
                        visited.mark_visited(node);
                        self.stack.push((WalkPhase::Post, node));
                        self.graph.successors(node, |succ| {
                            // This extra check here is an optimization to avoid needlessly placing
                            // an obviously-visited node on to the stack. Even if the node is not
                            // visited now, it may be by the time it is popped off the stack later.
                            if !visited.is_visited(succ) {
                                self.stack.push((WalkPhase::Pre, succ));
                            }
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

pub struct PostOrder<G: Graph, V> {
    pub visited: V,
    ctx: PostOrderContext<G>,
}

impl<G: Graph, V: VisitTracker<G::Node>> PostOrder<G, V> {
    pub fn new(graph: G, roots: impl IntoIterator<Item = G::Node>) -> Self {
        Self {
            visited: V::default(),
            ctx: PostOrderContext::new(graph, roots),
        }
    }

    pub fn next_event(&mut self) -> Option<(WalkPhase, G::Node)> {
        self.ctx.next_event(&mut self.visited)
    }
}

impl<G: Graph, V: VisitTracker<G::Node>> Iterator for PostOrder<G, V> {
    type Item = G::Node;

    fn next(&mut self) -> Option<G::Node> {
        self.ctx.next(&mut self.visited)
    }
}

pub type TreePostOrder<G> = PostOrder<G, NopTracker>;

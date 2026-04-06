#![no_std]

extern crate alloc;

use core::ops::ControlFlow;

pub mod dfs;

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

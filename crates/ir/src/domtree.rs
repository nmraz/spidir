use crate::{
    valgraph::{Node, ValGraph},
    valwalk::ForwardCfg,
};

pub use dominators::domtree::DomTreeNode;
pub type DomTree = dominators::domtree::DomTree<Node>;

pub fn compute(graph: &ValGraph, entry: Node) -> DomTree {
    DomTree::compute(ForwardCfg::new(graph), entry)
}

#[cfg(test)]
mod tests;

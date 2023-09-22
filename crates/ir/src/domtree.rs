use crate::valgraph::Node;

pub use dominators::domtree::DomTreeNode;
pub type DomTree = dominators::domtree::DomTree<Node>;

#[cfg(test)]
mod tests;

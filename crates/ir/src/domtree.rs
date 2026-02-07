use crate::valgraph::Node;

pub type DomTreeNode = dominators::domtree::DomTreeNode<Node>;
pub type DomTree = dominators::domtree::DomTree<Node>;

#[cfg(test)]
mod tests;

use crate::valgraph::Node;

pub type ValDomTreeNode = dominators::domtree::DomTreeNode<Node>;
pub type ValDomTree = dominators::domtree::DomTree<Node>;

#[cfg(test)]
mod tests;

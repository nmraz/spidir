use crate::valgraph::Node;

pub type Loop = dominators::loops::Loop<Node>;
pub type LoopForest = dominators::loops::LoopForest<Node>;

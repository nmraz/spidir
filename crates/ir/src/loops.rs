use crate::valgraph::Node;

pub type ValLoop = dominators::loops::Loop<Node>;
pub type ValLoopForest = dominators::loops::LoopForest<Node>;

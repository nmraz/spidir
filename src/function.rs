use alloc::vec::Vec;
use core::iter;

use crate::valgraph::{DepValueKind, Node, NodeKind, Type, ValGraph};

pub struct Function {
    pub ret_type: Type,
    pub arg_types: Vec<Type>,
    pub valgraph: ValGraph,
    pub entry_node: Node,
}

impl Function {
    pub fn with_signature(ret_type: Type, arg_types: Vec<Type>) -> Self {
        let mut valgraph = ValGraph::new();
        let entry_node = valgraph.create_node(
            NodeKind::Entry,
            [],
            iter::once(DepValueKind::Control)
                .chain(arg_types.iter().map(|&ty| DepValueKind::Value(ty))),
        );
        Self {
            ret_type,
            arg_types,
            valgraph,
            entry_node,
        }
    }
}

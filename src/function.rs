use alloc::vec::Vec;
use core::iter;

use crate::valgraph::{DepValueKind, Node, NodeKind, Type, ValGraph};

pub struct Signature {
    pub ret_type: Type,
    pub arg_types: Vec<Type>,
}

pub struct Function {
    pub sig: Signature,
    pub valgraph: ValGraph,
    pub entry_node: Node,
}

impl Function {
    pub fn with_signature(sig: Signature) -> Self {
        let mut valgraph = ValGraph::new();
        let entry_node = valgraph.create_node(
            NodeKind::Entry,
            [],
            iter::once(DepValueKind::Control)
                .chain(sig.arg_types.iter().map(|&ty| DepValueKind::Value(ty))),
        );
        Self {
            sig,
            valgraph,
            entry_node,
        }
    }
}

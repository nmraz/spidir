use alloc::vec::Vec;
use core::iter;
use cranelift_entity::{entity_impl, PrimaryMap};

use crate::valgraph::{DepValueKind, Node, NodeKind, Type, ValGraph};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function(u32);
entity_impl!(Function, "@");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExternFunction(u32);
entity_impl!(ExternFunction, "@");

pub struct Signature {
    pub ret_type: Type,
    pub arg_types: Vec<Type>,
}

pub struct FunctionData {
    pub sig: Signature,
    pub valgraph: ValGraph,
    pub entry_node: Node,
}

impl FunctionData {
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

pub struct Module {
    pub functions: PrimaryMap<Function, FunctionData>,
    pub extern_functions: PrimaryMap<ExternFunction, Signature>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            functions: PrimaryMap::new(),
            extern_functions: PrimaryMap::new(),
        }
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

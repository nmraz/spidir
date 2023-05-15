use alloc::{string::String, vec::Vec};
use core::iter;

use cranelift_entity::{entity_impl, PrimaryMap};

use crate::{
    node::{DepValueKind, NodeKind, Type},
    valgraph::{Node, ValGraph},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function(u32);
entity_impl!(Function, "func");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExternFunction(u32);
entity_impl!(ExternFunction, "extfunc");

pub struct Signature {
    pub ret_type: Type,
    pub arg_types: Vec<Type>,
}

pub struct FunctionData {
    pub name: String,
    pub sig: Signature,
    pub valgraph: ValGraph,
    pub entry_node: Node,
}

impl FunctionData {
    pub fn new(name: String, sig: Signature) -> Self {
        let mut valgraph = ValGraph::new();
        let entry_node = valgraph.create_node(
            NodeKind::Entry,
            [],
            iter::once(DepValueKind::Control)
                .chain(sig.arg_types.iter().map(|&ty| DepValueKind::Value(ty))),
        );
        Self {
            name,
            sig,
            valgraph,
            entry_node,
        }
    }
}

pub struct ExternFunctionData {
    pub name: String,
    pub sig: Signature,
}

pub struct Module {
    pub functions: PrimaryMap<Function, FunctionData>,
    pub extern_functions: PrimaryMap<ExternFunction, ExternFunctionData>,
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

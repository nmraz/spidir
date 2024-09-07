use alloc::{string::String, vec::Vec};
use core::{fmt, iter};

use cranelift_entity::{entity_impl, packed_option::ReservedValue, PrimaryMap};

use crate::{
    node::{DepValueKind, FunctionRef, NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
    write::{write_function_metadata, write_module},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function(u32);
entity_impl!(Function, "func");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExternFunction(u32);
entity_impl!(ExternFunction, "extfunc");

#[derive(Debug, Clone)]
pub struct Signature {
    pub ret_type: Option<Type>,
    pub param_types: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct FunctionMetadata {
    pub name: String,
    pub sig: Signature,
}

impl fmt::Display for FunctionMetadata {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_function_metadata(f, self)
    }
}

#[derive(Clone)]
pub struct FunctionBody {
    pub graph: ValGraph,
    pub entry: Node,
}

impl FunctionBody {
    pub fn new_invalid() -> Self {
        Self {
            graph: ValGraph::new(),
            entry: Node::reserved_value(),
        }
    }

    pub fn new(param_types: &[Type]) -> Self {
        let mut body = Self::new_invalid();

        let entry = body.graph.create_node(
            NodeKind::Entry,
            [],
            iter::once(DepValueKind::Control)
                .chain(param_types.iter().map(|&ty| DepValueKind::Value(ty))),
        );
        body.entry = entry;

        body
    }

    pub fn entry_ctrl(&self) -> DepValue {
        self.graph.node_outputs(self.entry)[0]
    }

    pub fn param_value(&self, index: u32) -> DepValue {
        self.graph.node_outputs(self.entry)[index as usize + 1]
    }
}

#[derive(Clone)]
pub struct FunctionData {
    pub metadata: FunctionMetadata,
    pub body: FunctionBody,
}

impl FunctionData {
    pub fn new(name: String, sig: Signature) -> Self {
        let body = FunctionBody::new(&sig.param_types);
        Self {
            metadata: FunctionMetadata { name, sig },
            body,
        }
    }
}

#[derive(Clone)]
pub struct Module {
    pub functions: PrimaryMap<Function, FunctionData>,
    pub extern_functions: PrimaryMap<ExternFunction, FunctionMetadata>,
}

impl Module {
    #[inline]
    pub fn new() -> Self {
        Self {
            functions: PrimaryMap::new(),
            extern_functions: PrimaryMap::new(),
        }
    }

    pub fn resolve_funcref(&self, funcref: FunctionRef) -> &FunctionMetadata {
        match funcref {
            FunctionRef::Internal(func) => &self.functions[func].metadata,
            FunctionRef::External(func) => &self.extern_functions[func],
        }
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_module(f, self)
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

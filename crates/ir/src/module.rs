use alloc::{string::String, vec::Vec};
use core::fmt;

use cranelift_entity::{entity_impl, PrimaryMap};

use crate::{
    builder::{BuilderExt, SimpleBuilder},
    node::{FunctionRef, Type},
    valgraph::{Node, ValGraph},
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

#[derive(Clone)]
pub struct FunctionData {
    pub metadata: FunctionMetadata,
    pub body: FunctionBody,
}

impl FunctionData {
    pub fn new(name: String, sig: Signature) -> Self {
        let mut graph = ValGraph::new();
        let entry = SimpleBuilder(&mut graph).build_entry(&sig.param_types);
        Self {
            metadata: FunctionMetadata { name, sig },
            body: FunctionBody {
                graph,
                entry: entry.node,
            },
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

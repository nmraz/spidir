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

#[derive(Clone)]
pub struct FunctionData {
    pub name: String,
    pub sig: Signature,
    pub graph: ValGraph,
    pub entry: Node,
}

impl FunctionData {
    pub fn new(name: String, sig: Signature) -> Self {
        let mut graph = ValGraph::new();
        let entry = SimpleBuilder(&mut graph).build_entry(&sig.param_types);
        Self {
            name,
            sig,
            graph,
            entry: entry.node,
        }
    }

    pub fn metadata(&self) -> FunctionMetadata<'_> {
        FunctionMetadata {
            name: &self.name,
            sig: &self.sig,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternFunctionData {
    pub name: String,
    pub sig: Signature,
}

impl ExternFunctionData {
    pub fn metadata(&self) -> FunctionMetadata<'_> {
        FunctionMetadata {
            name: &self.name,
            sig: &self.sig,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionMetadata<'a> {
    pub name: &'a str,
    pub sig: &'a Signature,
}

impl fmt::Display for FunctionMetadata<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_function_metadata(f, self)
    }
}

#[derive(Clone)]
pub struct Module {
    pub functions: PrimaryMap<Function, FunctionData>,
    pub extern_functions: PrimaryMap<ExternFunction, ExternFunctionData>,
}

impl Module {
    #[inline]
    pub fn new() -> Self {
        Self {
            functions: PrimaryMap::new(),
            extern_functions: PrimaryMap::new(),
        }
    }

    pub fn resolve_funcref(&self, funcref: FunctionRef) -> FunctionMetadata<'_> {
        match funcref {
            FunctionRef::Internal(func) => self.functions[func].metadata(),
            FunctionRef::External(func) => self.extern_functions[func].metadata(),
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

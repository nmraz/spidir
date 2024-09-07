use core::fmt;

use cranelift_entity::{entity_impl, PrimaryMap};

use crate::{
    function::{FunctionData, FunctionMetadata},
    node::FunctionRef,
    write::write_module,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function(u32);
entity_impl!(Function, "func");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExternFunction(u32);
entity_impl!(ExternFunction, "extfunc");

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

use alloc::string::String;
use core::fmt;

use cranelift_entity::{PrimaryMap, SecondaryMap, entity_impl};

use crate::{
    function::{FunctionBody, FunctionBorrow, FunctionData, FunctionMetadata, Signature},
    node::FunctionRef,
    write::write_module,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function(u32);
entity_impl!(Function, "func");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExternFunction(u32);
entity_impl!(ExternFunction, "extfunc");

#[derive(Clone, Default)]
pub struct ModuleMetadata {
    pub functions: PrimaryMap<Function, FunctionMetadata>,
    pub extern_functions: PrimaryMap<ExternFunction, FunctionMetadata>,
}

impl ModuleMetadata {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn resolve_funcref(&self, funcref: FunctionRef) -> &FunctionMetadata {
        match funcref {
            FunctionRef::Internal(func) => &self.functions[func],
            FunctionRef::External(func) => &self.extern_functions[func],
        }
    }
}

#[derive(Clone)]
pub struct Module {
    pub metadata: ModuleMetadata,
    pub functions: SecondaryMap<Function, FunctionData>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            metadata: ModuleMetadata::new(),
            functions: SecondaryMap::with_default(FunctionData::new_invalid()),
        }
    }

    pub fn create_function(&mut self, name: String, sig: Signature) -> Function {
        let body = FunctionBody::new(&sig.param_types);
        self.create_function_with_body(name, sig, body)
    }

    pub fn create_invalid_function(&mut self, name: String, sig: Signature) -> Function {
        self.create_function_with_body(name, sig, FunctionBody::new_invalid())
    }

    pub fn create_function_with_body(
        &mut self,
        name: String,
        sig: Signature,
        body: FunctionBody,
    ) -> Function {
        self.create_function_from_data_metadata(
            FunctionData::from_body(body),
            FunctionMetadata { name, sig },
        )
    }

    pub fn create_function_from_data_metadata(
        &mut self,
        data: FunctionData,
        metadata: FunctionMetadata,
    ) -> Function {
        let func = self.metadata.functions.push(metadata);
        self.functions[func] = data;
        func
    }

    pub fn borrow_function(&self, func: Function) -> FunctionBorrow<'_> {
        FunctionBorrow {
            data: &self.functions[func],
            metadata: &self.metadata.functions[func],
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

use alloc::string::String;
use core::fmt;

use cranelift_entity::{PrimaryMap, SecondaryMap, entity_impl};

use crate::{
    cache::NodeCache,
    function::{FunctionBody, FunctionBorrow, FunctionMetadata, Signature},
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
pub struct ModuleMetadata {
    functions: PrimaryMap<Function, FunctionMetadata>,
    extern_functions: PrimaryMap<ExternFunction, FunctionMetadata>,
}

impl ModuleMetadata {
    pub fn functions(&self) -> &PrimaryMap<Function, FunctionMetadata> {
        &self.functions
    }

    pub fn extern_functions(&self) -> &PrimaryMap<ExternFunction, FunctionMetadata> {
        &self.extern_functions
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
    pub function_bodies: SecondaryMap<Function, FunctionBody>,
    pub function_node_caches: SecondaryMap<Function, NodeCache>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            metadata: ModuleMetadata {
                functions: PrimaryMap::new(),
                extern_functions: PrimaryMap::new(),
            },
            function_bodies: SecondaryMap::with_default(FunctionBody::new_invalid()),
            function_node_caches: SecondaryMap::new(),
        }
    }

    pub fn create_extern_function(&mut self, name: String, sig: Signature) -> ExternFunction {
        self.metadata
            .extern_functions
            .push(FunctionMetadata { name, sig })
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
        self.create_function_from_metadata_body(FunctionMetadata { name, sig }, body)
    }

    pub fn create_function_from_metadata_body(
        &mut self,
        metadata: FunctionMetadata,
        body: FunctionBody,
    ) -> Function {
        let func = self.metadata.functions.push(metadata);
        self.function_bodies[func] = body;
        func
    }

    pub fn borrow_function(&self, func: Function) -> FunctionBorrow<'_> {
        FunctionBorrow {
            body: &self.function_bodies[func],
            metadata: &self.metadata.functions[func],
        }
    }

    pub fn iter_functions(&self) -> impl Iterator<Item = (Function, FunctionBorrow<'_>)> + '_ {
        self.metadata
            .functions
            .keys()
            .map(move |func| (func, self.borrow_function(func)))
    }

    pub fn iter_function_borrows(&self) -> impl Iterator<Item = FunctionBorrow<'_>> + '_ {
        self.iter_functions().map(|(_, func)| func)
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

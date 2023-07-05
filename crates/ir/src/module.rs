use alloc::{string::String, vec::Vec};
use core::fmt;

use cranelift_entity::{entity_impl, PrimaryMap};

use crate::{
    builder::NodeFactoryExt,
    node::{FunctionRef, Type},
    valgraph::{Node, ValGraph},
    write::write_module,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function(u32);
entity_impl!(Function, "func");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExternFunction(u32);
entity_impl!(ExternFunction, "extfunc");

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StackSlot(u32);
entity_impl!(StackSlot);

#[derive(Debug, Clone)]
pub struct Signature {
    pub ret_type: Option<Type>,
    pub param_types: Vec<Type>,
}

#[derive(Debug, Clone, Copy)]
pub struct StackSlotData {
    pub size: u32,
    pub align: u32,
}

pub type StackSlots = PrimaryMap<StackSlot, StackSlotData>;

#[derive(Clone)]
pub struct FunctionData {
    pub name: String,
    pub sig: Signature,
    pub stack_slots: StackSlots,
    pub graph: ValGraph,
    pub entry: Node,
}

impl FunctionData {
    pub fn new(name: String, sig: Signature) -> Self {
        let mut graph = ValGraph::new();
        let entry = graph.build_entry(&sig.param_types);
        Self {
            name,
            stack_slots: StackSlots::new(),
            sig,
            graph,
            entry: entry.node,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternFunctionData {
    pub name: String,
    pub sig: Signature,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionMetadata<'a> {
    pub name: &'a str,
    pub sig: &'a Signature,
}

#[derive(Clone)]
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

    pub fn resolve_funcref(&self, funcref: FunctionRef) -> FunctionMetadata<'_> {
        match funcref {
            FunctionRef::Internal(func) => {
                let func = &self.functions[func];
                FunctionMetadata {
                    name: &func.name,
                    sig: &func.sig,
                }
            }
            FunctionRef::External(func) => {
                let func = &self.extern_functions[func];
                FunctionMetadata {
                    name: &func.name,
                    sig: &func.sig,
                }
            }
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

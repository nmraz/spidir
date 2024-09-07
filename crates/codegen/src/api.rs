use alloc::vec::Vec;

use ir::{
    module::{FunctionData, Module},
    valgraph::{Node, ValGraph},
    valwalk::cfg_preorder,
};
use log::info;

use crate::{
    cfg::{CfgContext, FunctionBlockMap},
    emit::{emit_code, CodeBlob},
    isel::{select_instrs, IselError},
    lir::Lir,
    machine::{Machine, MachineLower},
    regalloc::{self, RegallocError},
    schedule::Schedule,
};

#[derive(Debug, Clone, Copy)]
pub enum CodegenError {
    Isel(IselError),
    Regalloc(RegallocError),
}

impl From<IselError> for CodegenError {
    fn from(v: IselError) -> Self {
        Self::Isel(v)
    }
}

impl From<RegallocError> for CodegenError {
    fn from(v: RegallocError) -> Self {
        Self::Regalloc(v)
    }
}

mod private {
    pub trait Sealed {}
}

pub trait Codegen: private::Sealed {
    fn codegen_func(&self, module: &Module, func: &FunctionData) -> Result<CodeBlob, CodegenError>;
}

impl<M: Machine> private::Sealed for M {}

impl<M: Machine> Codegen for M {
    fn codegen_func(&self, module: &Module, func: &FunctionData) -> Result<CodeBlob, CodegenError> {
        codegen_func(module, func, self)
    }
}

pub fn codegen_func<M: Machine>(
    module: &Module,
    func: &FunctionData,
    machine: &M,
) -> Result<CodeBlob, CodegenError> {
    info!("generating code for: {}", func.metadata());
    let (cfg_ctx, lir) = lower_func(module, func, machine)?;
    let assignment = regalloc::run(&lir, &cfg_ctx, machine)?;
    let code = emit_code(&lir, &cfg_ctx, &assignment, machine);
    Ok(code)
}

pub fn lower_func<M: MachineLower>(
    module: &Module,
    func: &FunctionData,
    machine: &M,
) -> Result<(CfgContext, Lir<M>), IselError> {
    let (cfg_ctx, block_map, schedule) = schedule_graph(&func.graph, func.entry);
    let lir = select_instrs(module, func, &schedule, &cfg_ctx, &block_map, machine)?;
    Ok((cfg_ctx, lir))
}

pub fn schedule_graph(graph: &ValGraph, entry: Node) -> (CfgContext, FunctionBlockMap, Schedule) {
    let cfg_preorder: Vec<_> = cfg_preorder(graph, entry).collect();
    let (cfg_ctx, block_map) = CfgContext::compute_for_valgraph(graph, &cfg_preorder);
    let schedule = Schedule::compute(graph, &cfg_preorder, &cfg_ctx, &block_map);
    (cfg_ctx, block_map, schedule)
}

#[cfg(test)]
mod tests {
    use crate::target::x64::X64Machine;

    use super::*;

    fn _codegen_object_safe(_codegen: &dyn Codegen) {}
    fn _real_machine_object_safe() {
        let machine = X64Machine;
        _codegen_object_safe(&machine);
    }
}
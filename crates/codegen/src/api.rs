use core::fmt;

use ir::{
    function::FunctionBorrow,
    module::ModuleMetadata,
    valgraph::{Node, ValGraph},
    valwalk::CfgPreorderInfo,
};
use log::{debug, info};

use crate::{
    cfg::{CfgContext, FunctionBlockMap},
    code_buffer::CodeBlob,
    emit::emit_code,
    isel::{IselError, select_instrs},
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

impl CodegenError {
    pub fn display<'a>(
        &'a self,
        module_metadata: &'a ModuleMetadata,
        func: FunctionBorrow<'a>,
    ) -> DisplayCodegenError<'a> {
        DisplayCodegenError {
            module_metadata,
            func,
            error: self,
        }
    }
}

pub struct DisplayCodegenError<'a> {
    module_metadata: &'a ModuleMetadata,
    func: FunctionBorrow<'a>,
    error: &'a CodegenError,
}

impl fmt::Display for DisplayCodegenError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "codegen for `{}` failed: ", self.func.metadata.name)?;
        match self.error {
            CodegenError::Isel(isel) => {
                write!(f, "{}", isel.display(self.module_metadata, self.func.body))
            }
            CodegenError::Regalloc(regalloc) => write!(f, "{regalloc}"),
        }
    }
}

#[derive(Default)]
pub struct CodegenOpts {
    pub verify_regalloc: bool,
}

mod private {
    pub trait Sealed {}
}

pub trait Codegen: private::Sealed {
    fn codegen_func(
        &self,
        module_metadata: &ModuleMetadata,
        func: FunctionBorrow<'_>,
        opts: &CodegenOpts,
    ) -> Result<CodeBlob, CodegenError>;
}

impl<M: Machine> private::Sealed for M {}

impl<M: Machine> Codegen for M {
    fn codegen_func(
        &self,
        module_metadata: &ModuleMetadata,
        func: FunctionBorrow<'_>,
        opts: &CodegenOpts,
    ) -> Result<CodeBlob, CodegenError> {
        codegen_func(module_metadata, func, self, opts)
    }
}

pub fn codegen_func<M: Machine>(
    module_metadata: &ModuleMetadata,
    func: FunctionBorrow<'_>,
    machine: &M,
    opts: &CodegenOpts,
) -> Result<CodeBlob, CodegenError> {
    info!("generating code for: {}", func.metadata);
    let (cfg_ctx, lir) = lower_func(module_metadata, func, machine)?;

    let assignment = regalloc::run(&lir, &cfg_ctx, machine)?;
    if opts.verify_regalloc {
        debug!("verifying regalloc for `{}`", func.metadata.name);
        if let Err(err) = regalloc::verify(&lir, &cfg_ctx, &assignment) {
            panic!(
                "register allocation for `{}` invalid: {}",
                func.metadata.name,
                err.display(&lir, &assignment)
            );
        }
    }

    let code = emit_code(&lir, &cfg_ctx, &assignment, machine);
    Ok(code)
}

pub fn lower_func<M: MachineLower>(
    module_metadata: &ModuleMetadata,
    func: FunctionBorrow<'_>,
    machine: &M,
) -> Result<(CfgContext, Lir<M>), IselError> {
    let (cfg_ctx, block_map, schedule) = schedule_graph(&func.body.graph, func.body.entry);
    let lir = select_instrs(
        module_metadata,
        func,
        &schedule,
        &cfg_ctx,
        &block_map,
        machine,
    )?;
    Ok((cfg_ctx, lir))
}

pub fn schedule_graph(graph: &ValGraph, entry: Node) -> (CfgContext, FunctionBlockMap, Schedule) {
    let cfg_preorder = CfgPreorderInfo::compute(graph, entry);
    let (cfg_ctx, block_map) = CfgContext::compute_for_valgraph(graph, &cfg_preorder.preorder);
    let schedule = Schedule::compute(graph, &cfg_preorder, &cfg_ctx, &block_map);
    (cfg_ctx, block_map, schedule)
}

#[cfg(test)]
mod tests {
    use crate::target::x64::X64Machine;

    use super::*;

    fn _codegen_object_safe(_codegen: &dyn Codegen) {}
    fn _real_machine_object_safe() {
        let machine = X64Machine::default();
        _codegen_object_safe(&machine);
    }
}

#![cfg_attr(not(test), no_std)]

use alloc::{boxed::Box, vec, vec::Vec};

use ir::module::Module;

use crate::state::{FunctionEditContext, ModuleState};

extern crate alloc;

mod canonicalize;
mod constfold;
mod sccp;
mod state;
mod utils;

pub use sccp::SccpPass;

pub trait ModulePass {
    fn run(&mut self, module: &mut Module, state: &mut ModuleState);
}

pub trait FunctionPass {
    fn run(&mut self, ctx: &mut FunctionEditContext<'_>);
}

pub struct FunctionPipeline {
    passes: Vec<Box<dyn FunctionPass>>,
}

impl FunctionPipeline {
    pub fn new(passes: Vec<Box<dyn FunctionPass>>) -> Self {
        Self { passes }
    }
}

impl ModulePass for FunctionPipeline {
    fn run(&mut self, module: &mut Module, state: &mut ModuleState) {
        for func in module.function_bodies.keys() {
            let mut ctx = state.edit_function(module, func);

            ctx.canonicalize_outstanding();
            for pass in &mut self.passes {
                pass.run(&mut ctx);
                ctx.canonicalize_outstanding();
            }
        }
    }
}

pub struct ModulePipeline {
    passes: Vec<Box<dyn ModulePass>>,
}

impl ModulePipeline {
    pub fn new(passes: Vec<Box<dyn ModulePass>>) -> Self {
        Self { passes }
    }

    pub fn run(&mut self, module: &mut Module) {
        let mut state = ModuleState::populate(module);
        for pass in &mut self.passes {
            pass.run(module, &mut state);
        }
    }
}

pub fn default_pipeline() -> ModulePipeline {
    ModulePipeline::new(vec![Box::new(FunctionPipeline::new(vec![Box::new(
        SccpPass,
    )]))])
}

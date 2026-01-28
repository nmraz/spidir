use alloc::{boxed::Box, vec::Vec};

use ir::module::Module;

use crate::state::{FunctionEditContext, ModuleState};

pub trait ModulePass {
    fn run(&self, module: &mut Module, state: &mut ModuleState);
}

pub trait FunctionPass {
    fn run(&self, ctx: &mut FunctionEditContext<'_>);
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
    fn run(&self, module: &mut Module, state: &mut ModuleState) {
        for func in module.function_bodies.keys() {
            let mut ctx = state.edit_function(module, func);

            ctx.canonicalize_outstanding();
            for pass in &self.passes {
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

    pub fn run(&self, module: &mut Module) {
        let mut state = ModuleState::populate(module);
        for pass in &self.passes {
            pass.run(module, &mut state);
        }
    }
}

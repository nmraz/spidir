#![cfg_attr(not(test), no_std)]

use alloc::{boxed::Box, vec};

extern crate alloc;

mod canonicalize;
mod constfold;
mod pass;
mod sccp;
mod state;
mod utils;

#[cfg(feature = "parse-pipeline-desc")]
mod parse_pipeline;

#[cfg(feature = "parse-pipeline-desc")]
pub use parse_pipeline::pipeline_from_desc;
pub use pass::{FunctionPass, FunctionPipeline, ModulePass, ModulePipeline};
pub use sccp::SccpPass;

pub fn default_pipeline() -> ModulePipeline {
    ModulePipeline::new(vec![Box::new(FunctionPipeline::new(vec![Box::new(
        SccpPass,
    )]))])
}

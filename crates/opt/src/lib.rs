#![cfg_attr(not(test), no_std)]

use ir::module::Module;

use crate::state::{EditContext, FunctionState};

extern crate alloc;

mod canonicalize;
mod constfold;
mod state;
mod utils;

pub fn run(module: &mut Module) {
    // For now: just run the canonicalization pass on everything.
    canonicalize(module);
}

pub fn canonicalize(module: &mut Module) {
    for (func, body) in module.function_bodies.iter_mut() {
        let mut state = FunctionState::populate(
            &module.metadata,
            body,
            &mut module.function_node_caches[func],
        );
        let mut ctx = EditContext::new(
            &module.metadata,
            body,
            &mut module.function_node_caches[func],
            &mut state,
        );
        ctx.canonicalize_outstanding();
    }
}

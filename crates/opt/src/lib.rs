#![cfg_attr(not(test), no_std)]

use ir::module::Module;

use crate::{
    sccp::do_sccp,
    state::{EditContext, FunctionState},
};

extern crate alloc;

mod canonicalize;
mod constfold;
mod sccp;
mod state;
mod utils;

pub fn run(module: &mut Module) {
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

        // Canonicalize everything first to reduce work.
        ctx.canonicalize_outstanding();

        do_sccp(&mut ctx);
        ctx.canonicalize_outstanding();
    }
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

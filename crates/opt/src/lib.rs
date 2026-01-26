#![cfg_attr(not(test), no_std)]

use ir::module::Module;

use crate::{sccp::do_sccp, state::ModuleState};

extern crate alloc;

mod canonicalize;
mod constfold;
mod sccp;
mod state;
mod utils;

pub fn run(module: &mut Module) {
    let mut state = ModuleState::populate(module);

    for func in module.function_bodies.keys() {
        let mut ctx = state.edit_function(module, func);

        // Canonicalize everything first to reduce work.
        ctx.canonicalize_outstanding();

        do_sccp(&mut ctx);
        ctx.canonicalize_outstanding();
    }
}

pub fn canonicalize(module: &mut Module) {
    let mut state = ModuleState::populate(module);
    for func in module.function_bodies.keys() {
        state.edit_function(module, func).canonicalize_outstanding();
    }
}

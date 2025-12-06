#![cfg_attr(not(test), no_std)]

use ir::module::Module;

extern crate alloc;

mod canonicalize;
mod reduce;

pub fn run(module: &mut Module) {
    // For now: just run the canonicalization pass on everything.
    canonicalize(module);
}

pub fn canonicalize(module: &mut Module) {
    for (func, body) in module.function_bodies.iter_mut() {
        canonicalize::canonicalize(
            &module.metadata,
            body,
            &mut module.function_node_caches[func],
        );
    }
}

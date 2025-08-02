use ir::module::Module;

#[unsafe(no_mangle)]
unsafe extern "C" fn spidir_opt_run(module: *mut Module) {
    let module = unsafe { &mut *module };

    for (func, body) in module.function_bodies.iter_mut() {
        opt::canonicalize::canonicalize(
            &module.metadata,
            body,
            &mut module.function_node_caches[func],
        );
    }
}

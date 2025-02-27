use ir::module::Module;

#[unsafe(no_mangle)]
unsafe extern "C" fn spidir_opt_run(module: *mut Module) {
    let module = unsafe { &mut *module };

    for func in module.functions.values_mut() {
        opt::canonicalize::canonicalize(&mut func.body, &mut func.node_cache);
    }
}

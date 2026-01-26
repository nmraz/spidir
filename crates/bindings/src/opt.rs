use ir::module::Module;

#[unsafe(no_mangle)]
unsafe extern "C" fn spidir_opt_run(module: *mut Module) {
    let module = unsafe { &mut *module };
    opt::default_pipeline().run(module);
}

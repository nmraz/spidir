use core::{
    ffi::c_char,
    fmt::{self, Write},
};

use alloc::boxed::Box;
use frontend::FunctionBuilder;
use ir::{
    function::{FunctionData, FunctionMetadata},
    module::Module,
    node::FunctionRef,
};

use crate::types::{
    funcref_from_api, funcref_to_api, name_signature_from_api, ApiFunction, ApiType,
    BuildFunctionCallback, DumpCallback, SPIDIR_DUMP_CONTINUE,
};

#[no_mangle]
extern "C" fn spidir_module_create() -> *mut Module {
    let module = Box::new(Module::new());
    Box::into_raw(module)
}

#[no_mangle]
unsafe extern "C" fn spidir_module_destroy(module: *mut Module) {
    unsafe {
        drop(Box::from_raw(module));
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_module_create_function(
    module: *mut Module,
    name: *const c_char,
    ret_type: ApiType,
    param_count: usize,
    param_types: *const ApiType,
) -> ApiFunction {
    unsafe {
        let module = &mut *module;

        let (name, sig) = name_signature_from_api(name, ret_type, param_count, param_types);
        let func = module.functions.push(FunctionData::new(name, sig));
        funcref_to_api(FunctionRef::Internal(func))
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_module_create_extern_function(
    module: *mut Module,
    name: *const c_char,
    ret_type: ApiType,
    param_count: usize,
    param_types: *const ApiType,
) -> ApiFunction {
    unsafe {
        let module = &mut *module;

        let (name, sig) = name_signature_from_api(name, ret_type, param_count, param_types);
        let func = module.extern_functions.push(FunctionMetadata { name, sig });
        funcref_to_api(FunctionRef::External(func))
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_module_build_function(
    module: *mut Module,
    func: ApiFunction,
    callback: BuildFunctionCallback,
    ctx: *mut (),
) {
    let FunctionRef::Internal(func) = funcref_from_api(func) else {
        panic!("external function passed to `spidir_module_build_function`");
    };

    unsafe {
        let module = &mut *module;
        let mut builder = FunctionBuilder::new(module, func);
        callback(&mut builder, ctx);
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_module_dump(
    module: *const Module,
    callback: DumpCallback,
    ctx: *mut (),
) {
    unsafe {
        let _ = write!(DumpWriteAdapter { callback, ctx }, "{}", &*module);
    }
}

struct DumpWriteAdapter {
    callback: DumpCallback,
    ctx: *mut (),
}

impl Write for DumpWriteAdapter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let res = (self.callback)(s.as_ptr() as *const c_char, s.len(), self.ctx);

        if res == SPIDIR_DUMP_CONTINUE {
            Ok(())
        } else {
            Err(fmt::Error)
        }
    }
}

#![no_std]

extern crate alloc;

use core::{
    ffi::{c_char, CStr},
    fmt::{self, Write},
    slice, str,
};

use alloc::{borrow::ToOwned, boxed::Box};
use frontend::FunctionBuilder;
use ir::{
    module::{ExternFunctionData, Function, FunctionData, Module, Signature},
    node::Type,
};

mod builder;

#[cfg(not(test))]
mod handlers;

#[repr(C)]
struct ApiFunction(u32);
#[repr(C)]
struct ApiExternFunction(u32);
#[repr(C)]
struct ApiBlock(u32);
#[repr(C)]
struct ApiStackSlot(u32);
#[repr(C)]
struct ApiValue(u32);
#[repr(C)]
struct ApiPhi(u32);

type BuildFunctionCallback = extern "C" fn(*mut FunctionBuilder, *mut ());
type DumpCallback = extern "C" fn(*const c_char, usize, *mut ()) -> u8;

const SPIDIR_TYPE_I32: u8 = 0;
const SPIDIR_TYPE_I64: u8 = 1;
const SPIDIR_TYPE_F64: u8 = 2;
const SPIDIR_TYPE_PTR: u8 = 3;

const SPIDIR_ICMP_EQ: u8 = 0;
const SPIDIR_ICMP_NE: u8 = 0;
const SPIDIR_ICMP_SLT: u8 = 0;
const SPIDIR_ICMP_SLE: u8 = 0;
const SPIDIR_ICMP_ULT: u8 = 0;
const SPIDIR_ICMP_ULE: u8 = 0;

const SPIDIR_DUMP_CONTINUE: u8 = 0;

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
    ret_type: *const u8,
    param_count: usize,
    param_types: *const u8,
) -> ApiFunction {
    unsafe {
        let module = &mut *module;

        let (name, sig) = get_name_signature(name, ret_type, param_count, param_types);

        let func = module
            .functions
            .push(FunctionData::new(name.to_owned(), sig));

        ApiFunction(func.as_u32())
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_module_create_extern_function(
    module: *mut Module,
    name: *const c_char,
    ret_type: *const u8,
    param_count: usize,
    param_types: *const u8,
) -> ApiExternFunction {
    unsafe {
        let module = &mut *module;

        let (name, sig) = get_name_signature(name, ret_type, param_count, param_types);

        let func = module.extern_functions.push(ExternFunctionData {
            name: name.to_owned(),
            sig,
        });

        ApiExternFunction(func.as_u32())
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_module_build_function(
    module: *mut Module,
    func: ApiFunction,
    callback: BuildFunctionCallback,
    ctx: *mut (),
) {
    unsafe {
        let module = &mut *module;
        let mut builder = FunctionBuilder::new(&mut module.functions[func_from_api(func)]);
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

unsafe fn get_name_signature(
    name: *const c_char,
    ret_type: *const u8,
    param_count: usize,
    param_types: *const u8,
) -> (&'static str, Signature) {
    unsafe {
        let name = CStr::from_ptr(name);
        let params = slice::from_raw_parts(param_types, param_count);

        let ret_type = if ret_type.is_null() {
            None
        } else {
            Some(type_from_api(*ret_type))
        };

        let sig = Signature {
            ret_type,
            param_types: params.iter().map(|&ty| type_from_api(ty)).collect(),
        };

        (name.to_str().expect("function name not utf-8"), sig)
    }
}

fn func_from_api(func: ApiFunction) -> Function {
    Function::from_u32(func.0)
}

fn type_from_api(api_type: u8) -> Type {
    match api_type {
        SPIDIR_TYPE_I32 => Type::I32,
        SPIDIR_TYPE_I64 => Type::I64,
        SPIDIR_TYPE_F64 => Type::F64,
        SPIDIR_TYPE_PTR => Type::Ptr,
        _ => panic!("unexpected type {api_type}"),
    }
}

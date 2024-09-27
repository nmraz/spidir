use core::ptr;

use alloc::{boxed::Box, vec::Vec};
use codegen::{
    api::{codegen_func, CodegenError, CodegenOpts},
    code_buffer::CodeBlob,
    machine::Machine,
};
use ir::{
    function::FunctionData, module::Module, node::FunctionRef, verify::verify_func,
    write::display_node,
};
use log::error;

use crate::types::{
    funcref_from_api, reloc_to_api, ApiCodegenConfig, ApiCodegenStatus, ApiFunction, ApiReloc,
    SPIDIR_CODEGEN_ERROR_ISEL, SPIDIR_CODEGEN_ERROR_REGALLOC, SPIDIR_CODEGEN_OK,
};

struct ApiCodegenMachineVtable {
    drop: unsafe fn(*mut ApiCodegenMachine),
    codegen_func: unsafe fn(
        *const ApiCodegenMachine,
        module: &Module,
        func: &FunctionData,
        opts: &CodegenOpts,
    ) -> Result<CodeBlob, CodegenError>,
}

pub struct ApiCodegenMachine {
    vtable: &'static ApiCodegenMachineVtable,
}

#[repr(C)]
struct ApiCodegenMachineInner<M> {
    base: ApiCodegenMachine,
    machine: M,
}

struct ApiCodegenBlob {
    code: Vec<u8>,
    relocs: Vec<ApiReloc>,
}

unsafe fn codegen_machine_drop<M>(codegen_machine: *mut ApiCodegenMachine) {
    unsafe {
        drop(Box::from_raw(
            codegen_machine as *mut ApiCodegenMachineInner<M>,
        ));
    }
}

unsafe fn codegen_machine_codegen_func<M: Machine>(
    codegen_machine: *const ApiCodegenMachine,
    module: &Module,
    func: &FunctionData,
    opts: &CodegenOpts,
) -> Result<CodeBlob, CodegenError> {
    let inner = unsafe { &*(codegen_machine as *const ApiCodegenMachineInner<M>) };
    codegen_func(module, func, &inner.machine, opts)
}

pub fn codegen_machine_to_api<M: Machine>(machine: M) -> *mut ApiCodegenMachine {
    Box::into_raw(Box::new(ApiCodegenMachineInner {
        base: ApiCodegenMachine {
            vtable: &ApiCodegenMachineVtable {
                drop: codegen_machine_drop::<M>,
                codegen_func: codegen_machine_codegen_func::<M>,
            },
        },
        machine,
    })) as *mut ApiCodegenMachine
}

fn codegen_blob_to_api(blob: CodeBlob) -> *mut ApiCodegenBlob {
    let code = blob.code;
    let relocs = blob.relocs.iter().map(reloc_to_api).collect();
    Box::into_raw(Box::new(ApiCodegenBlob { code, relocs }))
}

#[no_mangle]
unsafe extern "C" fn spidir_codegen_machine_destroy(machine: *mut ApiCodegenMachine) {
    unsafe {
        let dropper = (*machine).vtable.drop;
        dropper(machine);
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_codegen_blob_destroy(blob: *mut ApiCodegenBlob) {
    unsafe {
        drop(Box::from_raw(blob));
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_codegen_blob_get_code_size(blob: *const ApiCodegenBlob) -> usize {
    let blob = unsafe { &*blob };
    blob.code.len()
}

#[no_mangle]
unsafe extern "C" fn spidir_codegen_blob_get_code(blob: *const ApiCodegenBlob) -> *const u8 {
    let blob = unsafe { &*blob };
    blob.code.as_ptr()
}

#[no_mangle]
unsafe extern "C" fn spidir_codegen_blob_get_reloc_count(blob: *const ApiCodegenBlob) -> usize {
    let blob = unsafe { &*blob };
    blob.relocs.len()
}

#[no_mangle]
unsafe extern "C" fn spidir_codegen_blob_get_relocs(
    blob: *const ApiCodegenBlob,
) -> *const ApiReloc {
    let blob = unsafe { &*blob };
    blob.relocs.as_ptr()
}

#[no_mangle]
unsafe extern "C" fn spidir_codegen_emit_function(
    machine: *mut ApiCodegenMachine,
    config: *const ApiCodegenConfig,
    module: *const Module,
    func: ApiFunction,
    out_blob: *mut *mut ApiCodegenBlob,
) -> ApiCodegenStatus {
    let FunctionRef::Internal(func) = funcref_from_api(func) else {
        panic!("external function passed to `spidir_codegen_emit_function`");
    };

    let module = unsafe { &*module };
    let config = unsafe { &*config };

    let func = &module.functions[func];

    if config.verify_ir {
        verify_ir_function(module, func);
    }

    let codegen_opts = CodegenOpts {
        verify_regalloc: config.verify_regalloc,
    };

    let res = unsafe {
        let codegen_func = (*machine).vtable.codegen_func;
        codegen_func(machine, module, func, &codegen_opts)
    };

    let blob = match res {
        Ok(blob) => blob,
        Err(err) => {
            error!("{}", err.display(module, func));

            let err = match err {
                CodegenError::Isel(_) => SPIDIR_CODEGEN_ERROR_ISEL,
                CodegenError::Regalloc(_) => SPIDIR_CODEGEN_ERROR_REGALLOC,
            };

            unsafe {
                *out_blob = ptr::null_mut();
            }

            return err;
        }
    };

    unsafe {
        *out_blob = codegen_blob_to_api(blob);
    }

    SPIDIR_CODEGEN_OK
}

fn verify_ir_function(module: &Module, func: &FunctionData) {
    if let Err(errs) = verify_func(module, func) {
        error!("codegen verification of `{}` failed:", func.metadata.name);
        for err in errs {
            error!(
                "    `{}`: {}",
                display_node(module, &func.body, err.node(&func.body.graph)),
                err.display(&func.body.graph)
            );
        }
        panic!("`{}` contained invalid IR", func.metadata.name);
    }
}

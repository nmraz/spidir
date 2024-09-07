use core::ptr;

use alloc::{boxed::Box, vec::Vec};
use codegen::{
    api::{codegen_func, CodegenError},
    emit::CodeBlob,
    machine::Machine,
    regalloc::RegallocError,
    target::x64::X64Machine,
};
use ir::{function::FunctionData, module::Module, node::FunctionRef, write::display_node};
use log::error;

use crate::types::{
    funcref_from_api, reloc_to_api, ApiCodegenStatus, ApiFunction, ApiReloc,
    SPIDIR_CODEGEN_ERROR_ISEL, SPIDIR_CODEGEN_ERROR_REGALLOC, SPIDIR_CODEGEN_OK,
};

struct ApiCodegenMachineVtable {
    drop: unsafe fn(*mut ApiCodegenMachine),
    codegen_func: unsafe fn(
        *const ApiCodegenMachine,
        module: &Module,
        func: &FunctionData,
    ) -> Result<CodeBlob, CodegenError>,
}

struct ApiCodegenMachine {
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
) -> Result<CodeBlob, CodegenError> {
    let inner = unsafe { &*(codegen_machine as *const ApiCodegenMachineInner<M>) };
    codegen_func(module, func, &inner.machine)
}

fn codegen_machine_to_api<M: Machine>(machine: M) -> *mut ApiCodegenMachine {
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
    module: *const Module,
    func: ApiFunction,
    out_blob: *mut *mut ApiCodegenBlob,
) -> ApiCodegenStatus {
    let FunctionRef::Internal(func) = funcref_from_api(func) else {
        panic!("external function passed to `spidir_codegen_emit_function`");
    };

    let module = unsafe { &*module };
    let func = &module.functions[func];

    let res = unsafe {
        let codegen_func = (*machine).vtable.codegen_func;
        codegen_func(machine, module, func)
    };

    let blob = match res {
        Ok(blob) => blob,
        Err(err) => {
            let err = match err {
                CodegenError::Isel(err) => {
                    error!(
                        "failed to select `{}`: `{}`",
                        func.metadata.name,
                        display_node(module, &func.body, err.node)
                    );
                    SPIDIR_CODEGEN_ERROR_ISEL
                }
                CodegenError::Regalloc(RegallocError::OutOfRegisters(instr)) => {
                    error!("out of registers at instruction {instr}");
                    SPIDIR_CODEGEN_ERROR_REGALLOC
                }
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

#[no_mangle]
extern "C" fn spidir_codegen_create_x64_machine() -> *mut ApiCodegenMachine {
    codegen_machine_to_api(X64Machine)
}

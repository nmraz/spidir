use codegen::target::x64::{CodeModel, X64Machine, X64MachineConfig};

use crate::codegen::{codegen_machine_to_api, ApiCodegenMachine};

type ApiX64CodeModel = u8;

const SPIDIR_X64_CM_SMALL_PIC: u8 = 0;
const SPIDIR_X64_CM_LARGE_ABS: u8 = 1;

#[repr(C)]
struct ApiX64MachineConfig {
    internal_code_model: ApiX64CodeModel,
    extern_code_model: ApiX64CodeModel,
}

fn x64_code_model_from_api(code_model: ApiX64CodeModel) -> CodeModel {
    match code_model {
        SPIDIR_X64_CM_SMALL_PIC => CodeModel::SmallPic,
        SPIDIR_X64_CM_LARGE_ABS => CodeModel::LargeAbs,
        _ => panic!("unsupported x64 code model"),
    }
}

fn x64_machine_config_from_api(machine_config: &ApiX64MachineConfig) -> X64MachineConfig {
    X64MachineConfig {
        internal_code_model: x64_code_model_from_api(machine_config.internal_code_model),
        extern_code_model: x64_code_model_from_api(machine_config.extern_code_model),
    }
}

#[no_mangle]
extern "C" fn spidir_codegen_create_x64_machine() -> *mut ApiCodegenMachine {
    codegen_machine_to_api(X64Machine::default())
}

#[no_mangle]
unsafe extern "C" fn spidir_codegen_create_x64_machine_with_config(
    config: *const ApiX64MachineConfig,
) -> *mut ApiCodegenMachine {
    let config = unsafe { x64_machine_config_from_api(&*config) };
    codegen_machine_to_api(X64Machine::new(config))
}

use core::{
    ffi::{CStr, c_char},
    slice,
};

use alloc::{borrow::ToOwned, string::String};
use codegen::code_buffer::{Reloc, RelocTarget};
use frontend::{Block, FunctionBuilder};
use ir::{
    function::Signature,
    module::{ExternFunction, Function},
    node::{FcmpKind, FunctionRef, IcmpKind, MemSize, Type},
    valgraph::DepValue,
};
use smallvec::SmallVec;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ApiFunction(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ApiExternFunction(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ApiFunctionRef(pub u64);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ApiBlock(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ApiValue(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ApiPhi(pub u32);

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct ApiReloc {
    pub target: ApiFunctionRef,
    pub addend: i64,
    pub offset: u32,
    pub kind: u8,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct ApiCodegenConfig {
    pub verify_ir: bool,
    pub verify_regalloc: bool,
}

pub type ApiType = u8;
pub type ApiIcmpKind = u8;
pub type ApiFcmpKind = u8;
pub type ApiMemSize = u8;
pub type ApiCodegenStatus = u32;

pub type BuildFunctionCallback = extern "C" fn(*mut FunctionBuilder, *mut ());
pub type DumpCallback = extern "C" fn(*const c_char, usize, *mut ()) -> u8;

pub const SPIDIR_DUMP_CONTINUE: u8 = 0;

pub const SPIDIR_CODEGEN_OK: u32 = 0;
pub const SPIDIR_CODEGEN_ERROR_ISEL: u32 = 1;
pub const SPIDIR_CODEGEN_ERROR_REGALLOC: u32 = 2;

const SPIDIR_VALUE_INVALID: ApiValue = ApiValue(u32::MAX);

const SPIDIR_FUNCREF_INTERNAL: u64 = 0;
const SPIDIR_FUNCREF_EXTERNAL: u64 = 1;

const SPIDIR_TYPE_I32: u8 = 0;
const SPIDIR_TYPE_I64: u8 = 1;
const SPIDIR_TYPE_F64: u8 = 2;
const SPIDIR_TYPE_PTR: u8 = 3;
const SPIDIR_TYPE_NONE: u8 = u8::MAX;

const SPIDIR_ICMP_EQ: u8 = 0;
const SPIDIR_ICMP_NE: u8 = 1;
const SPIDIR_ICMP_SLT: u8 = 2;
const SPIDIR_ICMP_SLE: u8 = 3;
const SPIDIR_ICMP_ULT: u8 = 4;
const SPIDIR_ICMP_ULE: u8 = 5;

const SPIDIR_FCMP_OEQ: u8 = 0;
const SPIDIR_FCMP_ONE: u8 = 1;
const SPIDIR_FCMP_OLT: u8 = 2;
const SPIDIR_FCMP_OLE: u8 = 3;
const SPIDIR_FCMP_UEQ: u8 = 4;
const SPIDIR_FCMP_UNE: u8 = 5;
const SPIDIR_FCMP_ULT: u8 = 6;
const SPIDIR_FCMP_ULE: u8 = 7;

const SPIDIR_MEM_SIZE_1: u8 = 0;
const SPIDIR_MEM_SIZE_2: u8 = 1;
const SPIDIR_MEM_SIZE_4: u8 = 2;
const SPIDIR_MEM_SIZE_8: u8 = 3;

pub unsafe fn value_list_from_api(
    arg_count: usize,
    args: *const ApiValue,
) -> SmallVec<[DepValue; 4]> {
    unsafe {
        let args = slice_from_api(arg_count, args);
        args.iter().map(|&arg| value_from_api(arg)).collect()
    }
}

#[track_caller]
pub unsafe fn name_signature_from_api(
    name: *const c_char,
    ret_type: ApiType,
    param_count: usize,
    param_types: *const ApiType,
) -> (String, Signature) {
    unsafe {
        (
            CStr::from_ptr(name)
                .to_str()
                .expect("function name not utf-8")
                .to_owned(),
            signature_from_api(ret_type, param_count, param_types),
        )
    }
}

#[track_caller]
pub unsafe fn signature_from_api(
    ret_type: ApiType,
    param_count: usize,
    param_types: *const ApiType,
) -> Signature {
    unsafe {
        let params = slice_from_api(param_count, param_types);

        let ret_type = opt_type_from_api(ret_type);

        Signature {
            ret_type,
            param_types: params.iter().map(|&ty| type_from_api(ty)).collect(),
        }
    }
}

pub fn opt_value_to_api(value: Option<DepValue>) -> ApiValue {
    value.map_or(SPIDIR_VALUE_INVALID, value_to_api)
}

#[track_caller]
pub fn opt_value_from_api(value: ApiValue) -> Option<DepValue> {
    if value == SPIDIR_VALUE_INVALID {
        None
    } else {
        Some(value_from_api(value))
    }
}

#[track_caller]
pub fn opt_type_from_api(opt_type: ApiType) -> Option<Type> {
    if opt_type == SPIDIR_TYPE_NONE {
        None
    } else {
        Some(type_from_api(opt_type))
    }
}

pub fn function_from_api(func: ApiFunction) -> Function {
    Function::from_u32(func.0)
}

pub fn function_to_api(func: Function) -> ApiFunction {
    ApiFunction(func.as_u32())
}

pub fn extern_function_to_api(func: ExternFunction) -> ApiExternFunction {
    ApiExternFunction(func.as_u32())
}

pub fn funcref_from_api(func: ApiFunctionRef) -> FunctionRef {
    let val = func.0 as u32;
    let kind = func.0 >> 32;

    match kind {
        SPIDIR_FUNCREF_INTERNAL => FunctionRef::Internal(Function::from_u32(val)),
        SPIDIR_FUNCREF_EXTERNAL => FunctionRef::External(ExternFunction::from_u32(val)),
        _ => panic!("corrupt function reference"),
    }
}

pub fn funcref_to_api(func: FunctionRef) -> ApiFunctionRef {
    let val = match func {
        FunctionRef::Internal(func) => (SPIDIR_FUNCREF_INTERNAL << 32) | (func.as_u32() as u64),
        FunctionRef::External(func) => (SPIDIR_FUNCREF_EXTERNAL << 32) | (func.as_u32() as u64),
    };
    ApiFunctionRef(val)
}

pub fn value_to_api(value: DepValue) -> ApiValue {
    ApiValue(value.as_u32())
}

#[track_caller]
pub fn value_from_api(value: ApiValue) -> DepValue {
    assert!(
        value != SPIDIR_VALUE_INVALID,
        "value parameter is required here"
    );
    DepValue::from_u32(value.0)
}

pub fn block_to_api(block: Block) -> ApiBlock {
    ApiBlock(block.as_u32())
}

pub fn block_from_api(block: ApiBlock) -> Block {
    Block::from_u32(block.0)
}

#[track_caller]
pub fn type_from_api(api_type: ApiType) -> Type {
    match api_type {
        SPIDIR_TYPE_I32 => Type::I32,
        SPIDIR_TYPE_I64 => Type::I64,
        SPIDIR_TYPE_F64 => Type::F64,
        SPIDIR_TYPE_PTR => Type::Ptr,
        SPIDIR_TYPE_NONE => panic!("type parameter is required here"),
        _ => panic!("unexpected type {api_type}"),
    }
}

#[track_caller]
pub fn icmp_kind_from_api(kind: ApiIcmpKind) -> IcmpKind {
    match kind {
        SPIDIR_ICMP_EQ => IcmpKind::Eq,
        SPIDIR_ICMP_NE => IcmpKind::Ne,
        SPIDIR_ICMP_SLT => IcmpKind::Slt,
        SPIDIR_ICMP_SLE => IcmpKind::Sle,
        SPIDIR_ICMP_ULT => IcmpKind::Ult,
        SPIDIR_ICMP_ULE => IcmpKind::Ule,
        _ => panic!("unexpected icmp kind {kind}"),
    }
}

#[track_caller]
pub fn fcmp_kind_from_api(kind: ApiFcmpKind) -> FcmpKind {
    match kind {
        SPIDIR_FCMP_OEQ => FcmpKind::Oeq,
        SPIDIR_FCMP_ONE => FcmpKind::One,
        SPIDIR_FCMP_OLT => FcmpKind::Olt,
        SPIDIR_FCMP_OLE => FcmpKind::Ole,
        SPIDIR_FCMP_UEQ => FcmpKind::Ueq,
        SPIDIR_FCMP_UNE => FcmpKind::Une,
        SPIDIR_FCMP_ULT => FcmpKind::Ult,
        SPIDIR_FCMP_ULE => FcmpKind::Ule,
        _ => panic!("unexpected fcmp kind {kind}"),
    }
}

#[track_caller]
pub fn mem_size_from_api(api_size: ApiMemSize) -> MemSize {
    match api_size {
        SPIDIR_MEM_SIZE_1 => MemSize::S1,
        SPIDIR_MEM_SIZE_2 => MemSize::S2,
        SPIDIR_MEM_SIZE_4 => MemSize::S4,
        SPIDIR_MEM_SIZE_8 => MemSize::S8,
        _ => panic!("unexpected memory size {api_size}"),
    }
}

pub fn reloc_to_api(reloc: &Reloc) -> ApiReloc {
    let target = match reloc.target {
        RelocTarget::Function(func) => func,
        RelocTarget::ConstantPool => todo!("support constant pool in C API"),
    };
    ApiReloc {
        target: funcref_to_api(target),
        addend: reloc.addend,
        offset: reloc.offset,
        kind: reloc.kind.0,
    }
}

unsafe fn slice_from_api<'a, T: 'a>(len: usize, ptr: *const T) -> &'a [T] {
    if len == 0 {
        // Accept a null pointer if `len` is 0
        &[]
    } else {
        unsafe { slice::from_raw_parts(ptr, len) }
    }
}

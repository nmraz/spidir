use core::{
    ffi::{c_char, CStr},
    slice,
};

use alloc::{borrow::ToOwned, string::String};
use codegen::emit::Reloc;
use frontend::{Block, FunctionBuilder};
use ir::{
    module::{ExternFunction, Function, Signature},
    node::{FunctionRef, IcmpKind, MemSize, Type},
    valgraph::DepValue,
};
use smallvec::SmallVec;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ApiFunction(pub u64);
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
    pub target: ApiFunction,
    pub addend: i64,
    pub offset: u32,
    pub kind: u8,
}

pub type ApiType = u8;
pub type ApiIcmpKind = u8;
pub type ApiMemSize = u8;
pub type ApiCodegenStatus = u32;

pub type BuildFunctionCallback = extern "C" fn(*mut FunctionBuilder, *mut ());
pub type DumpCallback = extern "C" fn(*const c_char, usize, *mut ()) -> u8;

pub const SPIDIR_DUMP_CONTINUE: u8 = 0;

pub const SPIDIR_CODEGEN_OK: u32 = 0;
pub const SPIDIR_CODEGEN_ERROR_ISEL: u32 = 1;
pub const SPIDIR_CODEGEN_ERROR_REGALLOC: u32 = 2;

const SPIDIR_VALUE_INVALID: ApiValue = ApiValue(u32::MAX);
const EXTERN_FUNCTION_BIT: u64 = 1 << 63;

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
        let name = CStr::from_ptr(name);
        let params = slice_from_api(param_count, param_types);

        let ret_type = opt_type_from_api(ret_type);

        let sig = Signature {
            ret_type,
            param_types: params.iter().map(|&ty| type_from_api(ty)).collect(),
        };

        (
            name.to_str().expect("function name not utf-8").to_owned(),
            sig,
        )
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

pub fn funcref_from_api(func: ApiFunction) -> FunctionRef {
    let val = func.0;
    if val & EXTERN_FUNCTION_BIT != 0 {
        FunctionRef::External(ExternFunction::from_u32(val as u32))
    } else {
        FunctionRef::Internal(Function::from_u32(val as u32))
    }
}

pub fn funcref_to_api(func: FunctionRef) -> ApiFunction {
    let val = match func {
        FunctionRef::Internal(func) => func.as_u32() as u64,
        FunctionRef::External(func) => (func.as_u32() as u64) | EXTERN_FUNCTION_BIT,
    };
    ApiFunction(val)
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
    ApiReloc {
        target: funcref_to_api(reloc.target),
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

use core::{
    ffi::{c_char, CStr},
    slice,
};

use alloc::{borrow::ToOwned, string::String};
use frontend::{Block, FunctionBuilder};
use ir::{
    module::{Function, Signature},
    node::{IcmpKind, Type},
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
pub struct ApiBlock(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ApiValue(pub u32);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ApiPhi(pub u32);

pub type ApiType = u8;
pub type ApiIcmpKind = u8;

pub type BuildFunctionCallback = extern "C" fn(*mut FunctionBuilder, *mut ());
pub type DumpCallback = extern "C" fn(*const c_char, usize, *mut ()) -> u8;

const SPIDIR_VALUE_INVALID: ApiValue = ApiValue(u32::MAX);

pub const SPIDIR_TYPE_I32: u8 = 0;
pub const SPIDIR_TYPE_I64: u8 = 1;
pub const SPIDIR_TYPE_F64: u8 = 2;
pub const SPIDIR_TYPE_PTR: u8 = 3;
pub const SPIDIR_TYPE_NONE: u8 = u8::MAX;

pub const SPIDIR_ICMP_EQ: u8 = 0;
pub const SPIDIR_ICMP_NE: u8 = 1;
pub const SPIDIR_ICMP_SLT: u8 = 2;
pub const SPIDIR_ICMP_SLE: u8 = 3;
pub const SPIDIR_ICMP_ULT: u8 = 4;
pub const SPIDIR_ICMP_ULE: u8 = 5;

pub const SPIDIR_DUMP_CONTINUE: u8 = 0;

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

pub fn func_from_api(func: ApiFunction) -> Function {
    Function::from_u32(func.0)
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

unsafe fn slice_from_api<'a, T: 'a>(len: usize, ptr: *const T) -> &'a [T] {
    if len == 0 {
        // Accept a null pointer if `len` is 0
        &[]
    } else {
        unsafe { slice::from_raw_parts(ptr, len) }
    }
}

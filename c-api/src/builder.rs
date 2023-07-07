use core::slice;

use frontend::{Block, FunctionBuilder, PhiHandle};
use ir::{
    module::{ExternFunction, Function},
    node::{FunctionRef, IcmpKind, Type},
    valgraph::DepValue,
};
use paste::paste;
use smallvec::SmallVec;

use crate::{
    opt_type_from_api, type_from_api, ApiBlock, ApiExternFunction, ApiFunction, ApiIcmpKind,
    ApiPhi, ApiType, ApiValue, SPIDIR_ICMP_EQ, SPIDIR_ICMP_NE, SPIDIR_ICMP_SLE, SPIDIR_ICMP_SLT,
    SPIDIR_ICMP_ULE, SPIDIR_ICMP_ULT,
};

#[no_mangle]
unsafe extern "C" fn spidir_builder_create_block(builder: *mut FunctionBuilder<'_>) -> ApiBlock {
    unsafe {
        let builder = &mut *builder;
        let block = builder.create_block();
        ApiBlock(block.as_u32())
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_set_block(builder: *mut FunctionBuilder<'_>, block: ApiBlock) {
    unsafe {
        let builder = &mut *builder;
        builder.set_block(block_from_api(block));
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_set_entry_block(
    builder: *mut FunctionBuilder<'_>,
    block: ApiBlock,
) {
    unsafe {
        let builder = &mut *builder;
        builder.set_entry_block(block_from_api(block));
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_param_ref(
    builder: *mut FunctionBuilder<'_>,
    index: u32,
) -> ApiValue {
    unsafe {
        let builder = &mut *builder;
        value_to_api(builder.build_param_ref(index))
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_call(
    builder: *mut FunctionBuilder<'_>,
    ret_type: *const ApiType,
    func: ApiFunction,
    arg_count: usize,
    args: *const ApiValue,
    out_ret_value: *mut ApiValue,
) {
    unsafe {
        let builder = &mut *builder;
        let args = value_list_from_api(arg_count, args);
        let ret_type = opt_type_from_api(ret_type);
        opt_value_to_api(
            builder.build_call(
                ret_type,
                FunctionRef::Internal(Function::from_u32(func.0)),
                &args,
            ),
            out_ret_value,
        );
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_extern_call(
    builder: *mut FunctionBuilder<'_>,
    ret_type: *const ApiType,
    func: ApiExternFunction,
    arg_count: usize,
    args: *const ApiValue,
    out_ret_value: *mut ApiValue,
) {
    unsafe {
        let builder = &mut *builder;
        let args = value_list_from_api(arg_count, args);
        let ret_type = opt_type_from_api(ret_type);
        opt_value_to_api(
            builder.build_call(
                ret_type,
                FunctionRef::External(ExternFunction::from_u32(func.0)),
                &args,
            ),
            out_ret_value,
        );
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_return(
    builder: *mut FunctionBuilder<'_>,
    value: *const ApiValue,
) {
    unsafe {
        let builder = &mut *builder;
        builder.build_return(opt_value_from_api(value));
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_branch(
    builder: *mut FunctionBuilder<'_>,
    dest: ApiBlock,
) {
    unsafe {
        let builder = &mut *builder;
        builder.build_branch(block_from_api(dest));
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_brcond(
    builder: *mut FunctionBuilder<'_>,
    cond: ApiValue,
    true_dest: ApiBlock,
    false_dest: ApiBlock,
) {
    unsafe {
        let builder = &mut *builder;
        builder.build_brcond(
            value_from_api(cond),
            block_from_api(true_dest),
            block_from_api(false_dest),
        );
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_phi(
    builder: *mut FunctionBuilder<'_>,
    ty: ApiType,
    input_count: usize,
    inputs: *const ApiValue,
    out_phi_handle: *mut ApiPhi,
) -> ApiValue {
    unsafe {
        let builder = &mut *builder;
        let ty = type_from_api(ty);
        let inputs = value_list_from_api(input_count, inputs);
        let (phi_handle, res) = builder.build_phi(ty, &inputs);
        if !out_phi_handle.is_null() {
            *out_phi_handle = ApiPhi(phi_handle.as_u32());
        }
        value_to_api(res)
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_add_phi_input(
    builder: *mut FunctionBuilder<'_>,
    phi: ApiPhi,
    input: ApiValue,
) {
    unsafe {
        let builder = &mut *builder;
        builder.add_phi_input(PhiHandle::from_u32(phi.0), value_from_api(input));
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_iconst(
    builder: *mut FunctionBuilder<'_>,
    ty: ApiType,
    value: u64,
) -> ApiValue {
    unsafe {
        let builder = &mut *builder;
        value_to_api(builder.build_iconst(type_from_api(ty), value))
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_fconst(
    builder: *mut FunctionBuilder<'_>,
    value: f64,
) -> ApiValue {
    unsafe {
        let builder = &mut *builder;
        value_to_api(builder.build_fconst(Type::F64, value))
    }
}

macro_rules! impl_builder_binop {
    ($binop:ident) => {
        paste! {
            #[no_mangle]
            unsafe extern "C" fn [<spidir_builder_build_ $binop>](
                builder: *mut FunctionBuilder<'_>,
                ty: ApiType,
                a: ApiValue,
                b: ApiValue
            ) -> ApiValue {
                unsafe {
                    let builder = &mut *builder;
                    value_to_api(
                        builder.[<build_ $binop>](type_from_api(ty), value_from_api(a), value_from_api(b))
                    )
                }
            }
        }
    };
}

impl_builder_binop!(iadd);
impl_builder_binop!(isub);
impl_builder_binop!(and);
impl_builder_binop!(or);
impl_builder_binop!(xor);
impl_builder_binop!(shl);
impl_builder_binop!(lshr);
impl_builder_binop!(ashr);
impl_builder_binop!(imul);
impl_builder_binop!(sdiv);
impl_builder_binop!(udiv);

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_icmp(
    builder: *mut FunctionBuilder<'_>,
    kind: ApiIcmpKind,
    ty: ApiType,
    a: ApiValue,
    b: ApiValue,
) -> ApiValue {
    unsafe {
        let builder = &mut *builder;
        value_to_api(builder.build_icmp(
            icmp_kind_from_api(kind),
            type_from_api(ty),
            value_from_api(a),
            value_from_api(b),
        ))
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_ptroff(
    builder: *mut FunctionBuilder<'_>,
    ptr: ApiValue,
    off: ApiValue,
) -> ApiValue {
    unsafe {
        let builder = &mut *builder;
        value_to_api(builder.build_ptroff(value_from_api(ptr), value_from_api(off)))
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_load(
    builder: *mut FunctionBuilder<'_>,
    ty: ApiType,
    ptr: ApiValue,
) -> ApiValue {
    unsafe {
        let builder = &mut *builder;
        value_to_api(builder.build_load(type_from_api(ty), value_from_api(ptr)))
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_store(
    builder: *mut FunctionBuilder<'_>,
    data: ApiValue,
    ptr: ApiValue,
) {
    unsafe {
        let builder = &mut *builder;
        builder.build_store(value_from_api(data), value_from_api(ptr));
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_stackslot(
    builder: *mut FunctionBuilder<'_>,
    size: u32,
    align: u32,
) -> ApiValue {
    unsafe {
        let builder = &mut *builder;
        value_to_api(builder.build_stackslot(size, align))
    }
}

unsafe fn value_list_from_api(arg_count: usize, args: *const ApiValue) -> SmallVec<[DepValue; 4]> {
    unsafe {
        let args = slice::from_raw_parts(args, arg_count);
        args.iter().map(|&arg| value_from_api(arg)).collect()
    }
}

unsafe fn opt_value_to_api(value: Option<DepValue>, api: *mut ApiValue) {
    if !api.is_null() {
        unsafe {
            if let Some(value) = value {
                *api = value_to_api(value);
            } else {
                *api = ApiValue(u32::MAX);
            }
        }
    }
}

unsafe fn opt_value_from_api(value: *const ApiValue) -> Option<DepValue> {
    unsafe {
        if value.is_null() {
            None
        } else {
            Some(value_from_api(*value))
        }
    }
}

fn icmp_kind_from_api(kind: ApiIcmpKind) -> IcmpKind {
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

fn value_to_api(value: DepValue) -> ApiValue {
    ApiValue(value.as_u32())
}

fn value_from_api(value: ApiValue) -> DepValue {
    DepValue::from_u32(value.0)
}

fn block_from_api(block: ApiBlock) -> Block {
    Block::from_u32(block.0)
}

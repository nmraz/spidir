use frontend::{FunctionBuilder, PhiHandle};

use ir::module::Module;
use paste::paste;

use crate::types::{
    block_from_api, block_to_api, funcref_from_api, icmp_kind_from_api, mem_size_from_api,
    opt_value_from_api, opt_value_to_api, type_from_api, value_from_api, value_list_from_api,
    value_to_api, ApiBlock, ApiFunction, ApiIcmpKind, ApiMemSize, ApiPhi, ApiType, ApiValue,
};

#[no_mangle]
unsafe extern "C" fn spidir_builder_get_module(builder: *mut FunctionBuilder<'_>) -> *mut Module {
    unsafe { (*builder).module_mut() }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_create_block(builder: *mut FunctionBuilder<'_>) -> ApiBlock {
    unsafe {
        let builder = &mut *builder;
        let block = builder.create_block();
        block_to_api(block)
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_cur_block(
    builder: *mut FunctionBuilder<'_>,
    out_block: *mut ApiBlock,
) -> bool {
    unsafe {
        let builder = &mut *builder;
        if let Some(block) = builder.cur_block() {
            *out_block = block_to_api(block);
            return true;
        }
    }

    false
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
    func: ApiFunction,
    arg_count: usize,
    args: *const ApiValue,
) -> ApiValue {
    unsafe {
        let builder = &mut *builder;
        let args = value_list_from_api(arg_count, args);
        opt_value_to_api(builder.build_call(funcref_from_api(func), &args))
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_return(
    builder: *mut FunctionBuilder<'_>,
    value: ApiValue,
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
unsafe extern "C" fn spidir_builder_build_unreachable(builder: *mut FunctionBuilder<'_>) {
    unsafe {
        let builder = &mut *builder;
        builder.build_unreachable();
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
        value_to_api(builder.build_fconst(value))
    }
}

macro_rules! impl_builder_binop {
    ($binop:ident) => {
        paste! {
            #[no_mangle]
            unsafe extern "C" fn [<spidir_builder_build_ $binop>](
                builder: *mut FunctionBuilder<'_>,
                lhs: ApiValue,
                rhs: ApiValue
            ) -> ApiValue {
                unsafe {
                    let builder = &mut *builder;
                    value_to_api(
                        builder.[<build_ $binop>](value_from_api(lhs), value_from_api(rhs))
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

macro_rules! impl_builder_unop {
    ($unop:ident) => {
        paste! {
            #[no_mangle]
            unsafe extern "C" fn [<spidir_builder_build_ $unop>](
                builder: *mut FunctionBuilder<'_>,
                value: ApiValue
            ) -> ApiValue {
                unsafe {
                    let builder = &mut *builder;
                    value_to_api(
                        builder.[<build_ $unop>](value_from_api(value))
                    )
                }
            }
        }
    };
}

impl_builder_unop!(iext);
impl_builder_unop!(itrunc);

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_sfill(
    builder: *mut FunctionBuilder<'_>,
    width: u8,
    value: ApiValue,
) -> ApiValue {
    unsafe {
        let builder = &mut *builder;
        value_to_api(builder.build_sfill(width, value_from_api(value)))
    }
}

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
    size: ApiMemSize,
    ty: ApiType,
    ptr: ApiValue,
) -> ApiValue {
    unsafe {
        let builder = &mut *builder;
        value_to_api(builder.build_load(
            mem_size_from_api(size),
            type_from_api(ty),
            value_from_api(ptr),
        ))
    }
}

#[no_mangle]
unsafe extern "C" fn spidir_builder_build_store(
    builder: *mut FunctionBuilder<'_>,
    size: ApiMemSize,
    data: ApiValue,
    ptr: ApiValue,
) {
    unsafe {
        let builder = &mut *builder;
        builder.build_store(
            mem_size_from_api(size),
            value_from_api(data),
            value_from_api(ptr),
        );
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

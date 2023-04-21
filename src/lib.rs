#![cfg_attr(not(test), no_std)]

extern crate alloc;

use alloc::{borrow::ToOwned, vec::Vec};

use cranelift::{
    codegen,
    prelude::{settings::Flags, *},
};

pub fn compile_for_x64(context: &mut codegen::Context) -> Vec<u8> {
    let mut flag_builder = settings::builder();
    flag_builder.set("is_pic", "false").unwrap();
    let isa = isa::lookup_by_name("x86_64")
        .expect("x64 not supported?")
        .finish(Flags::new(flag_builder))
        .expect("failed to create ISA");
    let compiled = context.compile(&*isa).expect("failed to compile");
    compiled.code_buffer().to_owned()
}

#[cfg(test)]
mod tests {
    use cranelift::{
        codegen::{
            self,
            ir::{Function, UserFuncName},
        },
        prelude::{
            isa::CallConv, types, AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder,
            Signature,
        },
    };

    use crate::compile_for_x64;

    #[test]
    fn x64_compile_works() {
        let mut func_ctx = FunctionBuilderContext::new();

        let sig = Signature {
            params: vec![],
            returns: vec![AbiParam::new(types::I32)],
            call_conv: CallConv::Fast,
        };
        let func = Function::with_name_signature(UserFuncName::testcase("yum5"), sig);

        let mut ctx = codegen::Context::for_function(func);
        let mut bcx = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);

        let entry = bcx.create_block();
        bcx.switch_to_block(entry);
        let val = bcx.ins().iconst(types::I32, 5);
        bcx.ins().return_(&[val]);
        let x = compile_for_x64(&mut ctx);

        assert_eq!(
            x,
            &[0x55, 0x48, 0x89, 0xe5, 0xb8, 0x5, 0x0, 0x0, 0x0, 0x48, 0x89, 0xec, 0x5d, 0xc3]
        );
    }
}

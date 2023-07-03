#![cfg_attr(not(test), no_std)]

use cranelift_entity::{entity_impl, PrimaryMap};
use ir::{
    builder::NodeFactoryExt,
    module::FunctionData,
    node::{FunctionRef, IcmpKind, Type},
    valgraph::{DepValue, Node},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Block(u32);
entity_impl!(Block);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PhiHandle(Node);

struct BlockData {
    terminated: bool,
    region: Node,
    last_ctrl: DepValue,
}

pub struct FunctionBuilder<'a> {
    func: &'a mut FunctionData,
    blocks: PrimaryMap<Block, BlockData>,
    cur_block: Option<Block>,
}

impl<'a> FunctionBuilder<'a> {
    pub fn new(func: &'a mut FunctionData) -> Self {
        Self {
            func,
            blocks: PrimaryMap::new(),
            cur_block: None,
        }
    }

    pub fn cur_block(&self) -> Option<Block> {
        self.cur_block
    }

    pub fn set_block(&mut self, block: Block) {
        self.cur_block = Some(block);
    }

    pub fn create_block(&mut self) -> Block {
        let region = self.func.graph.build_region(&[]);
        self.blocks.push(BlockData {
            terminated: false,
            region: region.node,
            last_ctrl: region.ctrl,
        })
    }

    pub fn set_entry_block(&mut self, block: Block) {
        let entry_ctrl = self.func.graph.node_outputs(self.func.entry)[0];
        self.func
            .graph
            .add_node_input(self.blocks[block].region, entry_ctrl);
    }

    pub fn build_param_ref(&mut self, index: u32) -> DepValue {
        self.func.graph.node_outputs(self.func.entry)[index as usize + 1]
    }

    pub fn build_call(
        &mut self,
        ret_ty: Option<Type>,
        func: FunctionRef,
        args: &[DepValue],
    ) -> Option<DepValue> {
        let built = self
            .func
            .graph
            .build_call(ret_ty, func, self.cur_block_ctrl(), args);
        self.advance_cur_block_ctrl(built.ctrl);
        built.retval
    }

    pub fn build_return(&mut self, value: Option<DepValue>) {
        let ctrl = self.terminate_cur_block();
        self.func.graph.build_return(ctrl, value);
    }

    pub fn build_branch(&mut self, dest: Block) {
        let cur_ctrl = self.terminate_cur_block();
        self.func
            .graph
            .add_node_input(self.blocks[dest].region, cur_ctrl);
    }

    pub fn build_brcond(&mut self, cond: DepValue, true_dest: Block, false_dest: Block) {
        let cur_ctrl = self.terminate_cur_block();
        let built = self.func.graph.build_brcond(cur_ctrl, cond);
        self.func
            .graph
            .add_node_input(self.blocks[true_dest].region, built.true_ctrl);
        self.func
            .graph
            .add_node_input(self.blocks[false_dest].region, built.false_ctrl);
    }

    pub fn build_phi(&mut self, ty: Type, incoming_values: &[DepValue]) -> (PhiHandle, DepValue) {
        let selector = self
            .func
            .graph
            .node_outputs(self.blocks[self.require_cur_block()].region)[1];
        let built = self.func.graph.build_phi(ty, selector, incoming_values);
        (PhiHandle(built.node), built.output)
    }

    pub fn add_phi_input(&mut self, phi: PhiHandle, value: DepValue) {
        self.func.graph.add_node_input(phi.0, value);
    }

    pub fn build_iconst(&mut self, ty: Type, value: u64) -> DepValue {
        self.func.graph.build_iconst(ty, value)
    }

    pub fn build_fconst(&mut self, ty: Type, value: f64) -> DepValue {
        self.func.graph.build_fconst(ty, value)
    }

    pub fn build_iadd(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        self.func.graph.build_iadd(ty, a, b)
    }

    pub fn build_isub(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        self.func.graph.build_isub(ty, a, b)
    }

    pub fn build_and(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        self.func.graph.build_and(ty, a, b)
    }

    pub fn build_or(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        self.func.graph.build_or(ty, a, b)
    }

    pub fn build_xor(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        self.func.graph.build_xor(ty, a, b)
    }

    pub fn build_shl(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        self.func.graph.build_shl(ty, a, b)
    }

    pub fn build_lshr(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        self.func.graph.build_lshr(ty, a, b)
    }

    pub fn build_ashr(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        self.func.graph.build_ashr(ty, a, b)
    }

    pub fn build_imul(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        self.func.graph.build_imul(ty, a, b)
    }

    pub fn build_sdiv(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        let built = self.func.graph.build_sdiv(ty, self.cur_block_ctrl(), a, b);
        self.advance_cur_block_ctrl(built.ctrl);
        built.output
    }

    pub fn build_udiv(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        let built = self.func.graph.build_udiv(ty, self.cur_block_ctrl(), a, b);
        self.advance_cur_block_ctrl(built.ctrl);
        built.output
    }

    pub fn build_icmp(
        &mut self,
        kind: IcmpKind,
        output_ty: Type,
        a: DepValue,
        b: DepValue,
    ) -> DepValue {
        self.func.graph.build_icmp(kind, output_ty, a, b)
    }

    pub fn build_ptroff(&mut self, ptr: DepValue, off: DepValue) -> DepValue {
        self.func.graph.build_ptroff(ptr, off)
    }

    pub fn build_load(&mut self, ty: Type, ptr: DepValue) -> DepValue {
        let built = self.func.graph.build_load(ty, self.cur_block_ctrl(), ptr);
        self.advance_cur_block_ctrl(built.ctrl);
        built.output
    }

    pub fn build_store(&mut self, data: DepValue, ptr: DepValue) {
        let ctrl = self
            .func
            .graph
            .build_store(self.cur_block_ctrl(), data, ptr);
        self.advance_cur_block_ctrl(ctrl);
    }

    fn advance_cur_block_ctrl(&mut self, ctrl: DepValue) {
        let block = self.require_cur_block();
        self.blocks[block].last_ctrl = ctrl;
    }

    fn terminate_cur_block(&mut self) -> DepValue {
        let cur_block = self.require_cur_block();
        let ctrl = self.blocks[cur_block].last_ctrl;
        self.blocks[cur_block].terminated = true;
        ctrl
    }

    fn cur_block_ctrl(&self) -> DepValue {
        self.blocks[self.require_cur_block()].last_ctrl
    }

    fn require_cur_block(&self) -> Block {
        let block = self.cur_block.expect("current block not set");
        assert!(
            !self.blocks[block].terminated,
            "attempted to insert into terminated block"
        );
        block
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};
    use ir::{
        module::{FunctionData, Module, Signature},
        node::Type,
    };

    use super::*;

    fn check_built_function(
        ret_type: Option<Type>,
        params: &[Type],
        build: impl FnOnce(&mut FunctionBuilder<'_>),
        expected: Expect,
    ) {
        let mut module = Module::new();
        let func = module.functions.push(FunctionData::new(
            "func".to_owned(),
            Signature {
                ret_type,
                param_types: params.to_owned(),
            },
        ));
        build(&mut FunctionBuilder::new(&mut module.functions[func]));
        expected.assert_eq(module.to_string().trim());
    }

    #[test]
    fn build_simple_function() {
        check_built_function(
            Some(Type::I32),
            &[Type::I32, Type::I32, Type::I32],
            |builder| {
                // Add or multiply second and third parameters, depending on whether first parameter
                // is truthy.

                let entry_block = builder.create_block();
                builder.set_entry_block(entry_block);
                builder.set_block(entry_block);

                let add_block = builder.create_block();
                let mul_block = builder.create_block();

                let cond = builder.build_param_ref(0);
                let a = builder.build_param_ref(1);
                let b = builder.build_param_ref(2);

                builder.build_brcond(cond, add_block, mul_block);
                builder.set_block(add_block);
                let add_res = builder.build_iadd(Type::I32, a, b);
                builder.build_return(Some(add_res));

                builder.set_block(mul_block);
                let mul_res = builder.build_imul(Type::I32, a, b);
                builder.build_return(Some(mul_res));
            },
            expect![[r#"
                func @func:i32(i32, i32, i32) {
                    %0:ctrl, %1:i32, %2:i32, %3:i32 = entry
                    %4:ctrl, %5:phisel = region %0
                    %10:ctrl, %11:ctrl = brcond %4, %1
                    %6:ctrl, %7:phisel = region %10
                    %8:ctrl, %9:phisel = region %11
                    %12:i32 = iadd %2, %3
                    return %6, %12
                    %13:i32 = imul %2, %3
                    return %8, %13
                }"#]],
        );
    }

    #[test]
    fn build_phi_with_backedges() {
        check_built_function(
            Some(Type::I64),
            &[Type::I64],
            |builder| {
                let entry_block = builder.create_block();
                builder.set_entry_block(entry_block);
                builder.set_block(entry_block);

                let n = builder.build_param_ref(0);

                let zero = builder.build_iconst(Type::I64, 0);
                let zero_cmp = builder.build_icmp(IcmpKind::Eq, Type::I32, n, zero);

                let exit_block = builder.create_block();
                let loop_body = builder.create_block();

                builder.build_brcond(zero_cmp, exit_block, loop_body);

                builder.set_block(loop_body);
                let (indvar_n_phi, indvar_n_val) = builder.build_phi(Type::I64, &[n]);
                let (sum_phi, sum_val) = builder.build_phi(Type::I64, &[zero]);
                let next_sum = builder.build_iadd(Type::I64, sum_val, indvar_n_val);
                let one = builder.build_iconst(Type::I64, 1);
                let next_indvar_n = builder.build_isub(Type::I64, indvar_n_val, one);
                let exit_cmp = builder.build_icmp(IcmpKind::Eq, Type::I32, next_indvar_n, zero);

                builder.build_brcond(exit_cmp, exit_block, loop_body);
                builder.add_phi_input(indvar_n_phi, next_indvar_n);
                builder.add_phi_input(sum_phi, next_sum);

                builder.set_block(exit_block);
                let (_, exit_sum) = builder.build_phi(Type::I64, &[zero, next_sum]);
                builder.build_return(Some(exit_sum));
            },
            expect![[r#"
                func @func:i64(i64) {
                    %0:ctrl, %1:i64 = entry
                    %2:ctrl, %3:phisel = region %0
                    %15:i64 = iconst 1
                    %4:i64 = iconst 0
                    %5:i32 = icmp eq %1, %4
                    %10:ctrl, %11:ctrl = brcond %2, %5
                    %17:i32 = icmp eq %16, %4
                    %18:ctrl, %19:ctrl = brcond %8, %17
                    %6:ctrl, %7:phisel = region %10, %18
                    %8:ctrl, %9:phisel = region %11, %19
                    %12:i64 = phi %9, %1, %16
                    %16:i64 = isub %12, %15
                    %13:i64 = phi %9, %4, %14
                    %14:i64 = iadd %13, %12
                    %20:i64 = phi %7, %4, %14
                    return %6, %20
                }"#]],
        );
    }
}

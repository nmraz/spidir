#![cfg_attr(not(test), no_std)]

use cranelift_entity::{PrimaryMap, entity_impl};
use log::trace;

use ir::{
    builder::{Builder, BuilderExt},
    cache::CachingBuilder,
    function::{FunctionBody, Signature},
    module::{Function, Module},
    node::{DepValueKind, FcmpKind, FunctionRef, IcmpKind, MemSize, NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
    write::display_node,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Block(u32);
entity_impl!(Block);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PhiHandle(Node);

impl PhiHandle {
    #[inline]
    pub fn as_u32(self) -> u32 {
        self.0.as_u32()
    }

    #[inline]
    pub fn from_u32(x: u32) -> Self {
        Self(Node::from_u32(x))
    }
}

struct BlockData {
    terminated: bool,
    region: Node,
    last_ctrl: DepValue,
}

pub struct FunctionBuilder<'a> {
    module: &'a mut Module,
    func: Function,
    blocks: PrimaryMap<Block, BlockData>,
    cur_block: Option<Block>,
}

impl<'a> FunctionBuilder<'a> {
    #[inline]
    pub fn new(module: &'a mut Module, func: Function) -> Self {
        Self {
            module,
            func,
            blocks: PrimaryMap::new(),
            cur_block: None,
        }
    }

    #[inline]
    pub fn module(&self) -> &Module {
        self.module
    }

    #[inline]
    pub fn module_mut(&mut self) -> &mut Module {
        self.module
    }

    pub fn create_block(&mut self) -> Block {
        let region = self.builder().build_region(&[]);
        self.blocks.push(BlockData {
            terminated: false,
            region: region.node,
            last_ctrl: region.ctrl,
        })
    }

    #[inline]
    pub fn cur_block(&self) -> Option<Block> {
        self.cur_block
    }

    #[inline]
    pub fn set_block(&mut self, block: Block) {
        self.cur_block = Some(block);
        trace!("switched to block {}", block.as_u32());
    }

    pub fn set_entry_block(&mut self, block: Block) {
        let entry = self.body().entry;
        let region = self.blocks[block].region;
        let graph = self.graph_mut();

        let entry_ctrl = graph.node_outputs(entry)[0];
        graph.add_node_input(region, entry_ctrl);
    }

    pub fn build_param_ref(&mut self, index: u32) -> DepValue {
        self.graph().node_outputs(self.body().entry)[index as usize + 1]
    }

    pub fn build_funcaddr(&mut self, func: FunctionRef) -> DepValue {
        self.builder().build_funcaddr(func)
    }

    pub fn build_call(&mut self, func: FunctionRef, args: &[DepValue]) -> Option<DepValue> {
        let ret_ty = self.module.resolve_funcref(func).sig.ret_type;
        let ctrl = self.cur_block_ctrl();
        let built = self.builder().build_call(ret_ty, func, ctrl, args);
        self.advance_cur_block_ctrl(built.ctrl);
        built.retval
    }

    pub fn build_callind(
        &mut self,
        sig: Signature,
        target: DepValue,
        args: &[DepValue],
    ) -> Option<DepValue> {
        let ctrl = self.cur_block_ctrl();
        let built = self.builder().build_callind(sig, ctrl, target, args);
        self.advance_cur_block_ctrl(built.ctrl);
        built.retval
    }

    pub fn build_return(&mut self, value: Option<DepValue>) {
        let ctrl = self.terminate_cur_block();
        self.builder().build_return(ctrl, value);
    }

    pub fn build_branch(&mut self, dest: Block) {
        let cur_ctrl = self.terminate_cur_block();
        let region = self.blocks[dest].region;
        self.graph_mut().add_node_input(region, cur_ctrl);
        trace!("branched to block {}", dest.as_u32());
    }

    pub fn build_brcond(&mut self, cond: DepValue, true_dest: Block, false_dest: Block) {
        let cur_ctrl = self.terminate_cur_block();
        let built = self.builder().build_brcond(cur_ctrl, cond);

        let true_region = self.blocks[true_dest].region;
        let false_region = self.blocks[false_dest].region;

        let graph = self.graph_mut();
        graph.add_node_input(true_region, built.true_ctrl);
        graph.add_node_input(false_region, built.false_ctrl);
    }

    pub fn build_unreachable(&mut self) {
        let ctrl = self.terminate_cur_block();
        self.builder().build_unreachable(ctrl);
    }

    pub fn build_phi(&mut self, ty: Type, incoming_values: &[DepValue]) -> (PhiHandle, DepValue) {
        let selector = self
            .graph()
            .node_outputs(self.blocks[self.require_cur_block()].region)[1];
        let built = self.builder().build_phi(ty, selector, incoming_values);
        (PhiHandle(built.node), built.output)
    }

    pub fn add_phi_input(&mut self, phi: PhiHandle, value: DepValue) {
        self.graph_mut().add_node_input(phi.0, value);
    }

    pub fn build_iconst(&mut self, ty: Type, value: u64) -> DepValue {
        self.builder().build_iconst(ty, value)
    }

    pub fn build_iadd(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        self.builder().build_iadd(lhs, rhs)
    }

    pub fn build_isub(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        self.builder().build_isub(lhs, rhs)
    }

    pub fn build_and(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        self.builder().build_and(lhs, rhs)
    }

    pub fn build_or(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        self.builder().build_or(lhs, rhs)
    }

    pub fn build_xor(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        self.builder().build_xor(lhs, rhs)
    }

    pub fn build_shl(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        self.builder().build_shl(lhs, rhs)
    }

    pub fn build_lshr(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        self.builder().build_lshr(lhs, rhs)
    }

    pub fn build_ashr(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        self.builder().build_ashr(lhs, rhs)
    }

    pub fn build_imul(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        self.builder().build_imul(lhs, rhs)
    }

    pub fn build_sdiv(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        let ctrl = self.cur_block_ctrl();
        let built = self.builder().build_sdiv(ctrl, lhs, rhs);
        self.advance_cur_block_ctrl(built.ctrl);
        built.output
    }

    pub fn build_udiv(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        let ctrl = self.cur_block_ctrl();
        let built = self.builder().build_udiv(ctrl, lhs, rhs);
        self.advance_cur_block_ctrl(built.ctrl);
        built.output
    }

    pub fn build_srem(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        let ctrl = self.cur_block_ctrl();
        let built = self.builder().build_srem(ctrl, lhs, rhs);
        self.advance_cur_block_ctrl(built.ctrl);
        built.output
    }

    pub fn build_urem(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        let ctrl = self.cur_block_ctrl();
        let built = self.builder().build_urem(ctrl, lhs, rhs);
        self.advance_cur_block_ctrl(built.ctrl);
        built.output
    }

    pub fn build_iext(&mut self, value: DepValue) -> DepValue {
        self.builder().build_iext(value)
    }

    pub fn build_itrunc(&mut self, value: DepValue) -> DepValue {
        self.builder().build_itrunc(value)
    }

    pub fn build_sfill(&mut self, width: u8, value: DepValue) -> DepValue {
        self.builder().build_sfill(width, value)
    }

    pub fn build_icmp(
        &mut self,
        kind: IcmpKind,
        output_ty: Type,
        lhs: DepValue,
        rhs: DepValue,
    ) -> DepValue {
        self.builder().build_icmp(kind, output_ty, lhs, rhs)
    }

    pub fn build_fconst64(&mut self, value: f64) -> DepValue {
        self.builder().build_fconst64(value)
    }

    pub fn build_fadd(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        self.builder().build_fadd(lhs, rhs)
    }
    pub fn build_fsub(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        self.builder().build_fsub(lhs, rhs)
    }
    pub fn build_fmul(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        self.builder().build_fmul(lhs, rhs)
    }
    pub fn build_fdiv(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        self.builder().build_fdiv(lhs, rhs)
    }

    pub fn build_fcmp(
        &mut self,
        kind: FcmpKind,
        output_ty: Type,
        lhs: DepValue,
        rhs: DepValue,
    ) -> DepValue {
        self.builder().build_fcmp(kind, output_ty, lhs, rhs)
    }

    pub fn build_ptroff(&mut self, ptr: DepValue, off: DepValue) -> DepValue {
        self.builder().build_ptroff(ptr, off)
    }

    pub fn build_inttoptr(&mut self, value: DepValue) -> DepValue {
        self.builder().build_inttoptr(value)
    }
    pub fn build_ptrtoint(&mut self, value: DepValue) -> DepValue {
        self.builder().build_ptrtoint(value)
    }

    pub fn build_load(&mut self, size: MemSize, ty: Type, ptr: DepValue) -> DepValue {
        let ctrl = self.cur_block_ctrl();
        let built = self.builder().build_load(size, ty, ctrl, ptr);
        self.advance_cur_block_ctrl(built.ctrl);
        built.output
    }

    pub fn build_store(&mut self, size: MemSize, data: DepValue, ptr: DepValue) {
        let ctrl = self.cur_block_ctrl();
        let ctrl = self.builder().build_store(size, ctrl, data, ptr);
        self.advance_cur_block_ctrl(ctrl);
    }

    pub fn build_stackslot(&mut self, size: u32, align: u32) -> DepValue {
        self.builder().build_stackslot(size, align)
    }

    fn advance_cur_block_ctrl(&mut self, ctrl: DepValue) {
        let block = self.require_cur_block();
        self.blocks[block].last_ctrl = ctrl;
    }

    fn terminate_cur_block(&mut self) -> DepValue {
        let cur_block = self.require_cur_block();
        let ctrl = self.blocks[cur_block].last_ctrl;
        self.blocks[cur_block].terminated = true;
        trace!(
            "terminating block {}, last control node `{}`",
            cur_block.as_u32(),
            display_node(self.module, self.body(), self.graph().value_def(ctrl).0)
        );
        ctrl
    }

    fn builder(&mut self) -> impl Builder + '_ {
        GraphBuilderWrapper {
            module: self.module,
            func: self.func,
        }
    }

    fn cur_block_ctrl(&self) -> DepValue {
        self.blocks[self.require_cur_block()].last_ctrl
    }

    fn require_cur_block(&self) -> Block {
        let block = self.cur_block.expect("current block not set");
        assert!(
            !self.blocks[block].terminated,
            "attempted to insert into terminated block {}",
            block.as_u32()
        );
        block
    }

    fn graph(&self) -> &ValGraph {
        &self.body().graph
    }

    fn graph_mut(&mut self) -> &mut ValGraph {
        &mut self.body_mut().graph
    }

    fn body(&self) -> &FunctionBody {
        &self.module.functions[self.func].body
    }

    fn body_mut(&mut self) -> &mut FunctionBody {
        &mut self.module.functions[self.func].body
    }
}

struct GraphBuilderWrapper<'a> {
    module: &'a mut Module,
    func: Function,
}

impl Builder for GraphBuilderWrapper<'_> {
    fn create_node(
        &mut self,
        kind: NodeKind,
        inputs: impl IntoIterator<Item = DepValue>,
        output_kinds: impl IntoIterator<Item = DepValueKind>,
    ) -> Node {
        let func = &mut self.module.functions[self.func];
        let mut builder = CachingBuilder::new(&mut func.body, &mut func.node_cache);
        let node = builder.create_node(kind, inputs, output_kinds);
        trace!(
            "built node: `{}`",
            display_node(self.module, self.body(), node)
        );
        node
    }

    fn body(&self) -> &FunctionBody {
        &self.module.functions[self.func].body
    }

    fn body_mut(&mut self) -> &mut FunctionBody {
        &mut self.module.functions[self.func].body
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use ir::{
        function::{FunctionData, Signature},
        module::Module,
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
        build(&mut FunctionBuilder::new(&mut module, func));
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
                let add_res = builder.build_iadd(a, b);
                builder.build_return(Some(add_res));

                builder.set_block(mul_block);
                let mul_res = builder.build_imul(a, b);
                builder.build_return(Some(mul_res));
            },
            expect![[r#"
                func @func:i32(i32, i32, i32) {
                    %0:ctrl, %1:i32, %2:i32, %3:i32 = entry
                    %4:ctrl, %5:phisel = region %0
                    %10:ctrl, %11:ctrl = brcond %4, %1
                    %6:ctrl, %7:phisel = region %10
                    %8:ctrl, %9:phisel = region %11
                    %13:i32 = imul %2, %3
                    return %8, %13
                    %12:i32 = iadd %2, %3
                    return %6, %12
                }"#]],
        );
    }

    #[test]
    fn duplicated_nodes() {
        check_built_function(
            Some(Type::I32),
            &[Type::I32],
            |builder| {
                let entry_block = builder.create_block();
                builder.set_block(entry_block);
                builder.set_entry_block(entry_block);

                let param = builder.build_param_ref(0);
                let one = builder.build_iconst(Type::I32, 1);
                let x_inc1 = builder.build_iadd(param, one);

                let one = builder.build_iconst(Type::I32, 1);
                let x_inc2 = builder.build_iadd(param, one);

                let product = builder.build_imul(x_inc1, x_inc2);
                builder.build_return(Some(product));
            },
            expect![[r#"
                func @func:i32(i32) {
                    %0:ctrl, %1:i32 = entry
                    %2:ctrl, %3:phisel = region %0
                    %4:i32 = iconst 1
                    %5:i32 = iadd %1, %4
                    %6:i32 = imul %5, %5
                    return %2, %6
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
                let next_sum = builder.build_iadd(sum_val, indvar_n_val);
                let one = builder.build_iconst(Type::I64, 1);
                let next_indvar_n = builder.build_isub(indvar_n_val, one);
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
                    %8:ctrl, %9:phisel = region %11, %19
                    %12:i64 = phi %9, %1, %16
                    %16:i64 = isub %12, %15
                    %17:i32 = icmp eq %16, %4
                    %18:ctrl, %19:ctrl = brcond %8, %17
                    %6:ctrl, %7:phisel = region %10, %18
                    %14:i64 = iadd %13, %12
                    %20:i64 = phi %7, %4, %14
                    return %6, %20
                    %13:i64 = phi %9, %4, %14
                }"#]],
        );
    }

    #[test]
    fn build_stack_slots() {
        check_built_function(
            None,
            &[Type::I32, Type::F64],
            |builder| {
                let entry_block = builder.create_block();
                builder.set_entry_block(entry_block);
                builder.set_block(entry_block);

                let val32 = builder.build_param_ref(0);
                let val64 = builder.build_param_ref(1);

                let slot32 = builder.build_stackslot(4, 4);
                builder.build_store(MemSize::S4, val32, slot32);

                let slot64 = builder.build_stackslot(8, 8);
                builder.build_store(MemSize::S8, val64, slot64);

                builder.build_return(None);
            },
            expect![[r#"
                func @func(i32, f64) {
                    %0:ctrl, %1:i32, %2:f64 = entry
                    %5:ptr = stackslot 4:4
                    %7:ptr = stackslot 8:8
                    %3:ctrl, %4:phisel = region %0
                    %6:ctrl = store.4 %3, %1, %5
                    %8:ctrl = store.8 %6, %2, %7
                    return %8
                }"#]],
        );
    }

    #[test]
    fn build_callind() {
        check_built_function(
            None,
            &[Type::Ptr],
            |builder| {
                let entry_block = builder.create_block();
                builder.set_entry_block(entry_block);
                builder.set_block(entry_block);
                let target = builder.build_param_ref(0);
                builder.build_callind(
                    Signature {
                        ret_type: None,
                        param_types: vec![],
                    },
                    target,
                    &[],
                );
                builder.build_return(None);
            },
            expect![[r#"
                func @func(ptr) {
                    %0:ctrl, %1:ptr = entry
                    %2:ctrl, %3:phisel = region %0
                    %4:ctrl = callind () %2, %1
                    return %4
                }"#]],
        )
    }
}

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
        self.func.graph.build_return(self.cur_block_ctrl(), value);
    }

    pub fn build_branch(&mut self, dest: Block) {
        let cur_ctrl = self.cur_block_ctrl();
        self.func
            .graph
            .add_node_input(self.blocks[dest].region, cur_ctrl);
    }

    pub fn build_brcond(&mut self, cond: DepValue, true_dest: Block, false_dest: Block) {
        let cur_ctrl = self.cur_block_ctrl();
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

    fn cur_block_ctrl(&self) -> DepValue {
        self.blocks[self.require_cur_block()].last_ctrl
    }

    fn require_cur_block(&self) -> Block {
        self.cur_block.expect("current block not set")
    }
}

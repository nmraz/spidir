use alloc::vec::Vec;
use core::{fmt, iter, ops::Range};
use fx_utils::FxHashMap;

use cranelift_entity::SecondaryMap;

use crate::cfg::{Block, BlockCfg};

mod display;

pub use display::{
    Display, DisplayDefOperand, DisplayInstr, DisplayUseOperand, DisplayVirtReg, RegNames,
};
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct RegClass(u8);

impl RegClass {
    pub const fn new(class: u8) -> Self {
        Self(class)
    }

    pub fn as_u8(self) -> u8 {
        self.0
    }
}

const REG_NUM_BITS: usize = 24;
const REG_NUM_START_BIT: usize = 8;
const REG_NUM_BOUND: u32 = 1 << REG_NUM_BITS;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct VirtReg(u32);

impl VirtReg {
    pub fn new(num: u32, class: RegClass) -> Self {
        assert!(num < REG_NUM_BOUND);
        Self(num << REG_NUM_START_BIT | class.as_u8() as u32)
    }

    pub fn reg_num(self) -> u32 {
        self.0 >> REG_NUM_START_BIT
    }

    pub fn class(self) -> RegClass {
        RegClass(self.0 as u8)
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }

    pub fn from_u32(value: u32) -> Self {
        Self(value)
    }

    pub fn display(self, reg_names: &dyn RegNames) -> DisplayVirtReg<'_> {
        DisplayVirtReg {
            reg: self,
            reg_names,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PhysReg(u8);

impl PhysReg {
    pub const fn new(r: u8) -> Self {
        Self(r)
    }

    pub fn as_u8(self) -> u8 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OperandPos {
    Early,
    Late,
}

impl OperandPos {
    pub fn as_str(self) -> &'static str {
        match self {
            OperandPos::Early => "early",
            OperandPos::Late => "late",
        }
    }
}

impl fmt::Display for OperandPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum UseOperandConstraint {
    Any,
    AnyReg,
    Fixed(PhysReg),
    TiedToDef(u8),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefOperandConstraint {
    Any,
    AnyReg,
    Fixed(PhysReg),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct UseOperand {
    reg: VirtReg,
    constraint: UseOperandConstraint,
    pos: OperandPos,
}

impl UseOperand {
    pub fn new(reg: VirtReg, constraint: UseOperandConstraint, pos: OperandPos) -> Self {
        Self {
            reg,
            constraint,
            pos,
        }
    }

    pub fn reg(&self) -> VirtReg {
        self.reg
    }

    pub fn constraint(&self) -> UseOperandConstraint {
        self.constraint
    }

    pub fn pos(&self) -> OperandPos {
        self.pos
    }

    pub fn display<'a>(&'a self, reg_names: &'a dyn RegNames) -> DisplayUseOperand<'a> {
        DisplayUseOperand {
            operand: self,
            reg_names,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefOperand {
    reg: VirtReg,
    constraint: DefOperandConstraint,
    pos: OperandPos,
}

impl DefOperand {
    pub fn new(reg: VirtReg, constraint: DefOperandConstraint, pos: OperandPos) -> Self {
        Self {
            reg,
            constraint,
            pos,
        }
    }

    pub fn reg(&self) -> VirtReg {
        self.reg
    }

    pub fn constraint(&self) -> DefOperandConstraint {
        self.constraint
    }

    pub fn pos(&self) -> OperandPos {
        self.pos
    }

    pub fn display<'a>(&'a self, reg_names: &'a dyn RegNames) -> DisplayDefOperand<'a> {
        DisplayDefOperand {
            operand: self,
            reg_names,
        }
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Instr(u32);

impl Instr {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn from_usize(index: usize) -> Self {
        Self::new(index.try_into().unwrap())
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }

    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

pub struct InstrRange(Range<u32>);

impl InstrRange {
    pub fn new(start: Instr, end: Instr) -> Self {
        Self(start.0..end.0)
    }

    pub fn start(&self) -> Instr {
        Instr(self.0.start)
    }

    pub fn end(&self) -> Instr {
        Instr(self.0.end)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl Iterator for InstrRange {
    type Item = Instr;

    fn next(&mut self) -> Option<Instr> {
        self.0.next().map(Instr)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl DoubleEndedIterator for InstrRange {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(Instr)
    }
}

impl ExactSizeIterator for InstrRange {}

#[derive(Debug, Clone, Copy)]
struct InstrOperands {
    def_base: u32,
    def_len: u16,
    use_base: u32,
    use_len: u16,
}

#[derive(Debug, Default, Clone, Copy)]
struct BlockParamData {
    incoming_base: u32,
    incoming_len: u16,
    outgoing_base: u32,
    outgoing_len: u16,
}

#[derive(Clone)]
pub struct Lir<I> {
    block_instr_ranges: SecondaryMap<Block, (Instr, Instr)>,
    block_params: SecondaryMap<Block, BlockParamData>,
    outgoing_block_param_indices: Vec<(u32, u32)>,
    block_param_pool: Vec<VirtReg>,
    instrs: Vec<I>,
    instr_operands: Vec<InstrOperands>,
    def_pool: Vec<DefOperand>,
    use_pool: Vec<UseOperand>,
}

impl<I> Lir<I> {
    pub fn block_instrs(&self, block: Block) -> InstrRange {
        let (start, end) = self.block_instr_ranges[block];
        InstrRange::new(start, end)
    }

    pub fn instr_data(&self, instr: Instr) -> &I {
        &self.instrs[instr.as_usize()]
    }

    pub fn instr_uses(&self, instr: Instr) -> &[UseOperand] {
        let operands = &self.instr_operands[instr.as_usize()];
        let base = operands.use_base as usize;
        &self.use_pool[base..base + operands.use_len as usize]
    }

    pub fn instr_defs(&self, instr: Instr) -> &[DefOperand] {
        let operands = &self.instr_operands[instr.as_usize()];
        let base = operands.def_base as usize;
        &self.def_pool[base..base + operands.def_len as usize]
    }

    pub fn block_params(&self, block: Block) -> &[VirtReg] {
        let params = &self.block_params[block];
        let base = params.incoming_base as usize;
        &self.block_param_pool[base..base + params.incoming_len as usize]
    }

    pub fn outgoing_block_params(&self, block: Block, succ: u32) -> &[VirtReg] {
        let block_param_data = &self.block_params[block];
        let index_base = block_param_data.outgoing_base as usize;
        let indices = &self.outgoing_block_param_indices
            [index_base..index_base + block_param_data.outgoing_len as usize];
        let (base, len) = indices[succ as usize];
        let base = base as usize;
        &self.block_param_pool[base..base + len as usize]
    }
}

impl<I: fmt::Debug> Lir<I> {
    pub fn display_instr<'a>(
        &'a self,
        reg_names: &'a dyn RegNames,
        instr: Instr,
    ) -> DisplayInstr<'a, I> {
        DisplayInstr {
            lir: self,
            reg_names,
            instr,
        }
    }

    pub fn display<'a>(
        &'a self,
        cfg: &'a BlockCfg,
        block_order: &'a [Block],
        reg_names: &'a dyn RegNames,
    ) -> Display<'a, I> {
        Display {
            lir: self,
            cfg,
            block_order,
            reg_names,
        }
    }
}

pub struct InstrBuilder<'b, 'o, I> {
    builder: &'b mut Builder<'o, I>,
}

impl<I> InstrBuilder<'_, '_, I> {
    pub fn create_vreg(&mut self, class: RegClass) -> VirtReg {
        let num = self.builder.next_vreg;
        self.builder.next_vreg += 1;
        VirtReg::new(num, class)
    }

    pub fn copy_vreg(&mut self, dest: VirtReg, src: VirtReg) {
        let src = resolve_vreg_copy(&mut self.builder.vreg_copies, src).unwrap_or(src);
        assert!(
            dest != src,
            "vreg copy cycle on register {}",
            dest.reg_num()
        );
        assert!(
            src.class() == dest.class(),
            "attempted to copy vreg of class {} into vreg of class {}",
            src.class().as_u8(),
            dest.class().as_u8()
        );

        let prev = self.builder.vreg_copies.insert(dest, src);
        if let Some(prev) = prev {
            panic!(
                "vreg {} already copied from {}",
                dest.reg_num(),
                prev.reg_num()
            );
        }
    }

    pub fn push_instr(
        &mut self,
        data: I,
        defs: impl IntoIterator<Item = DefOperand>,
        uses: impl IntoIterator<Item = UseOperand>,
    ) {
        self.builder.lir.instrs.push(data);

        let def_base = self.builder.lir.def_pool.len();
        self.builder.lir.def_pool.extend(defs);
        let def_len = self.builder.lir.def_pool.len() - def_base;

        let use_base = self.builder.lir.use_pool.len();
        self.builder.lir.use_pool.extend(uses);
        let use_len = self.builder.lir.use_pool.len() - use_base;

        self.builder.lir.instr_operands.push(InstrOperands {
            def_base: def_base.try_into().unwrap(),
            def_len: def_len.try_into().unwrap(),
            use_base: use_base.try_into().unwrap(),
            use_len: use_len.try_into().unwrap(),
        });

        assert!(self.builder.lir.instrs.len() == self.builder.lir.instr_operands.len());
    }
}

pub struct OutgoingBlockParamBuilder<'b> {
    outgoing_block_param_indices: &'b mut Vec<(u32, u32)>,
    block_param_pool: &'b mut Vec<VirtReg>,
}

impl<'b> OutgoingBlockParamBuilder<'b> {
    pub fn push_succ_block_params(&mut self, outgoing_params: impl Iterator<Item = VirtReg>) {
        let base = self.block_param_pool.len();
        self.block_param_pool.extend(outgoing_params);
        let len = self.block_param_pool.len() - base;
        self.outgoing_block_param_indices
            .push((base.try_into().unwrap(), len.try_into().unwrap()));
    }
}

pub struct Builder<'o, I> {
    lir: Lir<I>,
    block_order: &'o [Block],
    vreg_copies: FxHashMap<VirtReg, VirtReg>,
    next_vreg: u32,
    last_finished_block: usize,
}

impl<'o, I> Builder<'o, I> {
    pub fn new(block_order: &'o [Block]) -> Self {
        assert!(!block_order.is_empty());
        let mut builder = Self {
            lir: Lir {
                block_instr_ranges: SecondaryMap::new(),
                block_params: SecondaryMap::new(),
                outgoing_block_param_indices: Vec::new(),
                block_param_pool: Vec::new(),
                instrs: Vec::new(),
                instr_operands: Vec::new(),
                def_pool: Vec::new(),
                use_pool: Vec::new(),
            },
            block_order,
            vreg_copies: FxHashMap::default(),
            last_finished_block: block_order.len(),
            next_vreg: 0,
        };
        builder.lir.block_instr_ranges[*block_order.last().unwrap()].1 = Instr::new(0);
        builder
    }

    pub fn finish_block(&mut self) {
        let next_instr = self.next_instr();
        self.last_finished_block -= 1;
        let last_finished_block = self.last_finished_block;

        // Note: we always want `.0` to be greater than `.1` for every block, so that when we
        // reverse everything later we'll end up with `.0 < .1`.
        self.lir.block_instr_ranges[self.block_order[last_finished_block]].0 = next_instr;

        if last_finished_block > 0 {
            self.lir.block_instr_ranges[self.block_order[last_finished_block - 1]].1 = next_instr;
        }
    }

    pub fn set_block_params(&mut self, block: Block, params: impl IntoIterator<Item = VirtReg>) {
        let base = self.lir.block_param_pool.len();
        self.lir.block_param_pool.extend(params);
        let len = self.lir.block_param_pool.len() - base;
        self.lir.block_params[block].incoming_base = base.try_into().unwrap();
        self.lir.block_params[block].incoming_len = len.try_into().unwrap();
    }

    pub fn set_outgoing_block_params(
        &mut self,
        block: Block,
        f: impl FnOnce(OutgoingBlockParamBuilder<'_>),
    ) {
        let base = self.lir.outgoing_block_param_indices.len();
        f(OutgoingBlockParamBuilder {
            outgoing_block_param_indices: &mut self.lir.outgoing_block_param_indices,
            block_param_pool: &mut self.lir.block_param_pool,
        });
        let len = self.lir.outgoing_block_param_indices.len() - base;
        self.lir.block_params[block].outgoing_base = base.try_into().unwrap();
        self.lir.block_params[block].outgoing_len = len.try_into().unwrap();
    }

    pub fn build_instrs(&mut self, f: impl FnOnce(InstrBuilder<'_, '_, I>)) {
        let orig_len = self.lir.instrs.len();
        f(InstrBuilder { builder: self });
        // We're building the LIR in reverse order, but the sequence of instructions should end up
        // in insertion order, so reverse this tail sequence we've just built.
        self.reverse_instrs(orig_len..self.lir.instrs.len());
    }

    pub fn finish(mut self) -> Lir<I> {
        assert!(self.last_finished_block == 0);

        let instr_count = self.lir.instrs.len();

        // We've built everything backwards, so reverse things now.
        self.reverse_instrs(0..instr_count);
        let instr_count: u32 = instr_count.try_into().unwrap();
        for (block_start, block_end) in self.lir.block_instr_ranges.values_mut() {
            *block_start = Instr::new(instr_count - block_start.as_u32());
            *block_end = Instr::new(instr_count - block_end.as_u32());
            assert!(*block_start <= *block_end);
        }

        self.propagate_vreg_copies();

        self.lir
    }

    fn next_instr(&self) -> Instr {
        Instr::from_usize(self.lir.instrs.len())
    }

    fn reverse_instrs(&mut self, range: Range<usize>) {
        self.lir.instrs[range.clone()].reverse();
        self.lir.instr_operands[range].reverse();
    }

    fn propagate_vreg_copies(&mut self) {
        for vreg_use in &mut self.lir.use_pool {
            if let Some(def) = resolve_vreg_copy(&mut self.vreg_copies, vreg_use.reg) {
                vreg_use.reg = def;
            }
        }
    }
}

fn resolve_vreg_copy(
    vreg_copies: &mut FxHashMap<VirtReg, VirtReg>,
    vreg: VirtReg,
) -> Option<VirtReg> {
    let def = vreg_copies.get(&vreg).copied()?;

    // Look through all existing copies we have here.
    let def = iter::successors(Some(def), |def| vreg_copies.get(def).copied())
        .last()
        .unwrap();

    // Poor man's path compression: we don't compress the entire path since that could require
    // linear space, but let's opportunistically shorten the one starting at this vreg.
    vreg_copies.insert(vreg, def);

    Some(def)
}

#[cfg(test)]
mod tests;

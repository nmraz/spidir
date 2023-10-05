use alloc::vec::Vec;
use core::{iter, ops::Range};
use fx_utils::FxHashMap;

use cranelift_entity::SecondaryMap;

use crate::cfg::Block;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct RegClass(u8);

impl RegClass {
    pub fn new(class: u8) -> Self {
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
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PhysReg(u8);

impl PhysReg {
    pub fn new(r: u8) -> Self {
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
    def_count: u16,
    use_base: u32,
    use_count: u16,
}

#[derive(Clone)]
pub struct Lir<I> {
    block_instr_ranges: SecondaryMap<Block, (Instr, Instr)>,
    block_params: SecondaryMap<Block, (u32, u32)>,
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
        &self.use_pool[operands.use_base as usize..operands.use_count as usize]
    }

    pub fn instr_defs(&self, instr: Instr) -> &[DefOperand] {
        let operands = &self.instr_operands[instr.as_usize()];
        &self.def_pool[operands.def_base as usize..operands.def_count as usize]
    }

    pub fn block_params(&self, block: Block) -> &[VirtReg] {
        let (start, end) = self.block_params[block];
        &self.block_param_pool[start as usize..end as usize]
    }

    pub fn outgoing_block_params(&self, _block: Block, _succ: u32) -> &[VirtReg] {
        todo!()
    }
}

pub struct InstrBuilder<'b, I> {
    builder: &'b mut Builder<I>,
}

impl<'b, I> InstrBuilder<'b, I> {
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
            def_count: def_len.try_into().unwrap(),
            use_base: use_base.try_into().unwrap(),
            use_count: use_len.try_into().unwrap(),
        });

        assert!(self.builder.lir.instrs.len() == self.builder.lir.instr_operands.len());
    }
}

pub struct Builder<I> {
    lir: Lir<I>,
    vreg_copies: FxHashMap<VirtReg, VirtReg>,
    next_vreg: u32,
    cur_block: Option<Block>,
}

impl<I> Builder<I> {
    pub fn new() -> Self {
        Self {
            lir: Lir {
                block_instr_ranges: SecondaryMap::new(),
                block_params: SecondaryMap::new(),
                block_param_pool: Vec::new(),
                instrs: Vec::new(),
                instr_operands: Vec::new(),
                def_pool: Vec::new(),
                use_pool: Vec::new(),
            },
            vreg_copies: FxHashMap::default(),
            cur_block: None,
            next_vreg: 0,
        }
    }

    pub fn mark_block(&mut self, block: Block) {
        let next_instr = self.next_instr();

        // Note: we always want `.0` to be greater than `.1` for every block, so that when we
        // reverse everything later we'll end up with `.0 < .1`.
        if let Some(cur_block) = self.cur_block {
            self.lir.block_instr_ranges[cur_block].0 = next_instr;
        }
        self.lir.block_instr_ranges[block].1 = next_instr;
    }

    pub fn build_instrs(&mut self, f: impl FnOnce(InstrBuilder<'_, I>)) {
        let orig_len = self.lir.instrs.len();
        f(InstrBuilder { builder: self });
        // We're building the LIR in reverse order, but the sequence of instructions should end up
        // in insertion order, so reverse this tail sequence we've just built.
        self.reverse_instrs(orig_len..self.lir.instrs.len());
    }

    pub fn finish(mut self) -> Lir<I> {
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

impl<I> Default for Builder<I> {
    fn default() -> Self {
        Self::new()
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

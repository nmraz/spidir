use alloc::vec::Vec;
use core::{fmt, iter, marker::PhantomData, ops::Range};
use fx_utils::FxHashMap;

use cranelift_entity::{
    entity_impl, packed_option::PackedOption, EntityRef, PrimaryMap, SecondaryMap,
};

use crate::{
    cfg::{Block, BlockCfg},
    machine::MachineCore,
};

mod display;

pub use display::{
    display_instr_data, Display, DisplayDefOperand, DisplayInstr, DisplayUseOperand, DisplayVirtReg,
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
pub struct VirtRegNum(u32);
entity_impl!(VirtRegNum, "%");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct VirtReg(u32);

impl VirtReg {
    pub fn new(num: u32, class: RegClass) -> Self {
        assert!(num < REG_NUM_BOUND);
        Self(num << REG_NUM_START_BIT | class.as_u8() as u32)
    }

    pub fn reg_num(self) -> VirtRegNum {
        VirtRegNum::from_u32(self.0 >> REG_NUM_START_BIT)
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

    pub fn display<M: MachineCore>(self) -> DisplayVirtReg<M> {
        DisplayVirtReg {
            reg: self,
            _marker: PhantomData,
        }
    }
}

const PHYS_REG_COUNT: u8 = 128;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PhysReg(u8);

impl PhysReg {
    pub const fn new(r: u8) -> Self {
        assert!(r < PHYS_REG_COUNT, "physical register number too large");
        Self(r)
    }

    pub fn as_u8(self) -> u8 {
        self.0
    }
}

#[derive(Clone, Copy)]
pub struct PhysRegSet(u128);

impl PhysRegSet {
    pub const fn empty() -> Self {
        Self(0)
    }

    pub fn is_empty(&self) -> bool {
        self.0 == 0
    }

    pub fn iter(&self) -> impl Iterator<Item = PhysReg> + '_ {
        (0..PHYS_REG_COUNT)
            .map(PhysReg)
            .filter(move |&reg| self.contains(reg))
    }

    pub fn contains(&self, reg: PhysReg) -> bool {
        (self.0 & (1 << reg.0)) != 0
    }

    pub fn add(&mut self, reg: PhysReg) {
        self.0 |= 1 << reg.0;
    }

    pub fn remove(&mut self, reg: PhysReg) {
        self.0 &= !(1 << reg.0);
    }
}

impl FromIterator<PhysReg> for PhysRegSet {
    fn from_iter<I: IntoIterator<Item = PhysReg>>(iter: I) -> Self {
        let mut retval = Self::empty();
        retval.extend(iter);
        retval
    }
}

impl Extend<PhysReg> for PhysRegSet {
    fn extend<I: IntoIterator<Item = PhysReg>>(&mut self, iter: I) {
        for reg in iter {
            self.add(reg);
        }
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

    pub fn any(reg: VirtReg) -> Self {
        Self::new(reg, UseOperandConstraint::Any, OperandPos::Early)
    }

    pub fn any_reg(reg: VirtReg) -> Self {
        Self::new(reg, UseOperandConstraint::AnyReg, OperandPos::Early)
    }

    pub fn fixed(reg: VirtReg, phys: PhysReg) -> Self {
        Self::new(reg, UseOperandConstraint::Fixed(phys), OperandPos::Early)
    }

    pub fn tied(reg: VirtReg, def_idx: u8) -> Self {
        Self::new(
            reg,
            UseOperandConstraint::TiedToDef(def_idx),
            OperandPos::Early,
        )
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

    pub fn display<M: MachineCore>(&self) -> DisplayUseOperand<'_, M> {
        DisplayUseOperand {
            operand: self,
            _marker: PhantomData,
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

    pub fn any(reg: VirtReg) -> Self {
        Self::new(reg, DefOperandConstraint::Any, OperandPos::Late)
    }

    pub fn any_reg(reg: VirtReg) -> Self {
        Self::new(reg, DefOperandConstraint::AnyReg, OperandPos::Late)
    }

    pub fn fixed(reg: VirtReg, phys: PhysReg) -> Self {
        Self::new(reg, DefOperandConstraint::Fixed(phys), OperandPos::Late)
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

    pub fn display<M: MachineCore>(&self) -> DisplayDefOperand<'_, M> {
        DisplayDefOperand {
            operand: self,
            _marker: PhantomData,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct StackSlotData {
    pub size: u32,
    pub align: u32,
}

impl fmt::Debug for StackSlotData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("StackSlot")
            .field("size", &self.size)
            .field("align", &self.align)
            .finish()
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct StackSlot(u32);
entity_impl!(StackSlot, "!");

#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Instr(u32);
entity_impl!(Instr, "i");

impl Instr {
    pub fn prev(self) -> Self {
        Self(self.0 - 1)
    }

    pub fn next(self) -> Self {
        Self(self.0 + 1)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstrRange {
    pub start: Instr,
    pub end: Instr,
}

impl InstrRange {
    pub fn new(start: Instr, end: Instr) -> Self {
        Self { start, end }
    }

    pub fn is_empty(&self) -> bool {
        self.start >= self.end
    }
}

impl IntoIterator for InstrRange {
    type Item = Instr;
    type IntoIter = InstrRangeIter;

    fn into_iter(self) -> InstrRangeIter {
        InstrRangeIter(self.start.0..self.end.0)
    }
}

pub struct InstrRangeIter(Range<u32>);

impl Iterator for InstrRangeIter {
    type Item = Instr;

    fn next(&mut self) -> Option<Instr> {
        self.0.next().map(Instr)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl DoubleEndedIterator for InstrRangeIter {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(Instr)
    }
}

impl ExactSizeIterator for InstrRangeIter {}

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

#[derive(Clone, Copy, PartialEq, Eq)]
struct ClobberIndex(u32);
entity_impl!(ClobberIndex);

#[derive(Clone)]
pub struct Lir<M: MachineCore> {
    block_instr_ranges: SecondaryMap<Block, (Instr, Instr)>,
    block_params: SecondaryMap<Block, BlockParamData>,
    live_in_regs: Vec<PhysReg>,
    outgoing_block_param_indices: Vec<(u32, u32)>,
    block_param_pool: Vec<VirtReg>,
    instrs: Vec<M::Instr>,
    instr_operands: Vec<InstrOperands>,
    instr_clobbers: Vec<PackedOption<ClobberIndex>>,
    def_pool: Vec<DefOperand>,
    use_pool: Vec<UseOperand>,
    clobbers: PrimaryMap<ClobberIndex, PhysRegSet>,
    stack_slots: PrimaryMap<StackSlot, StackSlotData>,
    vreg_count: u32,
}

impl<M: MachineCore> Lir<M> {
    pub fn all_instrs(&self) -> InstrRange {
        InstrRange::new(Instr(0), Instr(self.instrs.len() as u32))
    }

    pub fn block_instrs(&self, block: Block) -> InstrRange {
        let (start, end) = self.block_instr_ranges[block];
        InstrRange::new(start, end)
    }

    pub fn instr_data(&self, instr: Instr) -> &M::Instr {
        &self.instrs[instr.index()]
    }

    pub fn instr_uses(&self, instr: Instr) -> &[UseOperand] {
        let operands = &self.instr_operands[instr.index()];
        let base = operands.use_base as usize;
        &self.use_pool[base..base + operands.use_len as usize]
    }

    pub fn instr_defs(&self, instr: Instr) -> &[DefOperand] {
        let operands = &self.instr_operands[instr.index()];
        let base = operands.def_base as usize;
        &self.def_pool[base..base + operands.def_len as usize]
    }

    pub fn instr_clobbers(&self, instr: Instr) -> PhysRegSet {
        self.instr_clobbers[instr.index()]
            .expand()
            .map_or(PhysRegSet::empty(), |clobbers| self.clobbers[clobbers])
    }

    pub fn live_in_regs(&self) -> &[PhysReg] {
        &self.live_in_regs
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

    pub fn stack_slot_data(&self, slot: StackSlot) -> StackSlotData {
        self.stack_slots[slot]
    }

    pub fn vreg_count(&self) -> u32 {
        self.vreg_count
    }
}

impl<M: MachineCore> Lir<M> {
    pub fn display_instr(&self, instr: Instr) -> DisplayInstr<'_, M> {
        DisplayInstr { lir: self, instr }
    }

    pub fn display<'a>(&'a self, cfg: &'a BlockCfg, block_order: &'a [Block]) -> Display<'a, M> {
        Display {
            lir: self,
            cfg,
            block_order,
        }
    }
}

pub struct InstrBuilder<'o, 'b, M: MachineCore> {
    builder: &'b mut Builder<'o, M>,
}

impl<M: MachineCore> InstrBuilder<'_, '_, M> {
    pub fn create_vreg(&mut self, class: RegClass) -> VirtReg {
        self.builder.create_vreg(class)
    }

    pub fn copy_vreg(&mut self, dest: VirtReg, src: VirtReg) {
        self.builder.copy_vreg(dest, src)
    }

    pub fn push_instr(
        &mut self,
        data: M::Instr,
        defs: impl IntoIterator<Item = DefOperand>,
        uses: impl IntoIterator<Item = UseOperand>,
        clobbers: PhysRegSet,
    ) {
        self.builder.lir.instrs.push(data);

        let def_base = self.builder.lir.def_pool.len();
        self.builder.lir.def_pool.extend(defs);
        let def_len = self.builder.lir.def_pool.len() - def_base;

        let use_base = self.builder.lir.use_pool.len();
        self.builder.lir.use_pool.extend(uses);
        let use_len = self.builder.lir.use_pool.len() - use_base;

        if cfg!(debug_assertions) {
            for use_op in &self.builder.lir.use_pool[use_base..use_base + use_len] {
                if let UseOperandConstraint::TiedToDef(def_idx) = use_op.constraint() {
                    assert!(
                        (def_idx as usize) < def_len,
                        "use tied to nonexistent def {def_idx}"
                    );
                }
            }
        }

        self.builder.lir.instr_operands.push(InstrOperands {
            def_base: def_base.try_into().unwrap(),
            def_len: def_len.try_into().unwrap(),
            use_base: use_base.try_into().unwrap(),
            use_len: use_len.try_into().unwrap(),
        });

        let clobbers = if !clobbers.is_empty() {
            PackedOption::from(self.builder.lir.clobbers.push(clobbers))
        } else {
            None.into()
        };

        self.builder.lir.instr_clobbers.push(clobbers);

        assert!(self.builder.lir.instrs.len() == self.builder.lir.instr_operands.len());
        assert!(self.builder.lir.instrs.len() == self.builder.lir.instr_clobbers.len());
    }
}

pub struct Builder<'o, M: MachineCore> {
    lir: Lir<M>,
    block_order: &'o [Block],
    vreg_copies: FxHashMap<VirtReg, VirtReg>,
    outgoing_block_param_base: u32,
    cur_block: isize,
}

impl<'o, M: MachineCore> Builder<'o, M> {
    pub fn new(block_order: &'o [Block]) -> Self {
        assert!(!block_order.is_empty());
        Self {
            lir: Lir {
                block_instr_ranges: SecondaryMap::new(),
                block_params: SecondaryMap::new(),
                live_in_regs: Vec::new(),
                outgoing_block_param_indices: Vec::new(),
                block_param_pool: Vec::new(),
                instrs: Vec::new(),
                instr_operands: Vec::new(),
                instr_clobbers: Vec::new(),
                def_pool: Vec::new(),
                use_pool: Vec::new(),
                clobbers: PrimaryMap::new(),
                stack_slots: PrimaryMap::new(),
                vreg_count: 0,
            },
            block_order,
            vreg_copies: FxHashMap::default(),
            outgoing_block_param_base: 0,
            cur_block: block_order.len() as isize,
        }
    }

    pub fn advance_block(&mut self) -> Option<Block> {
        assert!(self.cur_block >= 0);

        let next_instr = self.next_instr();

        if self.cur_block < self.block_order.len() as isize {
            // Finish recording outgoing block parameters.
            let block = self.cur_block();
            let outgoing_block_param_base = self.outgoing_block_param_base;
            let outgoing_block_param_end: u32 = self
                .lir
                .outgoing_block_param_indices
                .len()
                .try_into()
                .unwrap();
            self.lir.block_params[block].outgoing_base = outgoing_block_param_base;
            self.lir.block_params[block].outgoing_len = (outgoing_block_param_end
                - outgoing_block_param_base)
                .try_into()
                .unwrap();
            self.outgoing_block_param_base = outgoing_block_param_end;

            // Note: we always want `.0` to be greater than `.1` for every block, so that when we
            // reverse everything later we'll end up with `.0 < .1`.
            self.lir.block_instr_ranges[block].0 = next_instr;
        }

        self.cur_block -= 1;
        if self.cur_block >= 0 {
            let block = self.cur_block();
            self.lir.block_instr_ranges[block].1 = next_instr;
            Some(block)
        } else {
            None
        }
    }

    pub fn create_vreg(&mut self, class: RegClass) -> VirtReg {
        let num = self.lir.vreg_count;
        self.lir.vreg_count += 1;
        VirtReg::new(num, class)
    }

    pub fn copy_vreg(&mut self, dest: VirtReg, src: VirtReg) {
        let src = resolve_vreg_copy(&mut self.vreg_copies, src).unwrap_or(src);
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

        let prev = self.vreg_copies.insert(dest, src);
        if let Some(prev) = prev {
            panic!(
                "vreg {} already copied from {}",
                dest.reg_num(),
                prev.reg_num()
            );
        }
    }

    pub fn create_stack_slot(&mut self, data: StackSlotData) -> StackSlot {
        self.lir.stack_slots.push(data)
    }

    pub fn set_live_in_regs(&mut self, live_in_regs: Vec<PhysReg>) {
        self.lir.live_in_regs = live_in_regs;
    }

    pub fn set_incoming_block_params(&mut self, params: impl IntoIterator<Item = VirtReg>) {
        let block = self.cur_block();
        let base = self.lir.block_param_pool.len();
        self.lir.block_param_pool.extend(params);
        let len = self.lir.block_param_pool.len() - base;
        self.lir.block_params[block].incoming_base = base.try_into().unwrap();
        self.lir.block_params[block].incoming_len = len.try_into().unwrap();
    }

    pub fn add_succ_outgoing_block_params(
        &mut self,
        outgoing_params: impl IntoIterator<Item = VirtReg>,
    ) {
        let base = self.lir.block_param_pool.len();
        self.lir.block_param_pool.extend(outgoing_params);
        let len = self.lir.block_param_pool.len() - base;
        self.lir
            .outgoing_block_param_indices
            .push((base.try_into().unwrap(), len.try_into().unwrap()));
    }

    pub fn build_instrs<R>(&mut self, f: impl FnOnce(InstrBuilder<'o, '_, M>) -> R) -> R {
        let orig_len = self.lir.instrs.len();
        let retval = f(InstrBuilder { builder: self });
        // We're building the LIR in reverse order, but the sequence of instructions should end up
        // in insertion order, so reverse this tail sequence we've just built.
        self.reverse_instrs(orig_len..self.lir.instrs.len());
        retval
    }

    pub fn finish(mut self) -> Lir<M> {
        assert!(self.cur_block == -1);
        assert!(self.lir.block_params(self.block_order[0]).len() == self.lir.live_in_regs.len());

        let instr_count = self.lir.instrs.len();

        // We've built everything backwards, so reverse things now.
        self.reverse_instrs(0..instr_count);
        let instr_count: u32 = instr_count.try_into().unwrap();
        for (block_start, block_end) in self.lir.block_instr_ranges.values_mut() {
            *block_start = Instr::from_u32(instr_count - block_start.as_u32());
            *block_end = Instr::from_u32(instr_count - block_end.as_u32());
            assert!(*block_start <= *block_end);
        }

        self.propagate_vreg_copies();

        self.lir
    }

    fn cur_block(&self) -> Block {
        self.block_order[self.cur_block as usize]
    }

    fn next_instr(&self) -> Instr {
        Instr::new(self.lir.instrs.len())
    }

    fn reverse_instrs(&mut self, range: Range<usize>) {
        self.lir.instrs[range.clone()].reverse();
        self.lir.instr_operands[range.clone()].reverse();
        self.lir.instr_clobbers[range].reverse();
    }

    fn propagate_vreg_copies(&mut self) {
        for vreg_use in &mut self.lir.use_pool {
            if let Some(def) = resolve_vreg_copy(&mut self.vreg_copies, vreg_use.reg) {
                vreg_use.reg = def;
            }
        }

        // Replace any outgoing block parameter values that are copies.
        for &block in self.block_order {
            let params = &self.lir.block_params[block];
            let outgoing_base = params.outgoing_base as usize;
            let outgoing_len = params.outgoing_len as usize;

            for &(succ_param_base, succ_param_len) in
                &self.lir.outgoing_block_param_indices[outgoing_base..outgoing_base + outgoing_len]
            {
                let succ_param_base = succ_param_base as usize;
                let succ_param_len = succ_param_len as usize;

                for param in &mut self.lir.block_param_pool
                    [succ_param_base..succ_param_base + succ_param_len]
                {
                    if let Some(def) = resolve_vreg_copy(&mut self.vreg_copies, *param) {
                        *param = def;
                    }
                }
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

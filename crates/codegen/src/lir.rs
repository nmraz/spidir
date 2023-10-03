use alloc::vec::Vec;
use core::ops::Range;

use cranelift_entity::{entity_impl, PrimaryMap, SecondaryMap};

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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Instr(u32);
entity_impl!(Instr);

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

pub struct Lir<I> {
    block_instr_ranges: SecondaryMap<Block, (Instr, Instr)>,
    block_params: SecondaryMap<Block, (u32, u32)>,
    block_param_pool: Vec<VirtReg>,
    instrs: PrimaryMap<Instr, I>,
    instr_operands: SecondaryMap<Instr, InstrOperands>,
    def_pool: Vec<DefOperand>,
    use_pool: Vec<UseOperand>,
}

impl<I> Lir<I> {
    pub fn block_instrs(&self, block: Block) -> InstrRange {
        let (start, end) = self.block_instr_ranges[block];
        InstrRange::new(start, end)
    }

    pub fn instr_data(&self, instr: Instr) -> &I {
        &self.instrs[instr]
    }

    pub fn instr_uses(&self, instr: Instr) -> &[UseOperand] {
        let operands = &self.instr_operands[instr];
        &self.use_pool[operands.use_base as usize..operands.use_count as usize]
    }

    pub fn instr_defs(&self, instr: Instr) -> &[DefOperand] {
        let operands = &self.instr_operands[instr];
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

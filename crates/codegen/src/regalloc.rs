use core::fmt;

use alloc::vec::Vec;

use cranelift_entity::{entity_impl, Keys, PrimaryMap, SecondaryMap};

use crate::{
    cfg::{Block, CfgContext},
    lir::{Instr, InstrRange, Lir, MemLayout, PhysReg, PhysRegSet},
    machine::{MachineCore, MachineRegalloc},
};

use self::context::RegAllocContext;

mod conflict;
mod context;
mod core_loop;
mod display;
mod live_set;
mod liveness;
mod parallel_copy;
mod reify;
mod types;
mod utils;
mod virt_reg_set;

pub use display::DisplayAssignment;
pub use types::{AssignmentCopy, TaggedAssignmentCopy};

#[derive(Debug, Clone, Copy)]
pub enum RegallocError {
    OutOfRegisters(Instr),
}

impl fmt::Display for RegallocError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegallocError::OutOfRegisters(instr) => {
                write!(f, "out of registers at instruction {instr}")
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SpillSlot(u32);
entity_impl!(SpillSlot, "spill");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum OperandAssignment {
    Reg(PhysReg),
    Spill(SpillSlot),
}

impl OperandAssignment {
    pub fn is_spill(self) -> bool {
        matches!(self, Self::Spill(..))
    }

    pub fn is_reg(&self) -> bool {
        matches!(self, Self::Reg(..))
    }

    pub fn as_reg(&self) -> Option<PhysReg> {
        match self {
            &Self::Reg(reg) => Some(reg),
            _ => None,
        }
    }

    pub fn as_spill(&self) -> Option<SpillSlot> {
        match self {
            &Self::Spill(spill) => Some(spill),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Default)]
struct InstrAssignmentData {
    def_base: u32,
    def_len: u16,
    use_base: u32,
    use_len: u16,
}

pub struct Assignment {
    spill_slots: PrimaryMap<SpillSlot, MemLayout>,
    instr_assignments: SecondaryMap<Instr, InstrAssignmentData>,
    operand_assignment_pool: Vec<OperandAssignment>,
    copies: Vec<TaggedAssignmentCopy>,
}

impl Assignment {
    pub fn spill_slots(&self) -> Keys<SpillSlot> {
        self.spill_slots.keys()
    }

    pub fn copies(&self) -> &[TaggedAssignmentCopy] {
        &self.copies
    }

    pub fn spill_slot_layout(&self, slot: SpillSlot) -> MemLayout {
        self.spill_slots[slot]
    }

    pub fn instr_def_assignments(&self, instr: Instr) -> &[OperandAssignment] {
        let assignment_data = &self.instr_assignments[instr];
        let base = assignment_data.def_base as usize;
        let len = assignment_data.def_len as usize;
        &self.operand_assignment_pool[base..base + len]
    }

    pub fn instr_use_assignments(&self, instr: Instr) -> &[OperandAssignment] {
        let assignment_data = &self.instr_assignments[instr];
        let base = assignment_data.use_base as usize;
        let len = assignment_data.use_len as usize;
        &self.operand_assignment_pool[base..base + len]
    }

    pub fn copy_tracker_from(&self, instr: Instr) -> AssignmentCopyTracker<'_> {
        let copy_idx = self
            .copies
            .partition_point(|copy| copy.instr < instr)
            .try_into()
            .unwrap();
        AssignmentCopyTracker {
            copies: &self.copies,
            copy_idx,
        }
    }

    pub fn instrs_and_copies(&self, range: InstrRange) -> AssignmentInstrIter<'_> {
        AssignmentInstrIter {
            instr: range.start,
            instr_end: range.end,
            copies: self.copy_tracker_from(range.start),
        }
    }

    pub fn compute_global_clobbers<M: MachineCore>(&self, lir: &Lir<M>) -> PhysRegSet {
        let mut clobbers = PhysRegSet::empty();

        for instr in lir.all_instrs() {
            clobbers |= &lir.instr_clobbers(instr);

            for def in self.instr_def_assignments(instr) {
                if let &OperandAssignment::Reg(reg) = def {
                    clobbers.add(reg);
                }
            }
        }

        for copy in &self.copies {
            if let &OperandAssignment::Reg(reg) = &copy.copy.to {
                clobbers.add(reg);
            }
        }

        clobbers
    }

    pub fn display<'a, M: MachineCore>(
        &'a self,
        block_order: &'a [Block],
        lir: &'a Lir<M>,
    ) -> DisplayAssignment<'a, M> {
        DisplayAssignment {
            assignment: self,
            lir,
            block_order,
        }
    }
}

pub struct AssignmentCopyTracker<'a> {
    copies: &'a [TaggedAssignmentCopy],
    copy_idx: u32,
}

impl<'a> AssignmentCopyTracker<'a> {
    pub fn next_copy_for(&mut self, instr: Instr) -> Option<(u32, AssignmentCopy)> {
        self.copies
            .get(self.copy_idx as usize)
            .filter(|copy| copy.instr == instr)
            .map(|copy| {
                let ret = (self.copy_idx, copy.copy);
                self.copy_idx += 1;
                ret
            })
    }
}

pub enum InstrOrCopy {
    Instr(Instr),
    Copy(AssignmentCopy),
}

pub struct AssignmentInstrIter<'a> {
    instr: Instr,
    instr_end: Instr,
    copies: AssignmentCopyTracker<'a>,
}

impl<'a> Iterator for AssignmentInstrIter<'a> {
    type Item = InstrOrCopy;

    fn next(&mut self) -> Option<InstrOrCopy> {
        let instr = self.instr;

        if instr >= self.instr_end {
            return None;
        }

        if let Some((_, copy)) = self.copies.next_copy_for(instr) {
            // This copy pertains to the current instruction, so return it before the instruction
            // itself.
            return Some(InstrOrCopy::Copy(copy));
        }

        self.instr = instr.next();
        Some(InstrOrCopy::Instr(instr))
    }
}

pub fn run<M: MachineRegalloc>(
    lir: &Lir<M>,
    cfg_ctx: &CfgContext,
    machine: &M,
) -> Result<Assignment, RegallocError> {
    let mut ctx = RegAllocContext::new(lir, cfg_ctx, machine);
    ctx.compute_liveness();
    ctx.build_live_sets();
    ctx.run_core_loop()?;

    let assignment = ctx.reify();

    // Dump things as they were *after* reification, since that can change some things as well.
    ctx.dump();

    Ok(assignment)
}

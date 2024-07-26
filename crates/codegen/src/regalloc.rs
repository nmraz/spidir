use alloc::vec::Vec;

use cranelift_entity::{entity_impl, Keys, PrimaryMap, SecondaryMap};
use types::TaggedAssignmentCopy;

use crate::{
    cfg::{Block, CfgContext},
    lir::{Instr, Lir, MemLayout, PhysReg, PhysRegSet},
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
pub use types::AssignmentCopy;

#[derive(Debug, Clone)]
pub enum Error {
    OutOfRegisters(Instr),
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

    pub fn copies(&self) -> AssignmentCopyIter<'_> {
        AssignmentCopyIter {
            copies: &self.copies,
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

pub struct AssignmentCopyIter<'a> {
    copies: &'a [TaggedAssignmentCopy],
}

impl<'a> AssignmentCopyIter<'a> {
    pub fn consume_copies_for(
        &mut self,
        instr: Instr,
    ) -> impl Iterator<Item = AssignmentCopy> + 'a {
        let len = self
            .copies
            .iter()
            .position(|copy| copy.instr != instr)
            .unwrap_or(self.copies.len());

        let (for_instr, after_instr) = self.copies.split_at(len);
        self.copies = after_instr;

        for_instr.iter().map(move |tagged_copy| tagged_copy.copy)
    }
}

pub fn run<M: MachineRegalloc>(
    lir: &Lir<M>,
    cfg_ctx: &CfgContext,
    machine: &M,
) -> Result<Assignment, Error> {
    let mut ctx = RegAllocContext::new(lir, cfg_ctx, machine);
    ctx.compute_liveness();
    ctx.build_live_sets();
    ctx.run_core_loop()?;

    let assignment = ctx.reify();

    // Dump things as they were *after* reification, since that can change some things as well.
    ctx.dump();

    Ok(assignment)
}

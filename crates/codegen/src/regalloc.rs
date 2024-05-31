use alloc::vec::Vec;

use cranelift_entity::{entity_impl, PrimaryMap, SecondaryMap};

use crate::{
    cfg::{Block, CfgContext},
    lir::{Instr, Lir, MemLayout, PhysReg},
    machine::{MachineCore, MachineRegalloc},
};

use self::{context::RegAllocContext, types::ParallelCopies};

mod conflict;
mod context;
mod core_loop;
mod display;
mod live_set;
mod liveness;
mod reify;
mod types;
mod utils;
mod virt_reg_set;

pub use display::DisplayAssignment;

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
    // TODO: Replace this with fully-resolved single copies once we get there.
    copies: ParallelCopies,
}

impl Assignment {
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

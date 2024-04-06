use core::iter;

use alloc::vec::Vec;

use cranelift_entity::{entity_impl, packed_option::ReservedValue, PrimaryMap, SecondaryMap};

use crate::{
    cfg::CfgContext,
    lir::{Instr, Lir, PhysReg},
    machine::MachineCore,
};

use self::context::RegAllocContext;

mod conflict;
mod context;
mod core_loop;
mod live_set;
mod liveness;
mod reify;
mod types;
mod utils;

#[derive(Debug, Clone)]
pub enum Error {
    OutOfRegisters(Instr),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct SpillSlotData {
    pub size: u32,
    pub align: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SpillSlot(u32);
entity_impl!(SpillSlot, "spill");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum OperandAssignment {
    Reg(PhysReg),
    Spill(SpillSlot),
}

#[derive(Clone, Copy, Default)]
struct InstrAssignmentData {
    def_base: u32,
    def_len: u16,
    use_base: u32,
    use_len: u16,
}

pub struct Assignment {
    spill_slots: PrimaryMap<SpillSlot, SpillSlotData>,
    instr_assignments: SecondaryMap<Instr, InstrAssignmentData>,
    operand_assignment_pool: Vec<OperandAssignment>,
}

impl Assignment {
    fn empty_for_lir<M: MachineCore>(lir: &Lir<M>) -> Self {
        let mut assignment = Assignment {
            spill_slots: PrimaryMap::new(),
            instr_assignments: SecondaryMap::new(),
            operand_assignment_pool: Vec::new(),
        };

        for instr in lir.all_instrs() {
            let def_len = lir.instr_defs(instr).len();
            let use_len = lir.instr_uses(instr).len();
            let def_base = assignment.operand_assignment_pool.len();
            let use_base = def_base + def_len;

            assignment.operand_assignment_pool.extend(
                iter::repeat(OperandAssignment::Reg(PhysReg::reserved_value()))
                    .take(def_len + use_len),
            );

            assignment.instr_assignments[instr] = InstrAssignmentData {
                def_base: def_base.try_into().unwrap(),
                def_len: def_len.try_into().unwrap(),
                use_base: use_base.try_into().unwrap(),
                use_len: use_len.try_into().unwrap(),
            };
        }

        assignment
    }

    fn assign_instr_def(&mut self, instr: Instr, idx: usize, assignment: OperandAssignment) {
        let assignment_data = &self.instr_assignments[instr];
        let base = assignment_data.def_base as usize;
        let len = assignment_data.def_len as usize;

        assert!(idx < len);
        self.operand_assignment_pool[base + idx] = assignment;
    }

    fn assign_instr_use(&mut self, instr: Instr, idx: usize, assignment: OperandAssignment) {
        let assignment_data = &self.instr_assignments[instr];
        let base = assignment_data.use_base as usize;
        let len = assignment_data.use_len as usize;

        assert!(idx < len);
        self.operand_assignment_pool[base + idx] = assignment;
    }
}

impl Assignment {
    pub fn spill_slot_data(&self, slot: SpillSlot) -> SpillSlotData {
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
}

pub fn run<M: MachineCore>(
    lir: &Lir<M>,
    cfg_ctx: &CfgContext,
    machine: &M,
) -> Result<Assignment, Error> {
    let mut ctx = RegAllocContext::new(lir, cfg_ctx, machine);
    ctx.compute_liveness();
    ctx.build_live_sets();
    ctx.run_core_loop()?;
    ctx.dump();
    Ok(ctx.reify())
}

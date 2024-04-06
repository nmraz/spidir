use cranelift_entity::packed_option::ReservedValue;
use smallvec::SmallVec;

use crate::machine::MachineCore;

use super::{context::RegAllocContext, types::LiveRange, Assignment, OperandAssignment, SpillSlot};

impl<M: MachineCore> RegAllocContext<'_, M> {
    pub fn reify(&mut self) -> Assignment {
        let mut assignment = Assignment::empty_for_lir(self.lir);
        let mut vreg_ranges = SmallVec::<[LiveRange; 4]>::new();

        for vreg in self.vreg_ranges.keys() {
            vreg_ranges.clone_from(&self.vreg_ranges[vreg]);

            vreg_ranges.sort_unstable_by_key(|&range| {
                let range_data = &self.live_ranges[range];
                let fragment_data = &self.live_set_fragments[range_data.fragment];

                // Allow ranges to overlap, but when a spilled and a non-spilled range start at the
                // same point, make sure the non-spilled range comes first.
                ((range_data.prog_range.start.index() as u64) << 1)
                    | (fragment_data.assignment.is_some() as u64)
            });

            // Resolve all defs first so tied uses are easier.
            for &range in vreg_ranges.iter() {
                let range_assignment = self.get_range_assignment(range);
                for &range_instr in &self.live_ranges[range].instrs {
                    if !range_instr.is_def() {
                        continue;
                    }

                    let instr = range_instr.instr();
                    // An instruction can only define a vreg once.
                    let (i, _def_op) = self
                        .lir
                        .instr_defs(instr)
                        .iter()
                        .enumerate()
                        .find(|(_, def_op)| def_op.reg().reg_num() == vreg)
                        .unwrap();

                    // TODO: Fixed operands.
                    assignment.assign_instr_def(instr, i, range_assignment);
                }
            }

            // Now that defs are resolved, do the same for uses.
            for &range in vreg_ranges.iter() {
                let range_assignment = self.get_range_assignment(range);
                for &range_instr in &self.live_ranges[range].instrs {
                    if range_instr.is_def() {
                        continue;
                    }

                    let instr = range_instr.instr();
                    // An instruction may use a vreg multiple times.
                    for (i, use_op) in self.lir.instr_uses(instr).iter().enumerate() {
                        if use_op.reg().reg_num() != vreg {
                            continue;
                        }

                        // TODO: Fixed/tied operands.
                        assignment.assign_instr_use(instr, i, range_assignment);
                    }
                }
            }
        }

        assignment
    }

    fn get_range_assignment(&self, range: LiveRange) -> OperandAssignment {
        match self.live_set_fragments[self.live_ranges[range].fragment]
            .assignment
            .expand()
        {
            Some(preg) => OperandAssignment::Reg(preg),
            None => {
                // TODO: Get spill slot.
                OperandAssignment::Spill(SpillSlot::reserved_value())
            }
        }
    }
}

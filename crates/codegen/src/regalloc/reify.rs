use core::iter;

use alloc::vec::Vec;

use cranelift_entity::{packed_option::ReservedValue, PrimaryMap, SecondaryMap};

use crate::{
    lir::{DefOperandConstraint, Instr, Lir, PhysReg, UseOperandConstraint},
    machine::MachineCore,
};

use super::{
    context::RegAllocContext, types::LiveRange, Assignment, InstrAssignmentData, OperandAssignment,
    SpillSlot,
};

impl<M: MachineCore> RegAllocContext<'_, M> {
    pub fn reify(&mut self) -> Assignment {
        let mut assignment = Assignment::empty_for_lir(self.lir);

        // Assign all fixed operands first, since dead fixed outputs will not even have associated
        // live ranges.
        self.assign_fixed_operands(&mut assignment);

        // Now, assign remaining operands based on live range allocations.
        self.assign_allocated_operands(&mut assignment);

        if cfg!(debug_assertions) {
            assignment.verify_all_assigned();
        }

        assignment
    }

    fn assign_allocated_operands(&mut self, assignment: &mut Assignment) {
        for vreg in self.vreg_ranges.keys() {
            self.vreg_ranges[vreg].sort_unstable_by_key(|&range| {
                let range_data = &self.live_ranges[range];
                let fragment_data = &self.live_set_fragments[range_data.fragment];

                // Allow ranges to overlap, but when a spilled and a non-spilled range start at the
                // same point, make sure the non-spilled range comes first.
                ((range_data.prog_range.start.index() as u64) << 1)
                    | (fragment_data.assignment.is_some() as u64)
            });

            // Start by resolving all defs so tied uses are easier.
            for &range in self.vreg_ranges[vreg].iter() {
                let range_assignment = self.get_range_assignment(range);
                for &range_instr in &self.live_ranges[range].instrs {
                    if !range_instr.is_def() {
                        continue;
                    }

                    let instr = range_instr.instr();
                    // An instruction can only define a vreg once.
                    let (i, def_op) = self
                        .lir
                        .instr_defs(instr)
                        .iter()
                        .enumerate()
                        .find(|(_, def_op)| def_op.reg().reg_num() == vreg)
                        .unwrap();

                    let op_assignment = match def_op.constraint() {
                        DefOperandConstraint::Fixed(_) => {
                            // Should be handled by `assign_fixed_operands`.
                            continue;
                        }
                        _ => range_assignment,
                    };

                    assignment.assign_instr_def(instr, i, op_assignment);
                }
            }
        }

        // Now that all defs are resolved, do the same for uses.
        for vreg in self.vreg_ranges.keys() {
            for &range in self.vreg_ranges[vreg].iter() {
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

                        let op_assignment = match use_op.constraint() {
                            UseOperandConstraint::Fixed(_) => {
                                // Should be handled by `assign_fixed_operands`.
                                continue;
                            }
                            UseOperandConstraint::TiedToDef(i) => {
                                assignment.instr_def_assignments(instr)[i as usize]
                            }
                            _ => range_assignment,
                        };

                        assignment.assign_instr_use(instr, i, op_assignment);
                    }
                }
            }
        }
    }

    fn assign_fixed_operands(&self, assignment: &mut Assignment) {
        for instr in self.lir.all_instrs() {
            for (i, use_op) in self.lir.instr_uses(instr).iter().enumerate() {
                if let UseOperandConstraint::Fixed(preg) = use_op.constraint() {
                    assignment.assign_instr_use(instr, i, OperandAssignment::Reg(preg));
                }
            }

            for (i, def_op) in self.lir.instr_defs(instr).iter().enumerate() {
                if let DefOperandConstraint::Fixed(preg) = def_op.constraint() {
                    assignment.assign_instr_def(instr, i, OperandAssignment::Reg(preg));
                }
            }
        }
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

impl Assignment {
    fn empty_for_lir<M: MachineCore>(lir: &Lir<M>) -> Self {
        let instr_count = lir.all_instrs().end.as_u32() as usize;

        let mut assignment = Assignment {
            spill_slots: PrimaryMap::new(),
            instr_assignments: SecondaryMap::with_capacity(instr_count),
            // Estimate: every instruction has at least one use or def. This isn't actually true,
            // but should usually be compensated for by the fact that many instructions have more
            // than one operand.
            operand_assignment_pool: Vec::with_capacity(instr_count),
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

    fn verify_all_assigned(&self) {
        for instr in self.instr_assignments.keys() {
            for (i, def_op) in self.instr_def_assignments(instr).iter().enumerate() {
                assert!(
                    def_op != &OperandAssignment::Reg(PhysReg::reserved_value()),
                    "instr {instr} def {i} unset"
                );
            }
            for (i, use_op) in self.instr_use_assignments(instr).iter().enumerate() {
                assert!(
                    use_op != &OperandAssignment::Reg(PhysReg::reserved_value()),
                    "instr {instr} use {i} unset"
                );
            }
        }
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

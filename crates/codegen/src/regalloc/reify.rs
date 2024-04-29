use core::iter;

use alloc::vec::Vec;

use cranelift_entity::{packed_option::ReservedValue, PrimaryMap, SecondaryMap};

use crate::{
    lir::{DefOperandConstraint, Instr, Lir, MemLayout, PhysReg, UseOperandConstraint},
    machine::{MachineCore, MachineRegalloc},
    regalloc::types::InstrSlot,
};

use super::{
    context::RegAllocContext,
    types::{AssignmentCopies, AssignmentCopy, AssignmentCopyPhase, LiveRange},
    Assignment, InstrAssignmentData, OperandAssignment, SpillSlot,
};

fn record_assignment_copy(
    copies: &mut AssignmentCopies,
    instr: Instr,
    phase: AssignmentCopyPhase,
    from: OperandAssignment,
    to: OperandAssignment,
) {
    if from != to {
        copies.push(AssignmentCopy {
            instr,
            phase,
            from,
            to,
        });
    }
}

impl<M: MachineRegalloc> RegAllocContext<'_, M> {
    pub fn reify(&mut self) -> Assignment {
        let mut assignment = Assignment::empty_for_lir(self.lir);
        let mut copies = AssignmentCopies::new();

        self.assign_spill_slots(&mut assignment);

        // Assign all fixed operands first, since dead fixed outputs will not even have associated
        // live ranges.
        self.assign_fixed_operands(&mut assignment);

        // Now, extract everything else we need (operand assignments, copies) out of the live range
        // assignments.
        self.sort_vreg_ranges();
        self.assign_allocated_operands(&mut assignment, &mut copies);
        self.collect_cross_fragment_copies(&mut copies);

        copies.sort_unstable_by_key(|copy| (copy.instr, copy.phase));

        assignment.copies = copies;

        if cfg!(debug_assertions) {
            assignment.verify_all_assigned();
        }

        assignment
    }

    fn collect_cross_fragment_copies(&self, copies: &mut AssignmentCopies) {
        for vreg_ranges in self.vreg_ranges.values() {
            let mut last_spill: Option<(LiveRange, SpillSlot)> = None;
            let mut prev_range: Option<(LiveRange, OperandAssignment)> = None;

            for &range in vreg_ranges {
                let range_assignment = self.get_range_assignment(range);
                let range_instrs = &self.live_ranges[range].instrs;
                let prog_range = self.live_ranges[range].prog_range;

                // Check if we have any spills/reloads to perform.
                if let Some((spill_range, spill_slot)) = last_spill {
                    let spill_prog_range = self.live_ranges[spill_range].prog_range;

                    // Note: we expect to start _strictly_ after the current spill because register
                    // ranges should be sorted before spill ranges starting at the same point.
                    debug_assert!(prog_range.start > spill_prog_range.start);

                    // If we have a later range intersecting the current spill range, it should be a
                    // small spill/reload connector; record the copy for that now.
                    if prog_range.end <= spill_prog_range.end {
                        // All such spill/reload connectors should be small ranges requiring
                        // registers and covering a single instruction.
                        debug_assert!(range_assignment.is_reg());
                        debug_assert!(range_instrs.len() == 1);

                        let range_instr = range_instrs[0];
                        let instr = range_instr.instr();
                        // We should never have inserted spills/reloads for non-`needs_reg`
                        // operands.
                        debug_assert!(range_instr.needs_reg());

                        if range_instr.is_def() {
                            // Spill
                            record_assignment_copy(
                                copies,
                                instr.next(),
                                AssignmentCopyPhase::PrevInstrCleanup,
                                range_assignment,
                                OperandAssignment::Spill(spill_slot),
                            );
                        } else {
                            // Reload
                            record_assignment_copy(
                                copies,
                                instr,
                                AssignmentCopyPhase::CrossFragment,
                                OperandAssignment::Spill(spill_slot),
                                range_assignment,
                            );
                        };
                    } else {
                        // We've run off the end of the spill range, reset things.
                        last_spill = None;
                    }
                }

                if let OperandAssignment::Spill(spill_slot) = range_assignment {
                    last_spill = Some((range, spill_slot));
                }

                // Stitch together touching live ranges.
                if let Some((prev_range, prev_assignment)) = prev_range {
                    let prev_prog_range = self.live_ranges[prev_range].prog_range;
                    if prev_prog_range.end == prog_range.start {
                        let boundary = prog_range.start;
                        let instr = boundary.instr();

                        // Adjacent ranges belonging to the same vreg should only happen because of
                        // basic block boundaries or splits, and both those cases use the `Before`
                        // slot.
                        debug_assert!(boundary.slot() == InstrSlot::Before);

                        // Cross-block copies need to be handled more delicately, so do only
                        // intra-block copies this way.
                        if !is_block_header(self.lir, instr) {
                            record_assignment_copy(
                                copies,
                                instr,
                                AssignmentCopyPhase::CrossFragment,
                                prev_assignment,
                                range_assignment,
                            );
                        }
                    }
                }

                prev_range = Some((range, range_assignment));
            }
        }
    }

    fn assign_allocated_operands(
        &self,
        assignment: &mut Assignment,
        copies: &mut AssignmentCopies,
    ) {
        // First pass: sort each register's live ranges and resolve all instruction def operands to
        // the correct physical registers.
        for (vreg, vreg_ranges) in self.vreg_ranges.iter() {
            for &range in vreg_ranges {
                let range_assignment = self.get_range_assignment(range);

                // Assign defs based on range data.
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

                    if let DefOperandConstraint::Fixed(preg) = def_op.constraint() {
                        // The assignment itself should have been handled by `assign_fixed_operands`.
                        // Just make sure to record the appropriate copy.
                        record_assignment_copy(
                            copies,
                            instr.next(),
                            AssignmentCopyPhase::PrevInstrCleanup,
                            OperandAssignment::Reg(preg),
                            range_assignment,
                        );
                    } else {
                        assignment.assign_instr_def(instr, i, range_assignment);
                    };
                }
            }
        }

        // Second pass: resolve all uses now that we have all defs and can easily determine tied
        // operands.
        for (vreg, vreg_ranges) in self.vreg_ranges.iter() {
            for &range in vreg_ranges {
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

                        match use_op.constraint() {
                            UseOperandConstraint::Fixed(preg) => {
                                // The assignment itself should have been handled by
                                // `assign_fixed_operands`. Just make sure to record the appropriate
                                // copy.
                                record_assignment_copy(
                                    copies,
                                    instr,
                                    AssignmentCopyPhase::InstrSetup,
                                    range_assignment,
                                    OperandAssignment::Reg(preg),
                                );
                            }
                            UseOperandConstraint::TiedToDef(j) => {
                                let def_assignment =
                                    assignment.instr_def_assignments(instr)[j as usize];

                                record_assignment_copy(
                                    copies,
                                    instr,
                                    AssignmentCopyPhase::InstrSetup,
                                    range_assignment,
                                    def_assignment,
                                );
                                assignment.assign_instr_use(instr, i, def_assignment);
                            }
                            _ => {
                                assignment.assign_instr_use(instr, i, range_assignment);
                            }
                        };
                    }
                }
            }
        }
    }

    fn sort_vreg_ranges(&mut self) {
        for ranges in self.vreg_ranges.values_mut() {
            ranges.sort_unstable_by_key(|&range| {
                let range_data = &self.live_ranges[range];
                let fragment_data = &self.live_set_fragments[range_data.fragment];

                // Allow ranges to overlap, but when a spilled and a non-spilled range start at the
                // same point, make sure the non-spilled range comes first.
                ((range_data.prog_range.start.index() as u64) << 1)
                    | (fragment_data.assignment.is_none() as u64)
            });
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

    fn assign_spill_slots(&mut self, assignment: &mut Assignment) {
        for live_set_data in self.live_sets.values_mut() {
            // TODO: Use the hulls to share spill slots where possible.
            if live_set_data.spill_hull.is_some() {
                let spill_slot = assignment
                    .create_spill_slot(self.machine.reg_class_spill_layout(live_set_data.class));
                live_set_data.spill_slot = spill_slot.into();
            }
        }
    }

    fn get_range_assignment(&self, range: LiveRange) -> OperandAssignment {
        let fragment_data = &self.live_set_fragments[self.live_ranges[range].fragment];
        match fragment_data.assignment.expand() {
            Some(preg) => OperandAssignment::Reg(preg),
            None => {
                let live_set_data = &self.live_sets[fragment_data.live_set];

                if cfg!(debug_assertions) {
                    let prog_range = self.live_ranges[range].prog_range;
                    let spill_hull = live_set_data
                        .spill_hull
                        .expect("non-reg live range should be spilled");
                    assert!(
                        spill_hull.contains(prog_range),
                        "unassigned live range {:?} not contained in live-set spill hull {:?}",
                        prog_range,
                        spill_hull
                    );
                }

                OperandAssignment::Spill(self.live_sets[fragment_data.live_set].spill_slot.unwrap())
            }
        }
    }
}

impl Assignment {
    fn empty_for_lir<M: MachineRegalloc>(lir: &Lir<M>) -> Self {
        let instr_count = lir.all_instrs().end.as_u32() as usize;

        let mut assignment = Assignment {
            spill_slots: PrimaryMap::new(),
            instr_assignments: SecondaryMap::with_capacity(instr_count),
            // Estimate: every instruction has at least one use or def. This isn't actually true,
            // but should usually be compensated for by the fact that many instructions have more
            // than one operand.
            operand_assignment_pool: Vec::with_capacity(instr_count),
            copies: AssignmentCopies::new(),
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

    fn create_spill_slot(&mut self, layout: MemLayout) -> SpillSlot {
        self.spill_slots.push(layout)
    }
}

fn is_block_header<M: MachineCore>(lir: &Lir<M>, instr: Instr) -> bool {
    instr == lir.block_instrs(lir.instr_block(instr)).start
}

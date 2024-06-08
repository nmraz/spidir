use core::iter;

use alloc::vec::Vec;

use cranelift_entity::{packed_option::ReservedValue, EntityRef, PrimaryMap, SecondaryMap};
use fx_utils::FxHashMap;
use log::trace;

use crate::{
    cfg::Block,
    lir::{DefOperandConstraint, Instr, Lir, MemLayout, PhysReg, UseOperandConstraint, VirtRegNum},
    machine::{MachineCore, MachineRegalloc},
    regalloc::types::InstrSlot,
};

use super::{
    context::RegAllocContext,
    parallel_copy,
    types::{
        LiveRange, ParallelCopies, ParallelCopy, ParallelCopyPhase, ProgramPoint,
        TaggedAssignmentCopies, TaggedAssignmentCopy,
    },
    Assignment, InstrAssignmentData, OperandAssignment, SpillSlot,
};

// Every block parameter is uniquely identified by its destination vreg because everything is in
// SSA. To uniquely identify a complete "from -> to" pair, all we need in addition is the relevant
// source block.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct BlockParamEdgeKey {
    from_block: Block,
    to_vreg: VirtRegNum,
}

type BlockParamOutMap = FxHashMap<BlockParamEdgeKey, OperandAssignment>;

struct BlockParamIn {
    block: Block,
    vreg: VirtRegNum,
    assignment: OperandAssignment,
}

type BlockParamIns = Vec<BlockParamIn>;

fn record_parallel_copy(
    copies: &mut ParallelCopies,
    instr: Instr,
    phase: ParallelCopyPhase,
    from: OperandAssignment,
    to: OperandAssignment,
) {
    if from != to {
        copies.push(ParallelCopy {
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
        let mut copies = ParallelCopies::new();

        self.assign_spill_slots(&mut assignment);

        // Assign all fixed operands first, since dead fixed outputs will not even have associated
        // live ranges.
        self.assign_fixed_operands(&mut assignment);

        // Now, extract everything else we need (operand assignments, copies) out of the live range
        // assignments.
        self.sort_vreg_ranges();
        self.assign_allocated_operands(&mut assignment, &mut copies);
        self.collect_func_live_in_copies(&mut copies);
        self.collect_cross_fragment_copies(&mut copies);

        copies.sort_unstable_by_key(|copy| (copy.instr, copy.phase));

        assignment.copies = resolve_parallel_copies(&copies);

        if cfg!(debug_assertions) {
            assignment.verify_all_assigned();
        }

        assignment
    }

    fn collect_func_live_in_copies(&self, copies: &mut ParallelCopies) {
        let first_block = self.cfg_ctx.block_order[0];
        for (&block_param, &preg) in self
            .lir
            .block_params(first_block)
            .iter()
            .zip(self.lir.live_in_regs())
        {
            let Some(&first_range) = self.vreg_ranges[block_param.reg_num()].first() else {
                continue;
            };
            let assignment = self.get_range_assignment(first_range);

            record_parallel_copy(
                copies,
                Instr::new(0),
                ParallelCopyPhase::Before,
                OperandAssignment::Reg(preg),
                assignment,
            );
        }
    }

    fn collect_cross_fragment_copies(&self, copies: &mut ParallelCopies) {
        let mut block_param_ins = BlockParamIns::new();
        let mut block_param_outs = BlockParamOutMap::default();

        // Step 1: Gather all vreg-local copies, tracking information about block params for the
        // next step.
        for (vreg, ranges) in self.vreg_ranges.iter() {
            self.collect_intra_block_range_copies(ranges, copies);
            self.collect_inter_block_range_copies(
                vreg,
                ranges,
                copies,
                &mut block_param_ins,
                &mut block_param_outs,
            );
        }

        // Step 2: Insert copies for block params once we've gathered block param information for
        // all vregs.
        trace!("inserting block param copies");
        for incoming in &block_param_ins {
            trace!("  -> {} ({})", incoming.vreg, incoming.block);
            for &pred in self.cfg_ctx.cfg.block_preds(incoming.block) {
                trace!("    {pred}");
                let from_assignment = *block_param_outs
                    .get(&BlockParamEdgeKey {
                        from_block: pred,
                        to_vreg: incoming.vreg,
                    })
                    .expect("block param source not recorded for predecessor");

                // We always insert copies for block params in the predecessor because we know we
                // are its unique successor.
                let pred_terminator = self.lir.block_terminator(pred);
                record_parallel_copy(
                    copies,
                    pred_terminator,
                    ParallelCopyPhase::PreCopy,
                    from_assignment,
                    incoming.assignment,
                );
            }
        }
    }

    fn collect_intra_block_range_copies(
        &self,
        ranges: &[LiveRange],
        copies: &mut Vec<ParallelCopy>,
    ) {
        let mut last_spill: Option<(LiveRange, SpillSlot)> = None;
        let mut prev_range: Option<(LiveRange, OperandAssignment)> = None;

        for &range in ranges {
            let range_assignment = self.get_range_assignment(range);
            let range_instrs = &self.live_ranges[range].instrs;
            let prog_range = self.live_ranges[range].prog_range;
            let mut copied_from_prev = false;

            // Stitch together live ranges with touching endpoints.
            if let Some((prev_range, prev_assignment)) = prev_range {
                let prev_prog_range = self.live_ranges[prev_range].prog_range;
                if prev_prog_range.end == prog_range.start {
                    let boundary = prog_range.start;
                    let instr = boundary.instr();

                    // Adjacent ranges belonging to the same vreg should only happen because of
                    // basic block boundaries or splits, and both those cases use the `Before`
                    // slot.
                    debug_assert!(boundary.slot() == InstrSlot::Before);

                    // Inter-block copies need to be handled more delicately, so we do them
                    // separately.
                    if !is_block_header(self.lir, instr) {
                        record_parallel_copy(
                            copies,
                            instr,
                            ParallelCopyPhase::Before,
                            prev_assignment,
                            range_assignment,
                        );
                        copied_from_prev = true;
                    }
                }
            }

            prev_range = Some((range, range_assignment));

            // Check if we have any spills/reloads to perform.
            if let Some((spill_range, spill_slot)) = last_spill {
                let spill_prog_range = self.live_ranges[spill_range].prog_range;

                // Note: we expect to start _strictly_ after the current spill because
                // register ranges should be sorted before spill ranges starting at the same
                // point.
                debug_assert!(prog_range.start > spill_prog_range.start);

                // If we have a later range intersecting the current spill range, it should
                // be a small spill/reload connector; record the copy for that now.
                if prog_range.end <= spill_prog_range.end {
                    // If the spilled def comes from the range just before us, there's no need
                    // to reload from the stack; we've already copied into our new register
                    // above.
                    if !copied_from_prev {
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
                            // Spill:
                            // This store could end up dead if the only use of the spilled value
                            // is the next instruction, in which case a simple register copy
                            // will be inserted. In practice, dealing with that would require
                            // dramatically complicating things here for very little tangible
                            // benefit (we expect these situations not to crop up often because
                            // shorter live ranges have highger spill weights).
                            record_parallel_copy(
                                copies,
                                instr.next(),
                                ParallelCopyPhase::Before,
                                range_assignment,
                                OperandAssignment::Spill(spill_slot),
                            );
                        } else {
                            // Reload:
                            record_parallel_copy(
                                copies,
                                instr,
                                ParallelCopyPhase::Reload,
                                OperandAssignment::Spill(spill_slot),
                                range_assignment,
                            );
                        };
                    }
                } else {
                    // We've run off the end of the spill range, reset things.
                    last_spill = None;
                }
            }

            if let OperandAssignment::Spill(spill_slot) = range_assignment {
                last_spill = Some((range, spill_slot));
            }
        }
    }

    fn collect_inter_block_range_copies(
        &self,
        vreg: VirtRegNum,
        ranges: &[LiveRange],
        copies: &mut ParallelCopies,
        block_param_ins: &mut BlockParamIns,
        block_param_outs: &mut BlockParamOutMap,
    ) {
        let mut block_outs = FxHashMap::default();
        let mut block_ins = Vec::new();

        let mut blocks = &self.cfg_ctx.block_order[..];

        trace!("collecting inter-block copies: {vreg}");

        // Walk through all block boundaries covered by `ranges`, collecting live-in and live-out
        // information.
        for &range in ranges {
            let assignment = self.get_range_assignment(range);
            let prog_range = self.live_ranges[range].prog_range;

            trace!("  {prog_range:?}");

            let Some(&first_block) = blocks.first() else {
                // If all blocks have been exhausted, we definitely don't have anything to do.
                break;
            };

            // When dealing with spill/reload connector ranges, we'll have ranges later in the list
            // that end before a previous one. Just skip those ranges, since they can never carry
            // the value into/out of blocks anyway.
            if prog_range.end < ProgramPoint::before(self.lir.block_instrs(first_block).start) {
                continue;
            }

            let mut block_idx = blocks
                .binary_search_by_key(&prog_range.start, |&block| {
                    ProgramPoint::before(self.lir.block_instrs(block).start)
                })
                .unwrap_or_else(|i| i - 1);

            while block_idx < blocks.len() {
                let block = blocks[block_idx];
                let block_range = self.lir.block_instrs(block);
                let header = block_range.start;
                let next_header = block_range.end;

                trace!("    {block}:");

                if prog_range.start <= ProgramPoint::before(header) {
                    if self.block_live_ins[block].contains(vreg) {
                        // This vreg is live-in at this block: record it so we can insert copies for the
                        // value once all live-out assignments are known.
                        trace!("      in");
                        block_ins.push((block, assignment));
                    } else {
                        // This vreg needs to be defined upon entry to this block, but isn't
                        // implicitly carried in from other blocks - its lifetime starts here. This
                        // is only possible when the vreg is a block param.
                        trace!("      in (block param)");

                        debug_assert!(self
                            .lir
                            .block_params(block)
                            .iter()
                            .any(|param| param.reg_num() == vreg));

                        block_param_ins.push(BlockParamIn {
                            block,
                            vreg,
                            assignment,
                        });
                    }
                }

                if ProgramPoint::before(next_header) <= prog_range.end {
                    // This vreg is live-out of this block. Note that the `<=` is very important
                    // here: the value is live-out even when it isn't used in the next block, in
                    // which case the range will end `Before` the next block's header.
                    trace!("      out");
                    block_outs.insert(block, assignment);
                }

                // TODO: This could be made more efficient by flattening and sorting things by
                // vreg/block in advance.
                let outgoing_param_uses = self
                    .lir
                    .outgoing_block_params(block)
                    .iter()
                    .copied()
                    .enumerate()
                    .filter(|(_idx, param)| param.reg_num() == vreg)
                    .map(|(idx, _param)| idx);

                for param_idx in outgoing_param_uses {
                    trace!("      out (block param {param_idx})");
                    for &succ in self.cfg_ctx.cfg.block_succs(block) {
                        let to_vreg = self.lir.block_params(succ)[param_idx].reg_num();
                        let key = BlockParamEdgeKey {
                            from_block: block,
                            to_vreg,
                        };
                        block_param_outs.insert(key, assignment);
                    }
                }

                if ProgramPoint::before(next_header) >= prog_range.end {
                    break;
                }

                block_idx += 1;
            }

            blocks = &blocks[block_idx..];
        }

        trace!("placing inter-block copies: {vreg}");

        // Now that we have the necessary liveness information, place copies along the appropriate
        // edges.
        for &(block, assignment) in &block_ins {
            let preds = self.cfg_ctx.cfg.block_preds(block);
            let single_pred = preds.len() == 1;

            for &pred in preds {
                let Some(&pred_assignment) = block_outs.get(&pred) else {
                    panic!("vreg {vreg} not live-out across edge {pred} -> {block}");
                };

                if assignment == pred_assignment {
                    continue;
                }

                // If we have a single predecessor, we can safely insert the copy at the start of
                // our own block. Otherwise, our predecessors should each have only `block` as a
                // successor, allowing the copy to be inserted at the end of each.
                let (instr, phase) = if single_pred {
                    (
                        self.lir.block_instrs(block).start,
                        ParallelCopyPhase::Before,
                    )
                } else {
                    debug_assert!(
                        self.cfg_ctx.cfg.block_succs(pred).len() == 1,
                        "critical edge not split"
                    );
                    let pred_terminator = self.lir.block_terminator(pred);
                    (pred_terminator, ParallelCopyPhase::PreCopy)
                };

                trace!("  {pred} -> {block} at {instr}");
                copies.push(ParallelCopy {
                    instr,
                    phase,
                    from: pred_assignment,
                    to: assignment,
                });
            }
        }
    }

    fn assign_allocated_operands(&self, assignment: &mut Assignment, copies: &mut ParallelCopies) {
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
                        // Just make sure to record the appropriate copy - we want it in the
                        // `BeforeInstr` phase because we model the register reservation as ending.
                        record_parallel_copy(
                            copies,
                            instr.next(),
                            ParallelCopyPhase::Before,
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
                                record_parallel_copy(
                                    copies,
                                    instr,
                                    ParallelCopyPhase::PreCopy,
                                    range_assignment,
                                    OperandAssignment::Reg(preg),
                                );
                            }
                            UseOperandConstraint::TiedToDef(j) => {
                                let def_assignment =
                                    assignment.instr_def_assignments(instr)[j as usize];

                                record_parallel_copy(
                                    copies,
                                    instr,
                                    ParallelCopyPhase::PreCopy,
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
            copies: TaggedAssignmentCopies::new(),
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

fn resolve_parallel_copies(mut parallel_copies: &[ParallelCopy]) -> Vec<TaggedAssignmentCopy> {
    let mut resolved = Vec::new();
    while let Some((instr, chunk)) = next_copy_chunk(&mut parallel_copies) {
        parallel_copy::resolve(
            chunk,
            || todo!("get tmp reg"),
            |copy| resolved.push(TaggedAssignmentCopy { instr, copy: *copy }),
        );
    }
    resolved
}

fn next_copy_chunk<'a>(
    parallel_copies: &mut &'a [ParallelCopy],
) -> Option<(Instr, &'a [ParallelCopy])> {
    let first = parallel_copies.first()?;
    let len = parallel_copies
        .iter()
        .position(|copy| (copy.instr, copy.phase) != (first.instr, first.phase))
        .unwrap_or(parallel_copies.len());
    let (cur_copies, next_copies) = parallel_copies.split_at(len);
    *parallel_copies = next_copies;
    Some((first.instr, cur_copies))
}

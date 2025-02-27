use core::{cmp, iter, mem};

use alloc::vec::Vec;

use cranelift_entity::{EntityRef, PrimaryMap, SecondaryMap, packed_option::ReservedValue};
use entity_set::DenseEntitySet;
use fx_utils::FxHashMap;
use log::trace;
use smallvec::SmallVec;

use crate::{
    cfg::{Block, CfgContext},
    lir::{
        DefOperandConstraint, Instr, InstrRange, Lir, MemLayout, PhysReg, PhysRegSet, RegClass,
        UseOperandConstraint, VirtReg,
    },
    machine::{MachineCore, MachineRegalloc},
    regalloc::types::{LiveRangeFlags, LiveSetFragmentFlags},
};

use super::{
    Assignment, InstrAssignmentData, OperandAssignment, SpillSlot, SpillSlotData,
    conflict::{RangeKeyIter, iter_btree_ranges, iter_slice_ranges},
    context::RegAllocContext,
    parallel_copy::{self, RegScavenger},
    redundant_copy::{RedundantCopyTracker, RedundantCopyVerdict},
    types::{
        BlockExitGhostCopy, CopySourceAssignment, InstrSlot, LiveRange, ParallelCopies,
        ParallelCopy, ParallelCopyPhase, ProgramPoint, TaggedAssignmentCopy,
    },
};

// Every block parameter is uniquely identified by its destination vreg because everything is in
// SSA. To uniquely identify a complete "from -> to" pair, all we need in addition is the relevant
// source block.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct BlockParamEdgeKey {
    from_block: Block,
    to_vreg: VirtReg,
}

struct BlockParamIn {
    block: Block,
    vreg: VirtReg,
    assignment: OperandAssignment,
}

type BlockIns = Vec<(Block, OperandAssignment)>;
type BlockOutMap = FxHashMap<Block, CopySourceAssignment>;
type BlockParamIns = Vec<BlockParamIn>;
type BlockParamOutMap = FxHashMap<BlockParamEdgeKey, (VirtReg, CopySourceAssignment)>;

fn record_parallel_copy(
    copies: &mut ParallelCopies,
    instr: Instr,
    phase: ParallelCopyPhase,
    class: RegClass,
    from: CopySourceAssignment,
    to: OperandAssignment,
) {
    if from != to.into() {
        copies.push(ParallelCopy {
            instr,
            phase,
            class,
            from,
            to,
        });
    }
}

impl<M: MachineRegalloc> RegAllocContext<'_, M> {
    pub fn reify(&mut self) -> Assignment {
        let mut assignment = Assignment::new(self.lir, mem::take(&mut self.killed_remat_defs));

        self.assign_spill_slots(&mut assignment);

        // Set all fixed operands first, since dead fixed outputs will not even have associated live
        // ranges.
        self.reify_fixed_operands(&mut assignment);

        let mut copies = ParallelCopies::new();
        // Now, extract everything else we need (operand assignments, copies) out of the live range
        // assignments.
        self.reify_allocated_operands(&mut assignment, &mut copies);
        self.collect_func_live_in_copies(&mut copies);
        self.collect_cross_range_copies(&mut assignment, &mut copies);

        self.resolve_parallel_copies(&mut assignment, copies);

        if cfg!(debug_assertions) {
            assignment.verify_all_assigned();
        }

        assignment
    }

    fn resolve_parallel_copies(
        &self,
        assignment: &mut Assignment,
        mut parallel_copies: ParallelCopies,
    ) {
        // Group copies by instruction/phase so we can walk them easily.
        parallel_copies.sort_unstable_by_key(parallel_copy_key);
        let mut parallel_copies = &parallel_copies[..];

        let mut resolved = Vec::new();
        let mut scavenger = AssignedRegScavenger::new(self, assignment);
        let mut redundant_copy_tracker = RedundantCopyTracker::new();

        let mut last_instr = self.lir.all_instrs().start;

        while let Some((pos, class, chunk)) = next_copy_chunk(&mut parallel_copies) {
            let instr = pos.instr();

            scavenger.reset(class, pos, chunk);
            advance_redundant_copy_tracker(
                self.lir,
                scavenger.assignment,
                last_instr,
                instr,
                &mut redundant_copy_tracker,
            );

            parallel_copy::resolve(chunk, &mut scavenger, |copy| {
                if redundant_copy_tracker.process_copy(copy) == RedundantCopyVerdict::Necessary {
                    resolved.push(TaggedAssignmentCopy {
                        instr: pos.instr(),
                        copy: *copy,
                    })
                }
            });

            last_instr = instr;
        }

        assignment.copies = resolved;
    }

    fn collect_func_live_in_copies(&self, copies: &mut ParallelCopies) {
        let first_block = self.cfg_ctx.block_order[0];
        for (&block_param, &preg) in self
            .lir
            .block_params(first_block)
            .iter()
            .zip(self.lir.live_in_regs())
        {
            let Some(&first_range) = self.vreg_ranges[block_param].first() else {
                continue;
            };
            let assignment = self.get_range_operand_assignment(first_range);

            record_parallel_copy(
                copies,
                Instr::new(0),
                ParallelCopyPhase::Before,
                self.lir.vreg_class(block_param),
                OperandAssignment::Reg(preg).into(),
                assignment,
            );
        }
    }

    fn collect_cross_range_copies(&self, assignment: &mut Assignment, copies: &mut ParallelCopies) {
        let mut block_ins = BlockIns::new();
        let mut block_outs = BlockOutMap::default();

        let mut block_param_ins = BlockParamIns::new();
        let mut block_param_outs = BlockParamOutMap::default();

        // Step 1: Gather all vreg-local copies, tracking information about block params for the
        // next step.
        for (vreg, ranges) in self.vreg_ranges.iter() {
            self.collect_intra_block_range_copies(vreg, ranges, copies);

            block_ins.clear();
            block_outs.clear();
            self.collect_block_in_outs(
                vreg,
                ranges,
                &mut block_ins,
                &mut block_outs,
                &mut block_param_ins,
                &mut block_param_outs,
            );

            self.collect_inter_block_range_copies(vreg, copies, &block_ins, &block_outs);
        }

        // Step 2: Insert copies for block params once we've gathered block param information for
        // all vregs.
        self.collect_block_param_copies(&block_param_ins, &block_param_outs, assignment, copies);
    }

    fn collect_intra_block_range_copies(
        &self,
        vreg: VirtReg,
        ranges: &[LiveRange],
        copies: &mut Vec<ParallelCopy>,
    ) {
        trace!("collecting intra-block copies: {vreg}");

        let class = self.lir.vreg_class(vreg);

        let mut last_canonical_range: Option<(LiveRange, CopySourceAssignment)> = None;
        for &range in ranges {
            let range_assignment = self.get_range_assignment(range);
            let range_data = &self.live_ranges[range];
            let prog_range = range_data.prog_range;

            trace!("    {prog_range:?}");

            if !range_data.flags.contains(LiveRangeFlags::SPILL_CONNECTOR) {
                // Ordinary (canonical), non-remat ranges: stitch together live ranges with touching
                // endpoints.

                if let Some(range_assignment) = range_assignment.as_operand() {
                    if let Some((last_range, last_assignment)) = last_canonical_range {
                        let last_range_data = &self.live_ranges[last_range];
                        let last_prog_range = last_range_data.prog_range;
                        if last_prog_range.end == prog_range.start {
                            let boundary = prog_range.start;
                            let instr = boundary.instr();

                            // Adjacent ranges belonging to the same vreg should only happen because of
                            // basic block boundaries or splits, and both those cases use the `Before`
                            // slot.
                            debug_assert!(boundary.slot() == InstrSlot::Before);

                            // Inter-block copies need to be handled more delicately, so we do them
                            // separately.
                            if !is_block_header(self.lir, self.cfg_ctx, instr) {
                                trace!("        copy: {instr}");

                                record_parallel_copy(
                                    copies,
                                    instr,
                                    ParallelCopyPhase::Before,
                                    class,
                                    last_assignment,
                                    range_assignment,
                                );
                            }
                        }
                    }
                }

                last_canonical_range = Some((range, range_assignment));
            } else {
                // Spill connectors: insert a spill/reload or remat.

                let range_assignment = range_assignment
                    .as_operand()
                    .expect("spill/reload connector cannot be a remat");

                let (spill_range, source) =
                    last_canonical_range.expect("spill/reload connector without surrounding spill");

                debug_assert!(!source.is_reg(), "spill/reload to/from register");

                let spill_prog_range = self.live_ranges[spill_range].prog_range;

                debug_assert!(spill_prog_range.contains(prog_range));

                // All such connectors should be small ranges requiring registers and covering a
                // single instruction.
                debug_assert!(range_assignment.is_reg());
                debug_assert!(range_data.instrs.len() == 1);

                let range_instr = range_data.instrs[0];
                let instr = range_instr.instr();
                // We should never have inserted spills/reloads for non-`needs_reg`
                // operands.
                debug_assert!(range_instr.needs_reg());

                if range_instr.is_def() {
                    // We should never have any spill connectors for rematerializable defs: those
                    // instructions should be killed entirely during spilling.
                    let spill = source.as_spill().unwrap();
                    trace!("    spill ({spill}): {}", instr.next());
                    record_parallel_copy(
                        copies,
                        instr.next(),
                        ParallelCopyPhase::Before,
                        class,
                        range_assignment.into(),
                        OperandAssignment::Spill(spill),
                    );
                } else {
                    trace!("        reload ({}): {instr}", source.display(self.lir));
                    record_parallel_copy(
                        copies,
                        instr,
                        ParallelCopyPhase::Reload,
                        class,
                        source,
                        range_assignment,
                    );
                }
            }
        }
    }

    fn collect_block_in_outs(
        &self,
        vreg: VirtReg,
        ranges: &[LiveRange],
        block_ins: &mut BlockIns,
        block_outs: &mut BlockOutMap,
        block_param_ins: &mut BlockParamIns,
        block_param_outs: &mut BlockParamOutMap,
    ) {
        let block_order = &self.cfg_ctx.block_order[..];

        let mut last_range_end_block = 0;

        trace!("collecting inter-block inputs/output: {vreg}");

        // Walk through all block boundaries covered by `ranges`, collecting live-in and live-out
        // information.
        for &range in ranges {
            let assignment = self.get_range_assignment(range);
            let prog_range = self.live_ranges[range].prog_range;

            trace!("  {prog_range:?}");

            let Some(&last_block) = block_order.get(last_range_end_block) else {
                // If all blocks have been exhausted, we definitely don't have anything to do.
                break;
            };

            // Spill connectors can never carry a value into/out of a block, so skip them.
            if self.live_ranges[range]
                .flags
                .contains(LiveRangeFlags::SPILL_CONNECTOR)
            {
                continue;
            }

            // Canonical ranges should always be disjoint and sorted.
            debug_assert!(
                prog_range.end >= ProgramPoint::before(self.lir.block_instrs(last_block).start)
            );

            let mut block_idx = self.lir.instr_block_index(prog_range.start.instr());

            while block_idx < block_order.len() {
                let block = block_order[block_idx];
                let block_range = self.lir.block_instrs(block);
                let header = block_range.start;
                let next_header = block_range.end;

                trace!("    {block}:");

                if prog_range.start <= ProgramPoint::before(header) {
                    // Completely ignore any remats that are live-through this block: they will be
                    // generated only when necessary and never need to be copied into.
                    if let Some(assignment) = assignment.as_operand() {
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

                            debug_assert!(
                                self.lir
                                    .block_params(block)
                                    .iter()
                                    .any(|&param| param == vreg)
                            );

                            block_param_ins.push(BlockParamIn {
                                block,
                                vreg,
                                assignment,
                            });
                        }
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
                    .filter(|(_idx, param)| *param == vreg)
                    .map(|(idx, _param)| idx);

                for param_idx in outgoing_param_uses {
                    trace!("      out (block param {param_idx})");
                    for &succ in self.cfg_ctx.cfg.block_succs(block) {
                        let to_vreg = self.lir.block_params(succ)[param_idx];
                        let key = BlockParamEdgeKey {
                            from_block: block,
                            to_vreg,
                        };
                        block_param_outs.insert(key, (vreg, assignment));
                    }
                }

                if ProgramPoint::before(next_header) >= prog_range.end {
                    break;
                }

                block_idx += 1;
            }

            last_range_end_block = block_idx;
        }
    }

    fn collect_inter_block_range_copies(
        &self,
        vreg: VirtReg,
        copies: &mut ParallelCopies,
        block_ins: &BlockIns,
        block_outs: &BlockOutMap,
    ) {
        trace!("placing inter-block copies: {vreg}");

        // Now that we have the necessary liveness information, place copies along the appropriate
        // edges.
        for &(block, assignment) in block_ins {
            let preds = self.cfg_ctx.cfg.block_preds(block);
            let single_pred = preds.len() == 1;

            for &pred in preds {
                let Some(&pred_assignment) = block_outs.get(&pred) else {
                    panic!("vreg {vreg} not live-out across edge {pred} -> {block}");
                };

                if pred_assignment == assignment.into() {
                    // Nothing to copy here, don't bother trying to figure out where.
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
                    class: self.lir.vreg_class(vreg),
                    from: pred_assignment,
                    to: assignment,
                });
            }
        }
    }

    fn collect_block_param_copies(
        &self,
        block_param_ins: &BlockParamIns,
        block_param_outs: &BlockParamOutMap,
        assignment: &mut Assignment,
        copies: &mut Vec<ParallelCopy>,
    ) {
        trace!("collecting block param copies");

        for incoming in block_param_ins {
            trace!("  -> {} ({})", incoming.vreg, incoming.block);
            let class = self.lir.vreg_class(incoming.vreg);
            for &pred in self.cfg_ctx.cfg.block_preds(incoming.block) {
                trace!("    {pred}");
                let (from_vreg, from_assignment) = *block_param_outs
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
                    class,
                    from_assignment,
                    incoming.assignment,
                );

                // Record a "ghost copy" indicating that the new assignment now belongs to the
                // destination vreg and not the source vreg.
                assignment.block_exit_ghost_copies.push(BlockExitGhostCopy {
                    block: pred,
                    assignment: incoming.assignment,
                    from_vreg,
                    to_vreg: incoming.vreg,
                });
            }
        }

        // Keep things sorted by block so we can look them up easily later.
        assignment
            .block_exit_ghost_copies
            .sort_unstable_by_key(|ghost_copy| ghost_copy.block.as_u32());
    }

    fn reify_allocated_operands(&self, assignment: &mut Assignment, copies: &mut ParallelCopies) {
        // First pass: sort each register's live ranges and resolve all instruction def operands to
        // the correct physical registers.
        for (vreg, vreg_ranges) in self.vreg_ranges.iter() {
            for &range in vreg_ranges {
                let instrs = &self.live_ranges[range].instrs;
                if instrs.is_empty() {
                    continue;
                }

                let range_assignment = self.get_range_operand_assignment(range);

                // Assign defs based on range data.
                for &range_instr in instrs {
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
                        .find(|(_, def_op)| def_op.reg() == vreg)
                        .unwrap();

                    if let DefOperandConstraint::Fixed(preg) = def_op.constraint() {
                        // The assignment itself should have been handled by `assign_fixed_operands`.
                        // Just make sure to record the appropriate copy - we want it in the
                        // `BeforeInstr` phase because we model the register reservation as ending.
                        record_parallel_copy(
                            copies,
                            instr.next(),
                            ParallelCopyPhase::Before,
                            self.lir.vreg_class(vreg),
                            OperandAssignment::Reg(preg).into(),
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
                let instrs = &self.live_ranges[range].instrs;
                if instrs.is_empty() {
                    continue;
                }

                let range_assignment = self.get_range_operand_assignment(range);

                for &range_instr in instrs {
                    if range_instr.is_def() {
                        continue;
                    }

                    let instr = range_instr.instr();
                    // An instruction may use a vreg multiple times.
                    for (i, use_op) in self.lir.instr_uses(instr).iter().enumerate() {
                        if use_op.reg() != vreg {
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
                                    self.lir.vreg_class(vreg),
                                    range_assignment.into(),
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
                                    self.lir.vreg_class(vreg),
                                    range_assignment.into(),
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

    fn reify_fixed_operands(&self, assignment: &mut Assignment) {
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

    fn get_range_operand_assignment(&self, range: LiveRange) -> OperandAssignment {
        self.get_range_assignment(range)
            .as_operand()
            .expect("expected range assignment to be an operand")
    }

    fn get_range_assignment(&self, range: LiveRange) -> CopySourceAssignment {
        let fragment_data = &self.live_set_fragments[self.live_ranges[range].fragment];
        match fragment_data.assignment.expand() {
            Some(preg) => OperandAssignment::Reg(preg).into(),
            None => {
                debug_assert!(fragment_data.flags.contains(LiveSetFragmentFlags::SPILLED));

                let vreg = self.live_ranges[range].vreg;
                if let Some(remat_def) = self.remattable_vreg_defs[vreg].expand() {
                    return CopySourceAssignment::Remat(remat_def.instr());
                }

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
                    .into()
            }
        }
    }

    fn scavenge_free_reg_at(
        &self,
        class: RegClass,
        pos: ProgramPoint,
        used: &mut PhysRegSet,
    ) -> Option<PhysReg> {
        let reg = *self
            .machine
            .usable_regs(class)
            .iter()
            .find(|&&reg| !used.contains(reg) && self.is_reg_free_at(reg, pos))?;
        used.insert(reg);
        Some(reg)
    }

    fn is_reg_free_at(&self, reg: PhysReg, pos: ProgramPoint) -> bool {
        !self.is_reg_reserved_at(reg, pos) && !self.is_reg_assigned_at(reg, pos)
    }

    fn is_reg_assigned_at(&self, reg: PhysReg, pos: ProgramPoint) -> bool {
        let assignments = iter_btree_ranges(&self.phys_reg_assignments[reg.as_u8() as usize]);
        has_containing_range(assignments, pos)
    }

    fn is_reg_reserved_at(&self, reg: PhysReg, pos: ProgramPoint) -> bool {
        let reservations = iter_slice_ranges(
            &self.phys_reg_reservations[reg.as_u8() as usize],
            |reservation| (reservation.prog_range, &()),
        );
        has_containing_range(reservations, pos)
    }
}

impl Assignment {
    fn new<M: MachineRegalloc>(lir: &Lir<M>, killed_remat_defs: DenseEntitySet<Instr>) -> Self {
        let instr_count = lir.all_instrs().end.as_u32() as usize;

        let mut assignment = Assignment {
            spill_slots: PrimaryMap::new(),
            copies: Vec::new(),
            block_exit_ghost_copies: Vec::new(),
            instr_assignments: SecondaryMap::with_capacity(instr_count),
            // Estimate: every instruction has at least one use or def. This isn't actually true,
            // but should usually be compensated for by the fact that many instructions have more
            // than one operand.
            operand_assignment_pool: Vec::with_capacity(instr_count),
            killed_remat_defs,
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
            if self.killed_remat_defs.contains(instr) {
                continue;
            }

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
        self.spill_slots.push(SpillSlotData { layout })
    }

    fn expand_spill_slot(&mut self, spill: SpillSlot, new_layout: MemLayout) {
        let existing_layout = &mut self.spill_slots[spill].layout;
        existing_layout.size = cmp::max(existing_layout.size, new_layout.size);
        existing_layout.align = cmp::max(existing_layout.align, new_layout.align);
    }
}

struct AssignedRegScavenger<'a, M: MachineRegalloc> {
    ctx: &'a RegAllocContext<'a, M>,
    assignment: &'a mut Assignment,
    tmp_spills: SmallVec<[SpillSlot; 2]>,

    // Per-resolution state
    class: RegClass,
    pos: ProgramPoint,
    tmp_spill_idx: usize,
    used_tmp_regs: PhysRegSet,
}

impl<'a, M: MachineRegalloc> AssignedRegScavenger<'a, M> {
    fn new(ctx: &'a RegAllocContext<'a, M>, assignment: &'a mut Assignment) -> Self {
        Self {
            ctx,
            assignment,
            tmp_spills: SmallVec::new(),
            class: RegClass::new(0),
            pos: ProgramPoint::before(Instr::new(0)),
            tmp_spill_idx: 0,
            used_tmp_regs: PhysRegSet::empty(),
        }
    }

    fn reset(&mut self, class: RegClass, pos: ProgramPoint, copies: &[ParallelCopy]) {
        self.class = class;
        self.pos = pos;
        self.tmp_spill_idx = 0;
        self.used_tmp_regs = PhysRegSet::empty();

        // All copy sources need to be marked as used, because their live ranges could end just
        // before `pos`. Destinations need to be marked as used for correct behavior with block
        // live-outs and outgoing params, which might not be live at all at `pos`.
        for copy in copies {
            if let CopySourceAssignment::Operand(OperandAssignment::Reg(from)) = copy.from {
                self.used_tmp_regs.insert(from);
            }
            if let OperandAssignment::Reg(to) = copy.to {
                self.used_tmp_regs.insert(to);
            }
        }
    }
}

impl<M: MachineRegalloc> RegScavenger for AssignedRegScavenger<'_, M> {
    fn emergency_reg(&self) -> PhysReg {
        self.ctx.machine.usable_regs(self.class)[0]
    }

    fn get_fresh_tmp_reg(&mut self) -> Option<PhysReg> {
        self.ctx
            .scavenge_free_reg_at(self.class, self.pos, &mut self.used_tmp_regs)
    }

    fn get_fresh_tmp_spill(&mut self) -> SpillSlot {
        let spill_idx = self.tmp_spill_idx;
        let new_layout = self.ctx.machine.reg_class_spill_layout(self.class);

        self.tmp_spill_idx += 1;

        if let Some(&spill) = self.tmp_spills.get(spill_idx) {
            self.assignment.expand_spill_slot(spill, new_layout);
            return spill;
        }

        debug_assert_eq!(spill_idx, self.tmp_spills.len());
        let spill = self.assignment.create_spill_slot(new_layout);
        self.tmp_spills.push(spill);
        spill
    }
}

fn next_copy_chunk<'a>(
    parallel_copies: &mut &'a [ParallelCopy],
) -> Option<(ProgramPoint, RegClass, &'a [ParallelCopy])> {
    let first = parallel_copies.first()?;
    let len = parallel_copies
        .iter()
        .position(|copy| parallel_copy_key(copy) != parallel_copy_key(first))
        .unwrap_or(parallel_copies.len());

    let (cur_copies, next_copies) = parallel_copies.split_at(len);
    *parallel_copies = next_copies;

    Some((
        ProgramPoint::new(first.instr, first.phase.slot()),
        first.class,
        cur_copies,
    ))
}

fn advance_redundant_copy_tracker<M: MachineCore>(
    lir: &Lir<M>,
    assignment: &Assignment,
    from_instr: Instr,
    to_instr: Instr,
    tracker: &mut RedundantCopyTracker,
) {
    if lir.instr_block_index(from_instr) != lir.instr_block_index(to_instr) {
        tracker.reset();
        return;
    }

    for instr in InstrRange::new(from_instr, to_instr) {
        if assignment.killed_remat_defs.contains(instr) {
            continue;
        }

        for &def in assignment.instr_def_assignments(instr) {
            tracker.assignment_clobbered(def);
        }
        for clobber in lir.instr_clobbers(instr).iter() {
            tracker.assignment_clobbered(OperandAssignment::Reg(clobber));
        }
    }
}

fn parallel_copy_key(copy: &ParallelCopy) -> u64 {
    let instr = (copy.instr.as_u32() as u64) << 16;
    let class = (copy.class.as_u8() as u64) << 8;
    let phase = copy.phase as u64;
    instr | class | phase
}

fn has_containing_range(mut iter: impl RangeKeyIter, pos: ProgramPoint) -> bool {
    iter.skip_to_endpoint_above(pos);
    iter.current()
        .is_some_and(|(cur_range, _)| pos >= cur_range.start)
}

fn is_block_header<M: MachineCore>(lir: &Lir<M>, cfg_ctx: &CfgContext, instr: Instr) -> bool {
    instr
        == lir
            .block_instrs(cfg_ctx.block_order[lir.instr_block_index(instr)])
            .start
}

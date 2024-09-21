use alloc::collections::VecDeque;
use cranelift_entity::{packed_option::ReservedValue, SecondaryMap};
use fx_utils::FxHashSet;
use log::trace;
use smallvec::{smallvec, SmallVec};

use crate::{
    cfg::{Block, CfgContext},
    lir::{
        DefOperandConstraint, Instr, Lir, OperandPos, PhysReg, UseOperandConstraint, VirtRegNum,
    },
    machine::MachineRegalloc,
    regalloc::utils::is_sorted_by_key,
};

use super::{
    types::{
        AnnotatedPhysRegHint, LiveRange, LiveRangeData, LiveRangeInstr, LiveRangeInstrs,
        LiveRangeOpPos, LiveSetFragment, PhysRegHint, PhysRegReservation, ProgramPoint,
        ProgramRange,
    },
    utils::get_instr_weight,
    virt_reg_set::VirtRegSet,
    RegAllocContext,
};

impl<'a, M: MachineRegalloc> RegAllocContext<'a, M> {
    pub fn push_vreg_live_range(
        &mut self,
        vreg: VirtRegNum,
        fragment: LiveSetFragment,
        prog_range: ProgramRange,
        instrs: LiveRangeInstrs,
        is_spill_connector: bool,
    ) -> LiveRange {
        let live_range = self.live_ranges.push(LiveRangeData {
            prog_range,
            vreg,
            fragment,
            instrs,
            is_spill_connector,
        });
        self.vreg_ranges[vreg].push(live_range);
        live_range
    }

    pub fn compute_liveness(&mut self) {
        let (live_ins, live_outs) = compute_block_liveness(self.lir, self.cfg_ctx);

        // Stash these for cross-block copy insertion later.
        self.block_live_ins = live_ins;

        trace!("computing precise live ranges");

        let mut tied_defs = SmallVec::<[bool; 4]>::new();

        for &block in self.cfg_ctx.block_order.iter().rev() {
            trace!("  {block}");

            let block_range = self.lir.block_instrs(block);
            let terminator = block_range.end.prev();

            // In case we need to insert a copy just before a jump (either because of outgoing block
            // params or because of a split live range), we don't model the small live range of the
            // target block's vreg after the pre-copy before the jump to the target block, so we
            // need to make sure no unaccounted-for ranges might interfere with that small range.
            // For now, we achieve this by completely disallowing block terminators to define vregs.
            assert!(
                self.lir.instr_defs(terminator).is_empty()
                    && self.lir.instr_clobbers(terminator).is_empty()
            );

            for live_out in &live_outs[block] {
                trace!("    live-out {live_out}");
                self.open_live_out_range(live_out, block);
            }

            for &outgoing_vreg in self.lir.outgoing_block_params(block) {
                trace!("    outgoing param {outgoing_vreg}");

                // We can safely treat the last instruction in the block as using the value for
                // spill weight purposes, because we know the copy (if there is one) will ultimately
                // be placed here, as critical edges are already split.
                self.record_live_use(
                    outgoing_vreg,
                    ProgramPoint::pre_copy(terminator),
                    // We never consider outgoing params as needing registers because they represent
                    // copies anyway.
                    false,
                    block,
                );
            }

            for instr in block_range.into_iter().rev() {
                self.compute_instr_liveness(block, instr, &mut tied_defs);
            }

            for &incoming_vreg in self.lir.block_params(block) {
                trace!("    incoming param {incoming_vreg}");
                self.record_def(
                    incoming_vreg,
                    ProgramPoint::before(block_range.start),
                    ProgramPoint::pre_copy(block_range.start),
                );
            }
        }

        // The live range lists have their ranges sorted from last to first at this point; get them
        // sorted properly now.
        for live_ranges in self.vreg_ranges.values_mut() {
            live_ranges.reverse();

            debug_assert!(is_sorted_by_key(live_ranges, |&live_range| self
                .live_ranges[live_range]
                .prog_range
                .start));
        }

        // Do the same thing for all physical register reservations...
        for preg in 0..M::phys_reg_count() {
            self.phys_reg_reservations[preg as usize].reverse();

            debug_assert!(is_sorted_by_key(
                &self.phys_reg_reservations[preg as usize],
                |reservation| reservation.prog_range.start
            ));
        }

        // ...and for instruction lists within each live range.
        for live_range in self.live_ranges.values_mut() {
            live_range.instrs.reverse();

            debug_assert!(is_sorted_by_key(&live_range.instrs, |live_range_instr| {
                // Require uses to appear before defs.
                (live_range_instr.instr().as_u32() << 1) | (live_range_instr.is_def() as u32)
            }));
        }

        // Add register hints for all live-in values.
        let first_instr = self.lir.all_instrs().start;
        for (&preg, &vreg) in self
            .lir
            .live_in_regs()
            .iter()
            .zip(self.lir.block_params(self.cfg_ctx.block_order[0]))
        {
            let Some(&first_range) = self.vreg_ranges[vreg].first() else {
                continue;
            };

            self.hint_live_range(first_range, preg, first_instr);
        }

        // Make sure live range hints are sorted by instruction.
        for hints in self.live_range_hints.values_mut() {
            hints.reverse();
            debug_assert!(is_sorted_by_key(hints, |hint| hint.instr));
        }
    }

    fn compute_instr_liveness(
        &mut self,
        block: Block,
        instr: Instr,
        tied_defs: &mut SmallVec<[bool; 4]>,
    ) {
        // Note: we assume an instruction never uses vregs it defines.

        let defs = self.lir.instr_defs(instr);
        let uses = self.lir.instr_uses(instr);

        tied_defs.clear();
        tied_defs.resize(defs.len(), false);

        // Setup: find all uses tied to defs and mark those defs accordingly.
        for use_op in uses {
            if let UseOperandConstraint::TiedToDef(i) = use_op.constraint() {
                let i = i as usize;
                assert!(
                    !tied_defs[i],
                    "multiple uses tied to same def in instr {instr}"
                );

                let tied_def = defs[i];
                assert!(use_op.pos() == OperandPos::Early);
                assert!(tied_def.pos() == OperandPos::Late);

                tied_defs[i] = true;
            }
        }

        // Process defs and clobbers first so our physical register reservations are always seen in
        // descending order.
        for (i, def_op) in defs.iter().enumerate() {
            let (op_def_point, mut range_op_pos) = if tied_defs[i] {
                (ProgramPoint::pre_copy(instr), Some(LiveRangeOpPos::PreCopy))
            } else {
                let lir_op_pos = def_op.pos();
                (
                    ProgramPoint::for_operand(instr, lir_op_pos),
                    Some(LiveRangeOpPos::for_lir_op_pos(lir_op_pos)),
                )
            };

            let def_point = if let DefOperandConstraint::Fixed(_) = def_op.constraint() {
                // For spill/reload allocation later, treat the definition of the *vreg* as not
                // happening in the instruction at all: the only thing that happens within the
                // instruction itself is the assignment to the physical register, and that is then
                // copied out after the instruction.

                // If the vreg range starting right after the instruction ends up being spilled, we
                // won't need to allocate a small "connector" range from the instruction to the
                // spill point, since we can copy directly from the fixed register to the stack.
                range_op_pos = None;
                ProgramPoint::before(instr.next())
            } else {
                op_def_point
            };

            let live_range = self.record_instr_def(
                def_op.reg(),
                instr,
                def_constraint_needs_reg(def_op.constraint()),
                range_op_pos,
                def_point,
                // If this def is dead, make sure its range always gets extended past the
                // end of the instruction so that dead early defs interfere with late defs.
                ProgramPoint::before(instr.next()),
            );

            if let DefOperandConstraint::Fixed(preg) = def_op.constraint() {
                self.reserve_phys_reg(
                    preg,
                    ProgramRange::new(op_def_point, def_point),
                    instr,
                    live_range,
                    // There isn't really a good reason for multiple defs to refer to the same
                    // physical register (the same vreg could just be reused later instead), and
                    // allowing that would complicate our `phys_reg_reservations`.
                    false,
                );
            }
        }

        let clobbers = self.lir.instr_clobbers(instr);
        if !clobbers.is_empty() {
            // Clobbers are equivalent to dead late defs with physical register constraints.
            let clobber_range = ProgramRange::new(
                ProgramPoint::late(instr),
                ProgramPoint::before(instr.next()),
            );
            for clobber in clobbers.iter() {
                // We intentionally allow clobbers to overlap with existing late-out
                // operands, to make things like call return values easier to model.
                self.reserve_phys_reg(clobber, clobber_range, instr, None, true);
            }
        }

        // Now that defs have been processed, move on to the uses.
        for use_op in uses {
            let vreg = use_op.reg();

            let op_pos = ProgramPoint::for_operand(instr, use_op.pos()).next();
            let pos = match use_op.constraint() {
                UseOperandConstraint::TiedToDef(_) => ProgramPoint::pre_copy(instr),
                UseOperandConstraint::Fixed(_) => {
                    // We model fixed register constraints as a pre-copy into the correct
                    // physical register, and directly reserve the relevant register for
                    // the correct range within the instruction.
                    ProgramPoint::pre_copy(instr)
                }
                _ => {
                    // Note: ranges are half-open, so we actually mark the use as extending
                    // to the next program point.
                    op_pos
                }
            };

            let needs_reg = match use_op.constraint() {
                UseOperandConstraint::AnyReg => true,
                UseOperandConstraint::TiedToDef(i) => {
                    let tied_def_constraint = defs[i as usize].constraint();
                    def_constraint_needs_reg(tied_def_constraint)
                }
                // Note: we don't consider fixed operands as needing registers as they are being
                // rewritten to dedicated copies.
                UseOperandConstraint::Any | UseOperandConstraint::Fixed(_) => false,
            };

            let live_range = self.record_live_use(vreg, pos, needs_reg, block);
            if let UseOperandConstraint::Fixed(preg) = use_op.constraint() {
                self.reserve_phys_reg(
                    preg,
                    ProgramRange::new(pos, op_pos),
                    instr,
                    Some(live_range),
                    // We completely disallow overlaps with reservations for any other uses.
                    false,
                );
            }
        }
    }

    fn reserve_phys_reg(
        &mut self,
        preg: PhysReg,
        prog_range: ProgramRange,
        instr: Instr,
        copied_live_range: Option<LiveRange>,
        allow_identical_range: bool,
    ) {
        let reservations = &mut self.phys_reg_reservations[preg.as_u8() as usize];

        // We're adding reservations in reverse order, so the last reservation should be either
        // above us or identical (when `allow_identical_range` is true).
        if let Some(last_reservation) = reservations.last() {
            if last_reservation.prog_range.intersects(prog_range) {
                if allow_identical_range {
                    // We don't support multiple live range copies per physical register reservation.
                    debug_assert!(copied_live_range.is_none());
                    assert!(
                        last_reservation.prog_range == prog_range,
                        "attempted to reserve physical register with overlapping ranges"
                    );

                    // We have nothing more to do in this case: the new reservation is "absorbed"
                    // into the existing one.
                    return;
                } else {
                    panic!("attempted to reserve physical register with overlapping ranges");
                }
            }

            // At this point, we can safely verify our assumption that reservations are being added
            // in reverse order.
            debug_assert!(last_reservation.prog_range.start >= prog_range.end);
        }

        reservations.push(PhysRegReservation {
            prog_range,
            copied_live_range: copied_live_range.into(),
        });

        if let Some(live_range) = copied_live_range {
            self.hint_live_range(live_range, preg, instr);
        }
    }

    fn hint_live_range(&mut self, live_range: LiveRange, preg: PhysReg, instr: Instr) {
        let hints = self.live_range_hints.entry(live_range).or_default();
        hints.push(AnnotatedPhysRegHint {
            hint: PhysRegHint {
                preg,
                weight: get_instr_weight(self.lir, self.cfg_ctx, instr),
            },
            instr,
        })
    }

    fn record_instr_def(
        &mut self,
        vreg: VirtRegNum,
        instr: Instr,
        needs_reg: bool,
        op_pos: Option<LiveRangeOpPos>,
        def_point: ProgramPoint,
        dead_use_point: ProgramPoint,
    ) -> Option<LiveRange> {
        let live_range = self.record_def(vreg, def_point, dead_use_point)?;
        self.live_ranges[live_range]
            .instrs
            .push(LiveRangeInstr::new(
                instr,
                get_instr_weight(self.lir, self.cfg_ctx, instr),
                true,
                needs_reg,
                op_pos,
            ));
        Some(live_range)
    }

    fn record_def(
        &mut self,
        vreg: VirtRegNum,
        def_point: ProgramPoint,
        dead_use_point: ProgramPoint,
    ) -> Option<LiveRange> {
        if let Some(&last_range) = self.vreg_ranges[vreg].last() {
            let last_range_data = &mut self.live_ranges[last_range];

            // This def is live *somewhere*, so it must be live in the current block as well. That
            // means it should already have a block-covering range created by a use somewhere else
            // in the block (possibly a live-out).
            assert!(def_point >= last_range_data.prog_range.start);

            // We're supposed to be doing a backward scan of the LIR, so this should always hold.
            assert!(def_point < last_range_data.prog_range.end);

            last_range_data.prog_range.start = def_point;
            Some(last_range)
        } else if def_point < dead_use_point {
            // If the range for a dead def would be non-empty, create it now.
            Some(self.open_vreg_live_range(vreg, def_point, dead_use_point))
        } else {
            // Otheriwse, the range is empty (degenerate). This can happen with dead physical-
            // register-constrained defs, which push the def point to before the next instruction.
            None
        }
    }

    fn record_live_use(
        &mut self,
        vreg: VirtRegNum,
        use_point: ProgramPoint,
        needs_reg: bool,
        block: Block,
    ) -> LiveRange {
        let instr = use_point.instr();
        let op_pos = LiveRangeOpPos::for_instr_slot(use_point.slot());
        let live_range = self.open_use_range(vreg, use_point, block);
        let range_instrs = &mut self.live_ranges[live_range].instrs;
        if let Some(last_instr) = range_instrs.last_mut() {
            if last_instr.instr() == instr {
                // This should be completely impossible because instructions can't use vregs they
                // define.
                assert!(!last_instr.is_def());
                let prev_op_pos = last_instr
                    .op_pos()
                    .expect("uses must always have an operand pos");

                // Avoid recording the same instruction multiple times if it uses this vreg in
                // several operands - just make sure to update the fields we'll need in case we want
                // to spill later:
                // * `op_pos` should always point to the latest use point in the instruction so
                //   ranges for reloaded spills are correct.
                // * `needs_reg` should be true so we avoid reloading things more than once.
                last_instr.set_op_pos(prev_op_pos.max(op_pos));
                last_instr.set_needs_reg(true);

                return live_range;
            }
        }

        range_instrs.push(LiveRangeInstr::new(
            instr,
            get_instr_weight(self.lir, self.cfg_ctx, instr),
            false,
            needs_reg,
            Some(op_pos),
        ));

        live_range
    }

    fn open_use_range(
        &mut self,
        vreg: VirtRegNum,
        use_point: ProgramPoint,
        block: Block,
    ) -> LiveRange {
        let block_start = ProgramPoint::before(self.lir.block_instrs(block).start);
        if let Some(&last_range) = self.vreg_ranges[vreg].last() {
            let last_prog_range = self.live_ranges[last_range].prog_range;
            debug_assert!(last_prog_range.start >= block_start);
            if last_prog_range.start == block_start {
                // If a later use in this block has already "opened" a range, just use it.
                return last_range;
            }
        }

        self.open_vreg_live_range(vreg, block_start, use_point)
    }

    fn open_live_out_range(&mut self, vreg: VirtRegNum, block: Block) {
        let block_range = self.lir.block_instrs(block);

        // Note: ranges are half-open, and we want this range to end *after* the last
        // instruction. Even if the last block in the function has live-outs (which is unusual
        // but not technically disallowed by regalloc), the range will refer to the virtual
        // past-the-end instruction.
        self.open_vreg_live_range(
            vreg,
            ProgramPoint::before(block_range.start),
            ProgramPoint::before(block_range.end),
        );
    }

    fn open_vreg_live_range(
        &mut self,
        vreg: VirtRegNum,
        start: ProgramPoint,
        end: ProgramPoint,
    ) -> LiveRange {
        debug_assert!(end > start);
        trace!("    assigning {start:?}..{end:?} to {vreg}");
        if let Some(&last_range) = self.vreg_ranges[vreg].last() {
            let last_prog_range = &mut self.live_ranges[last_range].prog_range;
            assert!(end <= last_prog_range.start);
            // If possible, merge with the next range instead of creating a new one.
            if end == last_prog_range.start {
                last_prog_range.start = start;
                return last_range;
            }
        }

        self.push_vreg_live_range(
            vreg,
            // This will be filled in later in `build_live_sets`.
            LiveSetFragment::reserved_value(),
            ProgramRange::new(start, end),
            smallvec![],
            false,
        )
    }
}

fn def_constraint_needs_reg(constraint: DefOperandConstraint) -> bool {
    // Note: we don't consider fixed operands as needing registers as they are being rewritten to
    // dedicated copies.
    constraint == DefOperandConstraint::AnyReg
}

fn compute_block_liveness<M: MachineRegalloc>(
    lir: &Lir<M>,
    cfg_ctx: &CfgContext,
) -> (
    SecondaryMap<Block, VirtRegSet>,
    SecondaryMap<Block, VirtRegSet>,
) {
    let mut raw_block_uses = make_block_vreg_map(cfg_ctx);
    let mut raw_block_defs = make_block_vreg_map(cfg_ctx);

    trace!("collecting block defs/uses");

    for &block in &cfg_ctx.block_order {
        trace!("  {block}");

        let raw_uses = &mut raw_block_uses[block];
        let raw_defs = &mut raw_block_defs[block];

        for &outgoing_vreg in lir.outgoing_block_params(block) {
            trace!("    use {}", outgoing_vreg);
            raw_uses.add(outgoing_vreg);
        }

        for instr in lir.block_instrs(block) {
            // We can process defs/uses in bulk here because everything is in SSA.
            for use_op in lir.instr_uses(instr) {
                trace!("    use {}", use_op.reg());
                raw_uses.add(use_op.reg());
            }

            for def_op in lir.instr_defs(instr) {
                trace!("    def {}", def_op.reg());
                raw_defs.add(def_op.reg());
            }
        }

        for &incoming_vreg in lir.block_params(block) {
            trace!("    def {}", incoming_vreg);
            raw_defs.add(incoming_vreg);
        }
    }

    let mut live_ins = make_block_vreg_map(cfg_ctx);
    let mut live_outs = make_block_vreg_map(cfg_ctx);

    let mut worklist: VecDeque<_> = cfg_ctx.block_order.iter().copied().rev().collect();
    let mut workset = FxHashSet::from_iter(worklist.iter().copied());

    trace!("solving block liveness");

    while let Some(block) = worklist.pop_front() {
        workset.remove(&block);

        trace!("  {block}");

        // Note: we can do the union/subtraction in bulk here instead of iterating over individual
        // instructions because the LIR is already in SSA.
        let live_in = &mut live_ins[block];
        live_in.clone_from(&live_outs[block]);
        live_in.union(&raw_block_uses[block]);
        live_in.subtract(&raw_block_defs[block]);

        for &pred in cfg_ctx.cfg.block_preds(block) {
            let status = live_outs[pred].union(live_in);
            if status.is_changed() && !workset.contains(&pred) {
                trace!("    predecessor {pred} changed, requeuing");
                worklist.push_back(pred);
                workset.insert(pred);
            }
        }
    }

    (live_ins, live_outs)
}

fn make_block_vreg_map(cfg_ctx: &CfgContext) -> SecondaryMap<Block, VirtRegSet> {
    let mut map = SecondaryMap::new();
    map.resize(cfg_ctx.block_order.len());
    map
}

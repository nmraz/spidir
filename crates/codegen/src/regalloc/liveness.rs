use alloc::collections::VecDeque;
use cranelift_entity::{packed_option::ReservedValue, SecondaryMap};
use fx_utils::FxHashSet;
use log::trace;
use smallvec::{smallvec, SmallVec};

use crate::{
    cfg::{Block, CfgContext},
    lir::{
        DefOperandConstraint, Instr, Lir, OperandPos, PhysReg, UseOperandConstraint, VirtRegNum,
        VirtRegSet,
    },
    machine::MachineCore,
};

use super::{
    types::{
        AnnotatedPhysRegHint, LiveRange, LiveRangeData, LiveRangeInstr, LiveRangePhysCopy,
        LiveSetFragment, PhysRegCopyDir, PhysRegHint, PhysRegReservation, ProgramPoint,
        ProgramRange, TaggedLiveRange,
    },
    utils::get_instr_weight,
    RegAllocContext,
};

impl<'a, M: MachineCore> RegAllocContext<'a, M> {
    pub fn compute_liveness(&mut self) {
        let live_outs = compute_block_liveouts(self.lir, self.cfg_ctx);

        trace!("computing precise live ranges");

        let mut tied_defs = SmallVec::<[bool; 4]>::new();

        for &block in self.cfg_ctx.block_order.iter().rev() {
            trace!("  {block}");

            let block_range = self.lir.block_instrs(block);
            let last_instr = block_range.end.prev();

            // In case we need to insert a copy just before a jump (either because of outgoing block
            // params or because of a split live range), we don't model the small live range of the
            // target block's vreg after the pre-copy before the jump to the target block, so we
            // need to make sure no unaccounted-for ranges might interfere with that small range.
            // For now, we achieve this by completely disallowing block terminators to define vregs.
            assert!(
                self.lir.instr_defs(last_instr).is_empty()
                    && self.lir.instr_clobbers(last_instr).is_empty()
            );

            for live_out in &live_outs[block] {
                trace!("    live-out {live_out}");
                self.open_live_out_range(live_out, block);
            }

            for &outgoing_vreg in self.lir.outgoing_block_params(block) {
                let outgoing_vreg = outgoing_vreg.reg_num();
                trace!("    outgoing param {outgoing_vreg}");

                // We can safely treat the last instruction in the block as using the value for
                // spill weight purposes, because we know the copy (if there is one) will
                // ultimately be placed here, as critical edges are already split.
                self.record_live_use(
                    outgoing_vreg,
                    last_instr,
                    ProgramPoint::pre_copy(last_instr),
                    block,
                );
            }

            for instr in block_range.into_iter().rev() {
                self.compute_instr_liveness(block, instr, &mut tied_defs);
            }

            for &incoming_vreg in self.lir.block_params(block) {
                let incoming_vreg = incoming_vreg.reg_num();
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
        }

        // Do the same thing for all physical register reservations.
        for preg in 0..M::phys_reg_count() {
            self.phys_reg_reservations[preg as usize].reverse();
        }

        // Add register hints for all live-in values.
        let first_instr = self.lir.all_instrs().start;
        for (&preg, &vreg) in self
            .lir
            .live_in_regs()
            .iter()
            .zip(self.lir.block_params(self.cfg_ctx.block_order[0]))
        {
            let Some(first_range) = self.vreg_ranges[vreg.reg_num()].first() else {
                continue;
            };

            self.hint_live_range(first_range.live_range, preg, first_instr);
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
            let op_def_point = if tied_defs[i] {
                ProgramPoint::pre_copy(instr)
            } else {
                ProgramPoint::for_operand(instr, def_op.pos())
            };

            let def_point = match def_op.constraint() {
                // If the value is constrained to a single physical register, insert a copy
                // before the next instruction and start the live range there.
                DefOperandConstraint::Fixed(_) => ProgramPoint::before(instr.next()),
                _ => op_def_point,
            };

            let live_range = self.record_def(
                def_op.reg().reg_num(),
                def_point,
                // If this def is dead, make sure its range always gets extended past the
                // end of the instruction so that dead early defs interfere with late defs.
                ProgramPoint::before(instr.next()),
            );

            if let DefOperandConstraint::Fixed(preg) = def_op.constraint() {
                let copied_live_range = live_range.map(|live_range| LiveRangePhysCopy {
                    live_range,
                    direction: PhysRegCopyDir::FromPhys,
                });

                self.reserve_phys_reg(
                    preg,
                    ProgramRange::new(op_def_point, def_point),
                    instr,
                    copied_live_range,
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
            let vreg = use_op.reg().reg_num();

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

            let live_range = self.record_live_use(vreg, instr, pos, block);
            if let UseOperandConstraint::Fixed(preg) = use_op.constraint() {
                self.reserve_phys_reg(
                    preg,
                    ProgramRange::new(pos, op_pos),
                    instr,
                    Some(LiveRangePhysCopy {
                        live_range,
                        direction: PhysRegCopyDir::ToPhys,
                    }),
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
        copied_live_range: Option<LiveRangePhysCopy>,
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
            copied_live_range,
        });

        if let Some(copied_live_range) = copied_live_range {
            self.hint_live_range(copied_live_range.live_range, preg, instr);
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

    fn record_def(
        &mut self,
        vreg: VirtRegNum,
        def_point: ProgramPoint,
        dead_use_point: ProgramPoint,
    ) -> Option<LiveRange> {
        if let Some(last_range) = self.vreg_ranges[vreg].last_mut() {
            // This def is live *somewhere*, so it must be live in the current block as well. That
            // means it should already have a block-covering range created by a use somewhere else
            // in the block (possibly a live-out).
            assert!(def_point >= last_range.prog_range.start);

            // We're supposed to be doing a backward scan of the LIR, so this should always hold.
            assert!(def_point < last_range.prog_range.end);

            last_range.prog_range.start = def_point;
            self.live_ranges[last_range.live_range].prog_range.start = def_point;
            Some(last_range.live_range)
        } else if def_point < dead_use_point {
            // If the range for a dead def would be non-empty, create it now.
            Some(self.create_vreg_live_range(vreg, def_point, dead_use_point))
        } else {
            // Otheriwse, the range is empty (degenerate). This can happen with dead physical-
            // register-constrained defs, which push the def point to before the next instruction.
            None
        }
    }

    fn record_live_use(
        &mut self,
        vreg: VirtRegNum,
        instr: Instr,
        use_point: ProgramPoint,
        block: Block,
    ) -> LiveRange {
        let live_range = self.create_use_range(vreg, use_point, block);
        let range_instrs = &mut self.live_ranges[live_range].instrs;
        if let Some(last_instr) = range_instrs.last() {
            if last_instr.pos == instr {
                // Avoid recording the same instruction multiple times if it uses this vreg in
                // several operands.
                return live_range;
            }
        }

        range_instrs.push(LiveRangeInstr {
            pos: instr,
            weight: get_instr_weight(self.lir, self.cfg_ctx, instr),
        });

        live_range
    }

    fn create_use_range(
        &mut self,
        vreg: VirtRegNum,
        use_point: ProgramPoint,
        block: Block,
    ) -> LiveRange {
        let block_start = ProgramPoint::before(self.lir.block_instrs(block).start);
        if let Some(last_range) = self.vreg_ranges[vreg].last() {
            debug_assert!(last_range.prog_range.start >= block_start);
            if last_range.prog_range.start == block_start {
                // If a later use in this block has already "opened" a range, just use it.
                return last_range.live_range;
            }
        }

        self.create_vreg_live_range(vreg, block_start, use_point)
    }

    fn open_live_out_range(&mut self, vreg: VirtRegNum, block: Block) {
        let block_range = self.lir.block_instrs(block);

        // Note: ranges are half-open, and we want this range to end *after* the last
        // instruction. Even if the last block in the function has live-outs (which is unusual
        // but not technically disallowed by regalloc), the range will refer to the virtual
        // past-the-end instruction.
        self.create_vreg_live_range(
            vreg,
            ProgramPoint::before(block_range.start),
            ProgramPoint::before(block_range.end),
        );
    }

    fn create_vreg_live_range(
        &mut self,
        vreg: VirtRegNum,
        start: ProgramPoint,
        end: ProgramPoint,
    ) -> LiveRange {
        debug_assert!(end > start);
        trace!("    assigning {start:?}..{end:?} to {vreg}");
        if let Some(last_range) = self.vreg_ranges[vreg].last_mut() {
            assert!(end <= last_range.prog_range.start);
            // If possible, merge with the next range instead of creating a new one.
            if end == last_range.prog_range.start {
                last_range.prog_range.start = start;
                self.live_ranges[last_range.live_range].prog_range.start = start;
                return last_range.live_range;
            }
        }

        let prog_range = ProgramRange::new(start, end);
        let range_data = LiveRangeData {
            prog_range,
            vreg,
            fragment: LiveSetFragment::reserved_value(),
            instrs: smallvec![],
        };
        let live_range = self.live_ranges.push(range_data);
        self.vreg_ranges[vreg].push(TaggedLiveRange {
            prog_range,
            live_range,
        });
        live_range
    }
}

fn compute_block_liveouts<M: MachineCore>(
    lir: &Lir<M>,
    cfg_ctx: &CfgContext,
) -> SecondaryMap<Block, VirtRegSet> {
    let mut raw_block_uses = make_block_vreg_map(lir, cfg_ctx);
    let mut raw_block_defs = make_block_vreg_map(lir, cfg_ctx);

    trace!("collecting block defs/uses");

    for &block in &cfg_ctx.block_order {
        trace!("  {block}");

        let raw_uses = &mut raw_block_uses[block];
        let raw_defs = &mut raw_block_defs[block];

        for &outgoing_vreg in lir.outgoing_block_params(block) {
            trace!("    use {}", outgoing_vreg.reg_num());
            raw_uses.add(outgoing_vreg.reg_num());
        }

        for instr in lir.block_instrs(block) {
            // We can process defs/uses in bulk here because everything is in SSA.
            for use_op in lir.instr_uses(instr) {
                trace!("    use {}", use_op.reg().reg_num());
                raw_uses.add(use_op.reg().reg_num());
            }

            for def_op in lir.instr_defs(instr) {
                trace!("    def {}", def_op.reg().reg_num());
                raw_defs.add(def_op.reg().reg_num());
            }
        }

        for &incoming_vreg in lir.block_params(block) {
            trace!("    def {}", incoming_vreg.reg_num());
            raw_defs.add(incoming_vreg.reg_num());
        }
    }

    let mut live_ins = make_block_vreg_map(lir, cfg_ctx);
    let mut live_outs = make_block_vreg_map(lir, cfg_ctx);

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

    live_outs
}

fn make_block_vreg_map<M: MachineCore>(
    lir: &Lir<M>,
    cfg_ctx: &CfgContext,
) -> SecondaryMap<Block, VirtRegSet> {
    let mut map = SecondaryMap::with_default(VirtRegSet::new(lir.vreg_count()));
    map.resize(cfg_ctx.block_order.len());
    map
}

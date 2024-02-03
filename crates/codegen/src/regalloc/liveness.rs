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
        LiveRange, LiveRangeData, LiveRangeInstr, LiveSetFragment, PhysRegCopy, ProgramPoint,
        ProgramRange, RangeEndKey, TaggedLiveRange,
    },
    RegAllocContext,
};

impl<'a, M: MachineCore> RegAllocContext<'a, M> {
    pub fn compute_live_ranges(&mut self) {
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
                // Note: we assume an instruction never uses vregs it defines.

                let defs = self.lir.instr_defs(instr);
                let uses = self.lir.instr_uses(instr);

                tied_defs.clear();
                tied_defs.resize(defs.len(), false);

                for use_op in uses {
                    let vreg = use_op.reg().reg_num();

                    let op_pos = ProgramPoint::for_operand(instr, use_op.pos()).next();
                    let pos = match use_op.constraint() {
                        UseOperandConstraint::TiedToDef(i) => {
                            let i = i as usize;
                            assert!(
                                !tied_defs[i],
                                "multiple uses tied to same def in instr {instr}"
                            );

                            let tied_def = defs[i];
                            assert!(use_op.pos() == OperandPos::Early);
                            assert!(tied_def.pos() == OperandPos::Late);

                            tied_defs[i] = true;
                            ProgramPoint::pre_copy(instr)
                        }
                        UseOperandConstraint::Fixed(preg) => {
                            // We model fixed register constraints as a pre-copy into the correct
                            // physical register, and directly reserve the relevant register for
                            // the correct range within the instruction.
                            let pre_copy = ProgramPoint::pre_copy(instr);
                            // We completely disallow overlaps with reservations for any other uses.
                            self.reserve_phys_reg(preg, ProgramRange::new(pre_copy, op_pos), false);
                            pre_copy
                        }
                        _ => {
                            // Note: ranges are half-open, so we actually mark the use as extending
                            // to the next program point.
                            op_pos
                        }
                    };

                    let live_range = self.record_live_use(vreg, instr, pos, block);
                    if let UseOperandConstraint::Fixed(preg) = use_op.constraint() {
                        self.pre_instr_preg_copies[instr].push(PhysRegCopy { live_range, preg });
                    }
                }

                for (i, def_op) in defs.iter().enumerate() {
                    let mut def_point = if tied_defs[i] {
                        ProgramPoint::pre_copy(instr)
                    } else {
                        ProgramPoint::for_operand(instr, def_op.pos())
                    };

                    // If the value is constrained to a single physical register, insert a copy
                    // before the next instruction and start the live range there.
                    if let DefOperandConstraint::Fixed(preg) = def_op.constraint() {
                        let copy_point = ProgramPoint::before(instr.next());
                        // There isn't really a good reason for multiple defs to refer to the same
                        // physical register - just use a single vreg instead.
                        self.reserve_phys_reg(
                            preg,
                            ProgramRange::new(def_point, copy_point),
                            false,
                        );
                        def_point = copy_point;
                    }

                    let live_range = self.record_def(
                        def_op.reg().reg_num(),
                        def_point,
                        // If this def is dead, make sure its range always gets extended past the
                        // end of the instruction so that dead early defs always interfere with
                        // late defs.
                        ProgramPoint::before(instr.next()),
                    );

                    if let (DefOperandConstraint::Fixed(preg), Some(live_range)) =
                        (def_op.constraint(), live_range)
                    {
                        self.post_instr_preg_copies[instr].push(PhysRegCopy { live_range, preg });
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
                        self.reserve_phys_reg(clobber, clobber_range, true);
                    }
                }
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
    }

    fn reserve_phys_reg(
        &mut self,
        preg: PhysReg,
        range: ProgramRange,
        allow_identical_range: bool,
    ) {
        let assignments = &mut self.phys_reg_assignments[preg.as_u8() as usize];

        // Recall that non-degenerate ranges intersect iff each range's start start point precedes
        // the other's end point. Our allocation tree is keyed by end point, so start by performing
        // the `new_range.start < other_ranges.end` comparison.
        if let Some((intersection_candidate, candidate_live_range)) =
            assignments.range(RangeEndKey::point(range.start)..).next()
        {
            // If we're allowing existing identical ranges (like what happens with register defs
            // and clobbers), check for and ignore that case now.
            if allow_identical_range && intersection_candidate.0 == range {
                assert!(candidate_live_range.is_none());
                return;
            }

            // Otherwise, we forbid intersection. The ranges really will intersect if the reverse
            // start/end comparison holds. If that is the case, report the incompatibility now.
            assert!(
                intersection_candidate.0.start >= range.end,
                "attempted to reserve physical register with overlapping ranges"
            );
        }

        assignments.insert(RangeEndKey(range), None.into());
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

fn get_instr_weight<M: MachineCore>(lir: &Lir<M>, cfg_ctx: &CfgContext, instr: Instr) -> f32 {
    let block = lir.instr_block(instr);
    let loop_depth = cfg_ctx
        .depth_map
        .loop_depth(cfg_ctx.domtree.get_tree_node(block).unwrap());
    1000f32 * ((loop_depth + 1) as f32)
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

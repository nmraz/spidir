use core::mem;

use log::{log_enabled, trace};
use smallvec::{smallvec, SmallVec};

use crate::{
    lir::Instr,
    machine::MachineRegalloc,
    regalloc::types::{
        InstrSlot, LiveRangeInstr, LiveRangeOpPos, LiveSetFragmentFlags, ProgramPoint, ProgramRange,
    },
};

use super::{
    context::RegAllocContext,
    types::{AnnotatedPhysRegHint, LiveRange, LiveSet, LiveSetFragment},
    utils::get_weight_at_instr,
};

impl<M: MachineRegalloc> RegAllocContext<'_, M> {
    pub fn spill_fragment_and_neighbors(&mut self, fragment: LiveSetFragment) {
        self.spill_fragment(fragment);

        // Spill any neighbors (in both directions) that don't have any attached instructions, or
        // have only attached instructions that are worth spilling anyway.

        let mut cursor = fragment;
        while let Some(prev) = self.live_set_fragments[cursor].prev_split_neighbor.expand() {
            if !self.should_spill_prev_neighbor(prev) {
                break;
            }

            if !self.is_fragment_spilled(prev) {
                self.evict_fragment(prev);
                self.spill_fragment(prev);
            }

            cursor = prev;
        }

        let mut cursor = fragment;
        while let Some(next) = self.live_set_fragments[cursor].next_split_neighbor.expand() {
            if !self.should_spill_next_neighbor(next) {
                break;
            }

            if !self.is_fragment_spilled(next) {
                self.evict_fragment(next);
                self.spill_fragment(next);
            }

            cursor = next;
        }
    }

    fn should_spill_prev_neighbor(&self, prev_neighbor: LiveSetFragment) -> bool {
        if !self.fragment_has_instrs(prev_neighbor) {
            // Empty neighbors are always worth spilling.
            return true;
        }

        if self.is_fragment_atomic(prev_neighbor) {
            // We're going to be touching fragments that have attached instructions now, so make
            // sure we never try to spill an atomic one by accident.
            return false;
        }

        // Spill neighbors that contain only a single def instruction, as long as we won't need to
        // copy into yet another neighbor and that instruction won't be executed more frequently
        // than our current split point.

        if self.live_set_fragments[prev_neighbor]
            .prev_split_neighbor
            .is_some()
        {
            return false;
        }

        if let Some(instr) = self.fragment_only_instr(prev_neighbor) {
            if instr.is_def() {
                let end = self.fragment_hull(prev_neighbor).end.instr();
                return get_weight_at_instr(self.lir, self.cfg_ctx, instr.instr())
                    <= get_weight_at_instr(self.lir, self.cfg_ctx, end);
            }
        }

        false
    }

    fn should_spill_next_neighbor(&self, next_neighbor: LiveSetFragment) -> bool {
        if !self.fragment_has_instrs(next_neighbor) {
            // Empty neighbors are always worth spilling.
            return true;
        }

        if self.is_fragment_atomic(next_neighbor) {
            // We're going to be touching fragments that have attached instructions now, so make
            // sure we never try to spill an atomic one by accident.
            return false;
        }

        // Spill neighbors that contain only a single use instruction, as long as we won't need to
        // copy into yet another neighbor and that instruction won't be executed more frequently
        // than our current split point.

        if self.live_set_fragments[next_neighbor]
            .next_split_neighbor
            .is_some()
        {
            return false;
        }

        if let Some(instr) = self.fragment_only_instr(next_neighbor) {
            if !instr.is_def() {
                let start = self.fragment_hull(next_neighbor).start.instr();
                return get_weight_at_instr(self.lir, self.cfg_ctx, instr.instr())
                    <= get_weight_at_instr(self.lir, self.cfg_ctx, start);
            }
        }

        false
    }

    fn spill_fragment(&mut self, fragment: LiveSetFragment) {
        trace!("  spill: {fragment}");

        debug_assert!(!self.is_fragment_atomic(fragment));

        let live_set = self.live_set_fragments[fragment].live_set;

        let mut ranges = mem::take(&mut self.live_set_fragments[fragment].ranges);

        let mut reg_instrs = SmallVec::<[LiveRangeInstr; 4]>::new();
        let mut new_fragments = SmallVec::<[LiveSetFragment; 8]>::new();

        let mut last_instr = None;
        for range in ranges.drain(..) {
            let vreg = self.live_ranges[range.live_range].vreg;
            let can_remat = self.remattable_vreg_defs[vreg].is_some();

            if log_enabled!(log::Level::Trace) {
                trace!("    range {:?}:", range.prog_range);
                for instr in &self.live_ranges[range.live_range].instrs {
                    trace!("      {:?}", instr);
                }
            }

            let instrs = &mut self.live_ranges[range.live_range].instrs;

            reg_instrs.clear();
            if can_remat {
                // We're rematerializing, so push all uses into registers in preparation and kill
                // any definitions; we won't need them anymore.

                reg_instrs.extend(
                    instrs
                        .drain_filter(|instr| !instr.is_def())
                        .map(|mut instr| {
                            // We need these new connector ranges in registers because we don't want
                            // them to ever be "spilled" again; that would just attempt another
                            // remat and end up creating the same ranges.
                            instr.set_needs_reg(true);
                            instr
                        }),
                );

                // There should really only be one definition left at this point because everything
                // is in SSA, but we don't actually care. Note, however, that we can only kill the
                // definition if it belongs to the fragment being spilled; if we're spilling a
                // different fragment that has been split off, we don't know whether the original
                // fragment will be spilled as well.
                for def in instrs.drain(..) {
                    debug_assert!(def.instr() == self.remattable_vreg_defs[vreg].unwrap());
                    trace!("    kill: {}", def.instr());
                    self.killed_remat_defs.insert(def.instr());
                }
            } else {
                // We're not rematerializing: split off all register uses, leaving anything that can
                // be rewritten as a stack access attached to the original spill range.
                reg_instrs.extend(instrs.drain_filter(|instr| instr.needs_reg()));
            }

            let hints = self
                .live_range_hints
                .remove(&range.live_range)
                .unwrap_or_default();
            let mut hints = &hints[..];

            // Note: we assume the instructions are all in sorted order here so that the
            // `last_instr` checks inside make sense.
            for instr in &reg_instrs {
                let op_pos = instr
                    .op_pos()
                    .expect("register operand must have a position");

                let new_prog_range = if instr.is_def() {
                    let start_slot = match op_pos {
                        LiveRangeOpPos::PreCopy => InstrSlot::PreCopy,
                        LiveRangeOpPos::Early => InstrSlot::Early,
                        LiveRangeOpPos::Late => InstrSlot::Late,
                    };
                    ProgramRange::new(
                        ProgramPoint::new(instr.instr(), start_slot),
                        ProgramPoint::before(instr.instr().next()),
                    )
                } else {
                    let end_slot = match op_pos {
                        LiveRangeOpPos::PreCopy => InstrSlot::PreCopy,
                        LiveRangeOpPos::Early => InstrSlot::Early,
                        LiveRangeOpPos::Late => InstrSlot::Late,
                    };
                    ProgramRange::new(
                        ProgramPoint::before(instr.instr()),
                        ProgramPoint::new(instr.instr(), end_slot),
                    )
                };

                let new_fragment = if last_instr == Some(instr.instr()) {
                    // Having two ranges from the same instruction is possible for tied/coalesced
                    // operands. Make sure to assign all those ranges to the same fragment to avoid
                    // potentially increased register usage and redundant copies.
                    *new_fragments.last().unwrap()
                } else {
                    let new_fragment = self.create_live_fragment(live_set, smallvec![]);
                    new_fragments.push(new_fragment);
                    new_fragment
                };

                trace!(
                    "    carve: {} ({new_prog_range:?}) -> {new_fragment}",
                    instr.instr()
                );

                // Note: `vreg_ranges` will no longer be sorted by range order once we do this, but
                // we don't care within the assignment loop.
                let new_live_range = self.push_vreg_fragment_live_range(
                    vreg,
                    new_fragment,
                    new_prog_range,
                    smallvec![*instr],
                    true,
                );
                self.set_range_hints_for_instr(new_live_range, instr.instr(), &mut hints);

                last_instr = Some(instr.instr());
            }

            // If this range actually needs to live on the stack, make sure to record that fact.
            if !can_remat {
                self.expand_spill_hull(live_set, range.prog_range);
            }
        }

        for &new_fragment in &new_fragments {
            self.compute_live_fragment_properties(new_fragment);
            self.enqueue_fragment(new_fragment);
        }

        // Remember we've spilled this fragment so we don't try to touch it again in the future.
        self.live_set_fragments[fragment]
            .flags
            .insert(LiveSetFragmentFlags::SPILLED);
    }

    fn set_range_hints_for_instr(
        &mut self,
        live_range: LiveRange,
        instr: Instr,
        hints: &mut &[AnnotatedPhysRegHint],
    ) {
        // We expect the hints to almost always be empty, so leave in one very predictable branch
        // before the more complex logic.
        if hints.is_empty() {
            return;
        }

        while hints.first().is_some_and(|hint| hint.instr < instr) {
            *hints = &hints[1..];
        }

        let instr_hint_len = hints
            .iter()
            .position(|hint| hint.instr != instr)
            .unwrap_or(hints.len());
        let (instr_hints, remaining_hints) = hints.split_at(instr_hint_len);
        *hints = remaining_hints;

        self.live_range_hints.insert(live_range, instr_hints.into());
    }

    fn expand_spill_hull(&mut self, live_set: LiveSet, range: ProgramRange) {
        let set_spill_hull = &mut self.live_sets[live_set].spill_hull;
        match set_spill_hull {
            Some(existing_hull) => {
                existing_hull.start = existing_hull.start.min(range.start);
                existing_hull.end = existing_hull.end.max(range.end);
            }
            None => {
                *set_spill_hull = Some(range);
            }
        }
    }
}

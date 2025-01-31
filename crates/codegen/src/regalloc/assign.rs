use core::{cmp::Ordering, f32, mem};

use hashbrown::hash_map::Entry;
use itertools::Itertools;
use log::{log_enabled, trace};
use smallvec::{smallvec, SmallVec};

use crate::{
    lir::{Instr, PhysReg, PhysRegSet, VirtReg},
    machine::MachineRegalloc,
    regalloc::types::{
        AnnotatedPhysRegHint, InstrSlot, LiveRangeInstr, LiveRangeOpPos, LiveSetFragmentFlags,
        ProgramPoint, TaggedLiveRange, FRAGMENT_PRIO_HINTED,
    },
};

use super::{
    conflict::{iter_btree_ranges, iter_conflicts, iter_slice_ranges},
    context::RegAllocContext,
    types::{
        LiveRange, LiveRangeInstrs, LiveSet, LiveSetFragment, ProgramRange, QueuedFragment,
        RangeEndKey,
    },
    utils::{coalesce_slice, get_block_weight, get_weight_at_instr},
    RegallocError,
};

#[derive(Debug, Clone, Copy)]
enum ConflictBoundary {
    StartsAt(Instr),
    EndsAt(Instr),
}

impl ConflictBoundary {
    fn instr(self) -> Instr {
        match self {
            ConflictBoundary::StartsAt(instr) => instr,
            ConflictBoundary::EndsAt(instr) => instr,
        }
    }
}

enum ProbeConflict {
    Soft {
        fragments: SmallVec<[LiveSetFragment; 4]>,
        weight: f32,
        hint_weight: f32,
    },
    Hard {
        boundary: Option<ConflictBoundary>,
    },
}

#[derive(Clone, Copy)]
struct ProbeHint {
    preg: PhysReg,
    hint_weight: f32,
    sort_weight: f32,
}

type ProbeHints = SmallVec<[ProbeHint; 8]>;

impl<M: MachineRegalloc> RegAllocContext<'_, M> {
    pub fn assign_all_fragments(&mut self) -> Result<(), RegallocError> {
        // Process fragments in order of decreasing size, to try to fill in the larger ranges before
        // moving on to smaller ones. Because of weight-based eviction, we can still end up
        // revisiting a larger fragment later.
        for fragment in self.live_set_fragments.keys() {
            if self.live_set_fragments[fragment].ranges.is_empty() {
                continue;
            }

            self.enqueue_fragment(fragment);
        }

        while let Some(queued_fragment) = self.dequeue_fragment() {
            let fragment = queued_fragment.fragment;
            trace!(
                "process: {fragment}, prio {}, weight {}",
                queued_fragment.prio,
                self.live_set_fragments[fragment].spill_weight,
            );
            self.try_assign(fragment)?;
        }

        // Splitting and spilling may have created new, unsorted vreg ranges, so make sure to sort
        // them back before someone notices.
        self.sort_vreg_ranges();

        Ok(())
    }

    fn sort_vreg_ranges(&mut self) {
        for ranges in self.vreg_ranges.values_mut() {
            ranges.sort_unstable_by_key(|&range| {
                let range_data = &self.live_ranges[range];
                let fragment_data = &self.live_set_fragments[range_data.fragment];

                // Allow ranges to overlap (for spill connectors), but make sure reload connectors
                // starting with a spill always come after spills starting at the same point.
                ((range_data.prog_range.start.index() as u64) << 1)
                    | (fragment_data.assignment.is_some() as u64)
            });
        }
    }

    fn try_assign(&mut self, fragment: LiveSetFragment) -> Result<(), RegallocError> {
        let live_set = self.live_set_fragments[fragment].live_set;
        let class = self.live_sets[live_set].class;

        let mut hints = ProbeHints::new();
        self.collect_probe_hints(fragment, &mut hints);

        // Start with hinted registers in order of decreasing weight, then move on to the default
        // allocation order requested by the machine backend.
        let probe_order = hints
            .iter()
            .map(|hint| (hint.preg, hint.hint_weight))
            .chain(
                self.machine
                    .usable_regs(class)
                    .iter()
                    .map(|&preg| (preg, 0.0)),
            );

        let mut no_conflict_reg = None;
        let mut lightest_soft_conflict = None;
        let mut lightest_soft_conflict_weight = f32::INFINITY;
        let mut earliest_hard_conflict_boundary: Option<ConflictBoundary> = None;

        let mut probed_regs = PhysRegSet::empty();
        for (preg, hint_weight) in probe_order {
            if probed_regs.contains(preg) {
                continue;
            }

            probed_regs.insert(preg);

            trace!("  probe: {} (hint weight {hint_weight})", M::reg_name(preg));
            match self.probe_phys_reg(preg, fragment) {
                None => {
                    trace!("    no conflict");
                    no_conflict_reg = Some((preg, hint_weight));
                    break;
                }
                Some(ProbeConflict::Soft {
                    fragments,
                    weight: conflict_weight,
                    hint_weight: conflict_hint_weight,
                }) => {
                    trace!(
                        "    soft conflict with {} (weight {conflict_weight}, hint weight {conflict_hint_weight})",
                        fragments.iter().format(", ")
                    );

                    if hint_weight > conflict_hint_weight {
                        // If we can evict all overlapping fragments assigned to this register and
                        // prefer it more stronly than all of them (based on hint weight), go ahead
                        // and evict them now. This helps deal with the case where a preferred
                        // register happens to be taken by a fragment that doesn't actually need
                        // that particular register, in which case we might have otherwise ended up
                        // choosing an unrelated free register.
                        lightest_soft_conflict = Some((preg, hint_weight, fragments));
                        break;
                    }

                    if conflict_weight < lightest_soft_conflict_weight {
                        lightest_soft_conflict = Some((preg, hint_weight, fragments));
                        lightest_soft_conflict_weight = conflict_weight;
                    }
                }
                Some(ProbeConflict::Hard { boundary, .. }) => {
                    trace!("    hard conflict with boundary {boundary:?}");

                    // This boundary is interesting for splitting if it can non-degenerately split
                    // the fragment in two.
                    let split_boundary = boundary.filter(|boundary| {
                        self.can_split_fragment_before(fragment, boundary.instr())
                    });
                    if let Some(split_boundary) = split_boundary {
                        match earliest_hard_conflict_boundary {
                            Some(cur_boundary) => {
                                if split_boundary.instr() < cur_boundary.instr() {
                                    earliest_hard_conflict_boundary = Some(split_boundary);
                                }
                            }
                            None => {
                                earliest_hard_conflict_boundary = Some(split_boundary);
                            }
                        }
                    }
                }
            }
        }

        if let Some((no_conflict_reg, hint_weight)) = no_conflict_reg {
            self.assign_fragment_to_phys_reg(fragment, no_conflict_reg, hint_weight);
            return Ok(());
        }

        if let Some((preg, hint_weight, soft_conflicts)) = lightest_soft_conflict {
            for conflicting_fragment in soft_conflicts {
                self.evict_fragment(conflicting_fragment);
                self.enqueue_fragment(conflicting_fragment);
            }
            self.assign_fragment_to_phys_reg(fragment, preg, hint_weight);
            return Ok(());
        }

        if let Some(boundary) = earliest_hard_conflict_boundary {
            if self.try_split_fragment_for_conflict(fragment, boundary) {
                return Ok(());
            }
        }

        if !self.is_fragment_atomic(fragment) {
            self.spill_fragment_and_neighbors(fragment);
            return Ok(());
        }

        let instr = self.fragment_hull(fragment).start.instr();
        Err(RegallocError::OutOfRegisters(instr))
    }

    fn collect_probe_hints(&self, fragment: LiveSetFragment, hints: &mut ProbeHints) {
        hints.extend(
            self.live_set_fragments[fragment]
                .hints
                .iter()
                .map(|hint| ProbeHint {
                    preg: hint.preg,
                    hint_weight: hint.weight,
                    sort_weight: hint.weight,
                }),
        );

        let prev_fragment = self.live_set_fragments[fragment].prev_split_neighbor;
        let next_fragment = self.live_set_fragments[fragment].next_split_neighbor;

        // Avoid all the extra shuffling/sorting work when we don't need it, as we know the original
        // hints are sorted by weight.
        if prev_fragment.is_none() && next_fragment.is_none() {
            return;
        }

        // Collect hints from neighboring fragments if they have already been assigned. Note that
        // we never mark these as having "proper" hint weights for allocation tracking, as the
        // neighbors themselves might be evicted later.

        let hull = self.fragment_hull(fragment);

        if let Some(prev_fragment) = prev_fragment.expand() {
            if let Some(prev_assignment) =
                self.live_set_fragments[prev_fragment].assignment.expand()
            {
                let weight = get_weight_at_instr(self.lir, self.cfg_ctx, hull.start.instr());
                hints.push(ProbeHint {
                    preg: prev_assignment,
                    hint_weight: 0.0,
                    sort_weight: weight,
                });
            }
        }

        if let Some(next_fragment) = next_fragment.expand() {
            if let Some(next_assignment) =
                self.live_set_fragments[next_fragment].assignment.expand()
            {
                let weight = get_weight_at_instr(self.lir, self.cfg_ctx, hull.end.instr());
                hints.push(ProbeHint {
                    preg: next_assignment,
                    hint_weight: 0.0,
                    sort_weight: weight,
                });
            }
        }

        sort_probe_hints(hints);
    }

    fn spill_fragment_and_neighbors(&mut self, fragment: LiveSetFragment) {
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

    fn try_split_fragment_for_conflict(
        &mut self,
        fragment: LiveSetFragment,
        boundary: ConflictBoundary,
    ) -> bool {
        trace!("  try split: {fragment} for conflict {boundary:?}");

        // To avoid pointlessly chopping everything up into tiny pieces, only split fragments that
        // actually contain instructions using their values; ones that just carry the value
        // elsewhere can be spilled.
        if !self.fragment_has_instrs(fragment) {
            return false;
        }

        if let Some(global_split) = self.global_split_point(fragment) {
            trace!("    found global split: {global_split}");
            self.split_fragment_before(fragment, global_split);
            return true;
        }

        if let Some(conflict_split) = self.split_point_for_conflict(fragment, boundary) {
            trace!("    found conflict split: {conflict_split}");
            self.split_fragment_before(fragment, conflict_split);
            return true;
        };

        false
    }

    fn global_split_point(&self, fragment: LiveSetFragment) -> Option<Instr> {
        if !self.is_fragment_global(fragment) {
            return None;
        }

        let hull = self.fragment_hull(fragment);
        let start = hull.start.instr();
        let end = hull.end.instr();

        let mut block_idx = self.lir.instr_block_index(start);
        let mut last_block = self.cfg_ctx.block_order[block_idx];
        let mut last_weight = get_block_weight(self.cfg_ctx, last_block);

        let mut min_weight = f32::INFINITY;
        let mut split_point = None;

        // Note: the increment in the first iteration of the loop is always safe because this
        // fragment spans at least two blocks.
        loop {
            block_idx += 1;

            let block = self.cfg_ctx.block_order[block_idx];
            let block_start = self.lir.block_instrs(block).start;

            if block_start >= end {
                break;
            }

            let weight = get_block_weight(self.cfg_ctx, block);
            match last_weight.partial_cmp(&weight).unwrap() {
                Ordering::Less if last_weight < min_weight => {
                    // We've found a transition from a low-weight block to a higher-weight block,
                    // with the low weight having reached a new minimum across our walk. Split just
                    // before the end of the low-weight block to circumvent the high-weight one.
                    let last_terminator = self.lir.block_terminator(last_block);
                    if hull.can_split_before(ProgramPoint::before(last_terminator)) {
                        split_point = Some(last_terminator);
                        min_weight = last_weight;
                    }
                }
                Ordering::Greater if weight < min_weight => {
                    // We've found a transition from a high-weight block to a lower-weight block,
                    // with the low weight having reached a new minimum across our walk. Split just
                    // before the start of the low-weight block, noting that any copies inserted
                    // later will never be placed in a more deeply-nested loop.
                    if hull.can_split_before(ProgramPoint::before(block_start)) {
                        split_point = Some(block_start);
                        min_weight = weight;
                    }
                }
                _ => {}
            }

            last_block = block;
            last_weight = weight;
        }

        split_point
    }

    fn split_point_for_conflict(
        &self,
        fragment: LiveSetFragment,
        boundary: ConflictBoundary,
    ) -> Option<Instr> {
        if self.is_fragment_split(fragment) && self.fragment_only_instr(fragment).is_some() {
            // We don't want to repeatedly split single-instruction fragments on conflict
            // boundaries, as they can get us into pointless eviction/splitting/shuffling fights
            // under high register pressure. It's better to just give up early and spill in this
            // case; it frequently even reduces total spill count because there aren't a bunch of
            // small pieces to move around.
            return None;
        }

        match boundary {
            ConflictBoundary::StartsAt(instr) => {
                let last_instr_below = self
                    .fragment_instrs(fragment)
                    // We always split *before* the selected instruction, but we want to split *after*
                    // the last use here.
                    .map(|frag_instr| frag_instr.instr().next())
                    .take_while(|&frag_instr| frag_instr < instr)
                    .last();

                let instr = last_instr_below.unwrap_or(instr);

                self.can_split_fragment_before(fragment, instr)
                    .then_some(instr)
            }
            ConflictBoundary::EndsAt(instr) => {
                let first_instr_above = self
                    .fragment_instrs(fragment)
                    .map(|frag_instr| frag_instr.instr())
                    .find(|&frag_instr| frag_instr > instr);

                let instr = first_instr_above.unwrap_or(instr);

                self.can_split_fragment_before(fragment, instr)
                    .then_some(instr)
            }
        }
    }

    fn split_fragment_before(&mut self, fragment: LiveSetFragment, instr: Instr) {
        trace!(
            "  split: {fragment} (hull {:?}) at {instr}",
            self.fragment_hull(fragment)
        );

        debug_assert!(self.can_split_fragment_before(fragment, instr));

        let pos = ProgramPoint::before(instr);

        let (split_idx, split_boundary_range) = match self.live_set_fragments[fragment]
            .ranges
            .binary_search_by_key(&pos, |range| range.prog_range.start)
        {
            Ok(i) => {
                // We've hit the exact start of a range, so move it and all ranges above it into the
                // new fragment.
                (i, false)
            }
            Err(i) => {
                debug_assert!(i > 0, "split point should lie within some fragment range");
                let prev_range_end = self.live_set_fragments[fragment].ranges[i - 1]
                    .prog_range
                    .end;

                if pos < prev_range_end {
                    // We've hit the middle of a range, so leave everything below it unchanged,
                    // split it appropriately, and move everything above it into the new fragment.
                    // The splitting is performed by moving the range into the new fragment,
                    // adjusting it there, and adding a new range to cover the first part to the
                    // original fragment.
                    (i - 1, true)
                } else {
                    // We lie outside the nearest range starting below us, so leave it intact
                    // and move everything above it into the new fragment.
                    (i, false)
                }
            }
        };

        let live_set = self.live_set_fragments[fragment].live_set;
        let new_ranges = self.live_set_fragments[fragment]
            .ranges
            .drain(split_idx..)
            .collect();
        let new_fragment = self.create_live_fragment(live_set, new_ranges);

        if split_boundary_range {
            self.split_fragment_boundary_range_before(fragment, new_fragment, instr);
        }

        self.live_set_fragments[new_fragment].prev_split_neighbor = fragment.into();
        self.live_set_fragments[new_fragment].next_split_neighbor =
            self.live_set_fragments[fragment].next_split_neighbor;
        self.live_set_fragments[fragment].next_split_neighbor = new_fragment.into();

        self.compute_live_fragment_properties(fragment);
        self.compute_live_fragment_properties(new_fragment);

        self.enqueue_fragment(fragment);
        self.enqueue_fragment(new_fragment);
    }

    fn split_fragment_boundary_range_before(
        &mut self,
        old_fragment: LiveSetFragment,
        new_fragment: LiveSetFragment,
        instr: Instr,
    ) {
        let pos = ProgramPoint::before(instr);

        // The range to be split should already reside in the new fragment at this point.
        let split_range = &mut self.live_set_fragments[new_fragment].ranges[0];
        let split_live_range = split_range.live_range;
        let split_prog_range = split_range.prog_range;

        let low_range = ProgramRange::new(split_prog_range.start, pos);
        let high_range = ProgramRange::new(pos, split_prog_range.end);

        trace!("splitting range {split_prog_range:?} into {low_range:?} and {high_range:?}");

        // Update the relevant fields in the original live range to describe the upper part of
        // the split.
        split_range.prog_range = high_range;
        self.live_ranges[split_live_range].prog_range = high_range;

        // Split the attached instructions on the boundary using a simple linear traversal: we
        // basically need to walk the entire instruction list anyway, and a linear traversal is
        // probably better for cache locality.

        let mut instrs = mem::take(&mut self.live_ranges[split_live_range].instrs);
        let instr_split_idx = instrs
            .iter()
            // Note: we're splitting before `instr`, so we want `instr` itself to be part of
            // the upper half of the range.
            .position(|range_instr| range_instr.instr() >= instr);
        let high_instrs = if let Some(instr_split_idx) = instr_split_idx {
            instrs.drain(instr_split_idx..).collect()
        } else {
            smallvec![]
        };

        self.live_ranges[split_live_range].instrs = high_instrs;

        // `instrs` itself now contains the "low" instructions.

        let vreg = self.live_ranges[split_live_range].vreg;

        // Create a new live range for the lower part at the end of the `old_fragment`.
        // Note: `vreg_ranges` will no longer be sorted by range order once we do this, but we
        // don't care within the assignment loop.
        let new_live_range =
            self.push_vreg_fragment_live_range(vreg, old_fragment, low_range, instrs, false);

        // If this range came with any attached register hints, split them as well.
        if let Entry::Occupied(mut entry) = self.live_range_hints.entry(split_live_range) {
            // This will end up containing only the low range's hints once we drain the high ones
            // below.
            let mut low_hints = mem::take(entry.get_mut());

            // Once again, hints pertaining to `instr` should end up in the upper half of the split
            // range and not the lower half.
            let split_idx = low_hints.iter().position(|hint| hint.instr >= instr);

            if let Some(split_idx) = split_idx {
                let high_hints: SmallVec<[AnnotatedPhysRegHint; 2]> =
                    low_hints.drain(split_idx..).collect();
                if !high_hints.is_empty() {
                    *entry.get_mut() = high_hints;
                } else {
                    entry.remove();
                }
            };

            if !low_hints.is_empty() {
                self.live_range_hints.insert(new_live_range, low_hints);
            }
        }
    }

    fn probe_phys_reg(&self, preg: PhysReg, fragment: LiveSetFragment) -> Option<ProbeConflict> {
        // See if we conflict with a reservation (derived from an operand constraint) first. If
        // everything there checks out, move on to comparing with other fragments assigned to the
        // register.
        self.probe_phys_reservation(preg, fragment)
            .or_else(|| self.probe_phys_assignment(preg, fragment))
    }

    fn probe_phys_reservation(
        &self,
        preg: PhysReg,
        fragment: LiveSetFragment,
    ) -> Option<ProbeConflict> {
        let reservations = iter_slice_ranges(
            &self.phys_reg_reservations[preg.as_u8() as usize],
            |reservation| (reservation.prog_range, &reservation.copied_live_range),
        );
        let fragment_ranges = iter_slice_ranges(
            &self.live_set_fragments[fragment].ranges,
            |fragment_range| (fragment_range.prog_range, &fragment_range.live_range),
        );

        for ((reservation_range, reservation_copy), (fragment_range, fragment_live_range)) in
            iter_conflicts(reservations, fragment_ranges)
        {
            // Part of the fragment conflicts with a physical reservation. This is actually
            // allowed if the reservation is copied out of the intersecting live range: that
            // just means the range is live-through the instruction using it in a physical
            // register, but we can keep using that register for the live range after the
            // instruction as well.

            // A simple example of this is signed division on x64. The LIR sequence for the
            // division might look like this:
            //
            // func @sdiv64 {
            // block0[%1:gpr($rdi), %2:gpr($rsi)]:
            //     %3:gpr($rdx)[late] = ConvertWord(Cqo) %1($rax)[early]
            //     %0:gpr($rax)[late], %4:gpr($rdx)[late] = Idiv(S64) %1($rax)[early], %3($rdx)[early], %2(any)[early]
            //     Ret %0($rax)[early]
            // }
            //
            // We want to place `%1` in `$rax` for both the `cqo` and the `div`, but `$rax` is
            // reserved for the `cqo`, so a naive check would return a conflict. That case is
            // okay, though, because the reservation of `$rax` is copied out of `%1`.

            if reservation_copy.expand() != Some(fragment_live_range) {
                return Some(ProbeConflict::Hard {
                    boundary: conflict_boundary(reservation_range, fragment_range),
                });
            }
        }

        None
    }

    fn probe_phys_assignment(
        &self,
        preg: PhysReg,
        fragment: LiveSetFragment,
    ) -> Option<ProbeConflict> {
        let preg_ranges = iter_btree_ranges(&self.phys_reg_assignments[preg.as_u8() as usize]);
        let fragment_ranges = iter_slice_ranges(
            &self.live_set_fragments[fragment].ranges,
            |fragment_range| (fragment_range.prog_range, &fragment_range.live_range),
        );

        let fragment_weight = self.live_set_fragments[fragment].spill_weight;

        let mut soft_conflicts = SmallVec::new();
        let mut soft_conflict_weight = 0f32;
        let mut soft_conflict_hint_weight = 0f32;

        let mut conflict_scratch = self.fragment_conflict_scratch.borrow_mut();
        conflict_scratch.clear();

        for ((preg_range, preg_assignment), (fragment_range, _)) in
            iter_conflicts(preg_ranges, fragment_ranges)
        {
            let allocated_fragment = self.live_ranges[preg_assignment].fragment;
            if conflict_scratch.contains(allocated_fragment) {
                // Conflict already recorded.
                continue;
            }

            conflict_scratch.insert(allocated_fragment);

            let fragment_data = &self.live_set_fragments[allocated_fragment];

            let allocated_weight = fragment_data.spill_weight;
            if allocated_weight >= fragment_weight {
                return Some(ProbeConflict::Hard {
                    boundary: conflict_boundary(preg_range, fragment_range),
                });
            }

            soft_conflicts.push(allocated_fragment);
            soft_conflict_weight = soft_conflict_weight.max(allocated_weight);
            soft_conflict_hint_weight =
                soft_conflict_hint_weight.max(fragment_data.assignment_hint_weight);
        }

        if !soft_conflicts.is_empty() {
            Some(ProbeConflict::Soft {
                fragments: soft_conflicts,
                weight: soft_conflict_weight,
                hint_weight: soft_conflict_hint_weight,
            })
        } else {
            None
        }
    }

    fn assign_fragment_to_phys_reg(
        &mut self,
        fragment: LiveSetFragment,
        preg: PhysReg,
        hint_weight: f32,
    ) {
        trace!("  assign: {fragment} -> {}", M::reg_name(preg));
        let data = &mut self.live_set_fragments[fragment];
        let preg_assignments = &mut self.phys_reg_assignments[preg.as_u8() as usize];
        assert!(data.assignment.is_none());
        data.assignment = preg.into();
        data.assignment_hint_weight = hint_weight;
        for range in &data.ranges {
            preg_assignments.insert(RangeEndKey(range.prog_range), range.live_range);
        }
    }

    fn evict_fragment(&mut self, fragment: LiveSetFragment) {
        let data = &mut self.live_set_fragments[fragment];
        let Some(preg) = data.assignment.take() else {
            return;
        };

        trace!("  evict: {fragment} from {}", M::reg_name(preg));
        let preg_assignments = &mut self.phys_reg_assignments[preg.as_u8() as usize];
        for range in &data.ranges {
            preg_assignments
                .remove(&RangeEndKey(range.prog_range))
                .unwrap();
        }
    }

    fn dequeue_fragment(&mut self) -> Option<QueuedFragment> {
        loop {
            let queued_fragment = self.worklist.pop()?;

            // We may have spilled this fragment while it was waiting in the queue; just skip it.
            if self.is_fragment_spilled(queued_fragment.fragment) {
                continue;
            }

            return Some(queued_fragment);
        }
    }

    fn enqueue_fragment(&mut self, fragment: LiveSetFragment) {
        let fragment_data = &self.live_set_fragments[fragment];

        let mut prio = fragment_data.size;
        if !fragment_data.hints.is_empty() {
            // Always try to allocate hinted fragments first, regardless of size. This won't hurt
            // packing ability too much, since hinted fragments are usually attached to physical
            // reservations anyway, so the corresponding register will already be unavailable in
            // some places.
            prio |= FRAGMENT_PRIO_HINTED;
        }

        trace!(
            "enqueue: {fragment}, prio {prio}, weight {}",
            fragment_data.spill_weight
        );
        self.worklist.push(QueuedFragment { fragment, prio });
    }

    fn push_vreg_fragment_live_range(
        &mut self,
        vreg: VirtReg,
        fragment: LiveSetFragment,
        prog_range: ProgramRange,
        instrs: LiveRangeInstrs,
        is_spill_connector: bool,
    ) -> LiveRange {
        let live_range =
            self.push_vreg_live_range(vreg, fragment, prog_range, instrs, is_spill_connector);
        self.live_set_fragments[fragment]
            .ranges
            .push(TaggedLiveRange {
                prog_range,
                live_range,
            });
        live_range
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

    fn fragment_only_instr(&self, fragment: LiveSetFragment) -> Option<LiveRangeInstr> {
        let mut instrs = self.fragment_instrs(fragment);
        let instr = instrs.next()?;
        if instrs.next().is_some() {
            return None;
        }
        Some(instr)
    }

    fn fragment_has_instrs(&self, fragment: LiveSetFragment) -> bool {
        self.fragment_instrs(fragment).next().is_some()
    }

    fn fragment_instrs(
        &self,
        fragment: LiveSetFragment,
    ) -> impl Iterator<Item = LiveRangeInstr> + '_ {
        self.live_set_fragments[fragment]
            .ranges
            .iter()
            .flat_map(|range| self.live_ranges[range.live_range].instrs.iter().copied())
    }

    fn can_split_fragment_before(&self, fragment: LiveSetFragment, instr: Instr) -> bool {
        self.fragment_hull(fragment)
            .can_split_before(ProgramPoint::before(instr))
    }

    fn is_fragment_split(&self, fragment: LiveSetFragment) -> bool {
        let fragment = &self.live_set_fragments[fragment];
        fragment.prev_split_neighbor.is_some() || fragment.next_split_neighbor.is_some()
    }

    fn is_fragment_global(&self, fragment: LiveSetFragment) -> bool {
        let hull = self.fragment_hull(fragment);
        self.lir.instr_block_index(hull.start.instr())
            != self.lir.instr_block_index(hull.end.instr())
    }

    fn is_fragment_spilled(&self, fragment: LiveSetFragment) -> bool {
        self.live_set_fragments[fragment]
            .flags
            .contains(LiveSetFragmentFlags::SPILLED)
    }

    fn is_fragment_atomic(&self, fragment: LiveSetFragment) -> bool {
        self.live_set_fragments[fragment]
            .flags
            .contains(LiveSetFragmentFlags::ATOMIC)
    }

    fn fragment_hull(&self, fragment: LiveSetFragment) -> ProgramRange {
        self.live_set_fragments[fragment].hull()
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

fn conflict_boundary(
    existing_range: ProgramRange,
    conflicting_range: ProgramRange,
) -> Option<ConflictBoundary> {
    if conflicting_range.start < existing_range.start {
        Some(ConflictBoundary::StartsAt(existing_range.start.instr()))
    } else if conflicting_range.end > existing_range.end {
        Some(ConflictBoundary::EndsAt(
            existing_range.end.instr_rounded_up(),
        ))
    } else {
        None
    }
}

fn sort_probe_hints(hints: &mut ProbeHints) {
    // First: group the hints by physical register.
    hints.sort_unstable_by_key(|hint| hint.preg.as_u8());

    // Coalesce adjacent hints for the same register, recording total weight for each.
    let new_len = coalesce_slice(hints, |prev_hint, cur_hint| {
        if prev_hint.preg == cur_hint.preg {
            Some(ProbeHint {
                preg: prev_hint.preg,
                hint_weight: prev_hint.hint_weight + cur_hint.hint_weight,
                sort_weight: prev_hint.sort_weight + cur_hint.sort_weight,
            })
        } else {
            None
        }
    });

    hints.truncate(new_len);

    // Now, sort the hints in order of decreasing weight.
    hints.sort_unstable_by(|lhs, rhs| {
        lhs.sort_weight
            .partial_cmp(&rhs.sort_weight)
            .unwrap()
            .reverse()
    });
}

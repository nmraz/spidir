use core::mem;

use hashbrown::hash_map::Entry;
use itertools::Itertools;
use log::{log_enabled, trace};
use smallvec::{smallvec, SmallVec};

use crate::{
    lir::{Instr, PhysReg, PhysRegSet, VirtReg},
    machine::MachineRegalloc,
    regalloc::types::{
        AnnotatedPhysRegHint, InstrSlot, LiveRangeInstr, LiveRangeOpPos, ProgramPoint,
        TaggedLiveRange, FRAGMENT_PRIO_HINTED,
    },
};

use super::{
    conflict::{iter_btree_ranges, iter_conflicts, iter_slice_ranges},
    context::RegAllocContext,
    types::{
        LiveRange, LiveRangeInstrs, LiveSetFragment, PhysRegHint, ProgramRange, QueuedFragment,
        RangeEndKey,
    },
    utils::{get_instr_weight, sort_reg_hints},
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

        while let Some(queued_fragment) = self.worklist.pop() {
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

        let mut hints = SmallVec::new();
        self.collect_fragment_hints(fragment, &mut hints);

        // Start with hinted registers in order of decreasing weight, then move on to the default
        // allocation order requested by the machine backend.
        let probe_order = hints.iter().copied().chain(
            self.machine
                .usable_regs(class)
                .iter()
                .map(|&preg| PhysRegHint { preg, weight: 0.0 }),
        );

        let mut no_conflict_reg = None;
        let mut lightest_soft_conflict = None;
        let mut lightest_soft_conflict_weight = f32::INFINITY;
        let mut earliest_hard_conflict_boundary: Option<ConflictBoundary> = None;

        let mut probed_regs = PhysRegSet::empty();
        for hint in probe_order {
            let preg = hint.preg;
            let hint_weight = hint.weight;

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
                        self.can_split_fragment_before(
                            fragment,
                            ProgramPoint::before(boundary.instr()),
                        )
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
                self.evict_and_requeue_fragment(conflicting_fragment);
            }
            self.assign_fragment_to_phys_reg(fragment, preg, hint_weight);
            return Ok(());
        }

        if let Some(boundary) = earliest_hard_conflict_boundary {
            // To avoid pointlessly chopping everything up into tiny pieces, only split fragments
            // that actually contain instructions using their values; ones that just carry the value
            // elsewhere can be spilled.
            if self.fragment_has_instrs(fragment) {
                self.split_fragment_for_conflict(fragment, boundary);
                return Ok(());
            }
        }

        if !self.is_fragment_atomic(fragment) {
            self.spill_fragment(fragment);
            return Ok(());
        }

        let instr = self.fragment_hull(fragment).start.instr();
        Err(RegallocError::OutOfRegisters(instr))
    }

    fn collect_fragment_hints(
        &self,
        fragment: LiveSetFragment,
        hints: &mut SmallVec<[PhysRegHint; 8]>,
    ) {
        hints.extend_from_slice(&self.live_set_fragments[fragment].hints);

        let prev_fragment = self.live_set_fragments[fragment].prev_split_neighbor;
        let next_fragment = self.live_set_fragments[fragment].next_split_neighbor;

        // Avoid all the extra shuffling/sorting work when we don't need it, as we know the original
        // hints are sorted.
        if prev_fragment.is_none() && next_fragment.is_none() {
            return;
        }

        let hull = self.fragment_hull(fragment);

        if let Some(prev_fragment) = prev_fragment.expand() {
            if let Some(prev_assignment) =
                self.live_set_fragments[prev_fragment].assignment.expand()
            {
                let weight = get_instr_weight(self.lir, self.cfg_ctx, hull.start.instr());
                hints.push(PhysRegHint {
                    preg: prev_assignment,
                    weight,
                });
            }
        }

        if let Some(next_fragment) = next_fragment.expand() {
            if let Some(next_assignment) =
                self.live_set_fragments[next_fragment].assignment.expand()
            {
                let weight = get_instr_weight(self.lir, self.cfg_ctx, hull.end.instr());
                hints.push(PhysRegHint {
                    preg: next_assignment,
                    weight,
                });
            }
        }

        let hint_count = sort_reg_hints(hints);
        hints.truncate(hint_count);
    }

    fn spill_fragment(&mut self, fragment: LiveSetFragment) {
        trace!("  spill: {fragment}");

        let live_set = self.live_set_fragments[fragment].live_set;
        let fragment_hull = self.fragment_hull(fragment);

        let mut ranges = mem::take(&mut self.live_set_fragments[fragment].ranges);

        let mut reg_instrs = SmallVec::<[LiveRangeInstr; 4]>::new();
        let mut new_fragments = SmallVec::<[LiveSetFragment; 8]>::new();

        let mut last_instr = None;
        for range in ranges.drain(..) {
            let vreg = self.live_ranges[range.live_range].vreg;

            if log_enabled!(log::Level::Trace) {
                trace!("    range {:?}:", range.prog_range);
                for instr in &self.live_ranges[range.live_range].instrs {
                    trace!("      {:?}", instr);
                }
            }

            reg_instrs.clear();
            reg_instrs.extend(
                self.live_ranges[range.live_range]
                    .instrs
                    .drain_filter(|instr| instr.needs_reg()),
            );

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
        }

        for &new_fragment in &new_fragments {
            self.compute_live_fragment_properties(new_fragment);
            self.enqueue_fragment(new_fragment);
        }

        let set_spill_hull = &mut self.live_sets[live_set].spill_hull;
        match set_spill_hull {
            Some(existing_hull) => {
                existing_hull.start = existing_hull.start.min(fragment_hull.start);
                existing_hull.end = existing_hull.end.max(fragment_hull.end);
            }
            None => {
                *set_spill_hull = Some(fragment_hull);
            }
        }
    }

    fn split_fragment_for_conflict(
        &mut self,
        fragment: LiveSetFragment,
        boundary: ConflictBoundary,
    ) {
        // TODO: Search for a better split point by looking away from the boundary.
        let instr = boundary.instr();
        trace!(
            "  split: {fragment} (hull {:?}) at {instr}",
            self.fragment_hull(fragment)
        );
        self.split_fragment_before(fragment, instr);
    }

    fn split_fragment_before(&mut self, fragment: LiveSetFragment, instr: Instr) {
        let pos = ProgramPoint::before(instr);

        debug_assert!(self.can_split_fragment_before(fragment, pos));

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

    fn evict_and_requeue_fragment(&mut self, fragment: LiveSetFragment) {
        let data = &mut self.live_set_fragments[fragment];
        let preg = data.assignment.take().unwrap();
        trace!("  evict: {fragment} from {}", M::reg_name(preg));
        let preg_assignments = &mut self.phys_reg_assignments[preg.as_u8() as usize];
        for range in &data.ranges {
            preg_assignments
                .remove(&RangeEndKey(range.prog_range))
                .unwrap();
        }
        self.enqueue_fragment(fragment);
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

    fn fragment_has_instrs(&self, fragment: LiveSetFragment) -> bool {
        self.live_set_fragments[fragment]
            .ranges
            .iter()
            .any(|range| !self.live_ranges[range.live_range].instrs.is_empty())
    }

    fn can_split_fragment_before(&self, fragment: LiveSetFragment, point: ProgramPoint) -> bool {
        self.fragment_hull(fragment).can_split_before(point)
    }

    fn is_fragment_atomic(&self, fragment: LiveSetFragment) -> bool {
        self.live_set_fragments[fragment].is_atomic
    }

    fn fragment_hull(&self, fragment: LiveSetFragment) -> ProgramRange {
        self.live_set_fragments[fragment].hull()
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

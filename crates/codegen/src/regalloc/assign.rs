use alloc::vec::Vec;
use core::f32;

use itertools::Itertools;
use log::trace;
use smallvec::SmallVec;

use crate::{
    lir::{Instr, PhysReg, PhysRegSet},
    machine::MachineRegalloc,
};

use super::{
    RegallocError,
    conflict::{iter_btree_ranges, iter_conflicts, iter_slice_ranges},
    context::RegAllocContext,
    types::{
        ConflictBoundary, LiveSetFragment, LiveSetFragmentFlags, ProgramRange, QueuedFragment,
        RangeEndKey,
    },
    utils::{coalesce_slice, get_weight_at_instr},
};

enum RegProbeConflict {
    Soft {
        fragments: SmallVec<[LiveSetFragment; 4]>,
        weight: f32,
        hint_weight: f32,
    },
    Hard {
        boundary: Option<ConflictBoundary>,
    },
}

enum ProbeResult {
    Found {
        preg: PhysReg,
        hint_weight: f32,
    },
    FoundSoftConflict {
        preg: PhysReg,
        hint_weight: f32,
        conflicting_fragments: SmallVec<[LiveSetFragment; 4]>,
    },
    HardConflict {
        boundary: Option<ConflictBoundary>,
    },
}

#[derive(Clone, Copy)]
struct ProbeHint {
    preg: PhysReg,
    hint_weight: f32,
    sort_weight: f32,
}

type ProbeOrder = Vec<ProbeHint>;

const FRAGMENT_PRIO_HINTED: u32 = 1 << 31;

impl<M: MachineRegalloc> RegAllocContext<'_, M> {
    pub fn enqueue_fragment(&mut self, fragment: LiveSetFragment) {
        let fragment_data = &self.live_set_fragments[fragment];

        let mut prio = fragment_data.size;
        if !fragment_data.phys_hints.is_empty() {
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

    pub fn evict_fragment(&mut self, fragment: LiveSetFragment) {
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

        let mut probe_order = ProbeOrder::new();

        while let Some(queued_fragment) = self.dequeue_fragment() {
            let fragment = queued_fragment.fragment;
            trace!(
                "process: {fragment}, prio {}, weight {}",
                queued_fragment.prio, self.live_set_fragments[fragment].spill_weight,
            );
            self.try_assign(&mut probe_order, fragment)?;
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

    fn try_assign(
        &mut self,
        probe_order: &mut ProbeOrder,
        fragment: LiveSetFragment,
    ) -> Result<(), RegallocError> {
        if self.is_fragment_cheaply_remattable(fragment) && self.should_spill_cheap_remat(fragment)
        {
            self.spill_fragment_and_neighbors(fragment);
            return Ok(());
        }

        let probe_result = self.probe_regs_for_fragment(fragment, probe_order);

        match probe_result {
            ProbeResult::Found { preg, hint_weight } => {
                self.assign_fragment_to_phys_reg(fragment, preg, hint_weight);
                Ok(())
            }
            ProbeResult::FoundSoftConflict {
                preg,
                hint_weight,
                conflicting_fragments,
            } => {
                for conflicting_fragment in conflicting_fragments {
                    self.evict_fragment(conflicting_fragment);
                    self.enqueue_fragment(conflicting_fragment);
                }
                self.assign_fragment_to_phys_reg(fragment, preg, hint_weight);
                Ok(())
            }
            ProbeResult::HardConflict { boundary } => {
                if let Some(boundary) = boundary {
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
        }
    }

    fn should_spill_cheap_remat(&self, fragment: LiveSetFragment) -> bool {
        if !self.fragment_has_weight(fragment) {
            // If the fragment is completely weightless, it has no uses and can be spilled eagerly.
            return true;
        }

        // If the fragment has only one use, trim it as close to that use as we possibly can.
        self.fragment_instrs(fragment)
            .filter(|instr| !instr.is_def())
            .nth(1)
            .is_none()
    }

    fn probe_regs_for_fragment(
        &self,
        fragment: LiveSetFragment,
        probe_order: &mut ProbeOrder,
    ) -> ProbeResult {
        let live_set = self.live_set_fragments[fragment].live_set;
        let class = self.live_sets[live_set].class;

        // Probe order: start with hinted registers in order of decreasing weight, then move on to
        // the default allocation order requested by the machine backend. The backend's allocation
        // order can be tacked on without re-coalescing/re-sorting the probe order because all
        // registers aren't treated as hinted.

        probe_order.clear();
        match self.collect_probe_hints(fragment, probe_order) {
            Ok(()) => {
                // Hints have been collected.
            }
            Err(conflict_boundary) => {
                // The fragment had mandatory hints (it was cheaply rematerializable), but none
                // could be satisfied; report a conflict to the caller.
                return ProbeResult::HardConflict {
                    boundary: conflict_boundary,
                };
            }
        }

        let is_hinted = !probe_order.is_empty();

        // When our fragment is cheaply rematerializable and has register hints, treat them as
        // mandatory: violations here will lead to copies later, but we would be better served by
        // rematerializing into the appropriate register instead.
        if !(self.is_fragment_cheaply_remattable(fragment) && is_hinted) {
            probe_order.extend(
                self.machine
                    .usable_regs(class)
                    .iter()
                    .map(|&preg| ProbeHint {
                        preg,
                        hint_weight: 0.0,
                        sort_weight: 0.0,
                    }),
            );
        }

        let mut lightest_soft_conflict = None;
        let mut lightest_soft_conflict_weight = f32::INFINITY;
        let mut earliest_hard_conflict_boundary = None;

        let mut probed_regs = PhysRegSet::empty();
        for hint in probe_order.iter() {
            let preg = hint.preg;
            let hint_weight = hint.hint_weight;

            if probed_regs.contains(preg) {
                continue;
            }

            probed_regs.insert(preg);

            trace!("  probe: {} (hint weight {hint_weight})", M::reg_name(preg));
            match self.probe_phys_reg(preg, fragment) {
                None => {
                    trace!("    no conflict");
                    return ProbeResult::Found { preg, hint_weight };
                }
                Some(RegProbeConflict::Soft {
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
                Some(RegProbeConflict::Hard { boundary, .. }) => {
                    trace!("    hard conflict with boundary {boundary:?}");
                    self.update_earliest_conflict_boundary(
                        fragment,
                        &mut earliest_hard_conflict_boundary,
                        boundary,
                    );
                }
            }
        }

        if let Some((preg, hint_weight, conflicting_fragments)) = lightest_soft_conflict {
            return ProbeResult::FoundSoftConflict {
                preg,
                hint_weight,
                conflicting_fragments,
            };
        }

        ProbeResult::HardConflict {
            boundary: earliest_hard_conflict_boundary,
        }
    }

    fn collect_probe_hints(
        &self,
        fragment: LiveSetFragment,
        probe_order: &mut ProbeOrder,
    ) -> Result<(), Option<ConflictBoundary>> {
        probe_order.extend(
            self.live_set_fragments[fragment]
                .phys_hints
                .iter()
                .map(|hint| ProbeHint {
                    preg: hint.preg,
                    hint_weight: hint.weight,
                    sort_weight: hint.weight,
                }),
        );

        let has_uncoalesced_copy_hints = self.live_set_fragments[fragment]
            .flags
            .contains(LiveSetFragmentFlags::HAS_UNCOALESCED_COPY_HINTS);

        let prev_fragment = self.live_set_fragments[fragment].prev_split_neighbor;
        let next_fragment = self.live_set_fragments[fragment].next_split_neighbor;

        // Avoid all the extra shuffling/sorting work when we don't need it, as we know the original
        // hints are sorted by weight.
        if !has_uncoalesced_copy_hints && prev_fragment.is_none() && next_fragment.is_none() {
            return Ok(());
        }

        // Collect hints from neighboring fragments if they have already been assigned. Note that
        // we never mark these as having "proper" hint weights for allocation tracking, as the
        // neighbors themselves might be evicted later.

        let hull = self.fragment_hull(fragment);

        if let Some(prev_fragment) = prev_fragment.expand() {
            self.collect_hint_from_fragment(hull.start.instr(), prev_fragment, probe_order);
        }

        if let Some(next_fragment) = next_fragment.expand() {
            self.collect_hint_from_fragment(hull.end.instr(), next_fragment, probe_order);
        }

        if has_uncoalesced_copy_hints {
            self.collect_uncoalesced_copy_hints(fragment, probe_order)?;
        }

        sort_probe_hints(probe_order);

        Ok(())
    }

    fn collect_hint_from_fragment(
        &self,
        instr: Instr,
        hint_fragment: LiveSetFragment,
        probe_order: &mut ProbeOrder,
    ) {
        if let Some(hint_assignment) = self.live_set_fragments[hint_fragment].assignment.expand() {
            self.collect_hint_at_instr(instr, hint_assignment, probe_order);
        }
    }

    fn collect_uncoalesced_copy_hints(
        &self,
        fragment: LiveSetFragment,
        probe_order: &mut Vec<ProbeHint>,
    ) -> Result<(), Option<ConflictBoundary>> {
        let cheaply_remattable = self.is_fragment_cheaply_remattable(fragment);

        let mut saw_cheap_remat_conflicts = false;
        let mut earliest_conflict_boundary = None;

        for uncoalesced_copy in &self.uncoalesced_fragment_copy_hints[&fragment] {
            let hint = self.fragment_copy_hints[uncoalesced_copy.hint];
            let hint_fragment = hint.get_other_fragment(fragment);

            // If this fragment hasn't been assigned yet, it isn't interesting as a copy hint.
            let Some(hint_assignment) = self.live_set_fragments[hint_fragment].assignment.expand()
            else {
                continue;
            };

            // Don't collect a hint here if the fragments still interfere somewhere, since we can't
            // currently assign them to the same register anyway.
            if let Some((existing_range, conflicting_range)) =
                self.first_fragment_conflict(hint_fragment, fragment)
            {
                // If the fragment is cheaply rematerializable, keep around the earliest
                // conflict boundary in case we need it.
                if hint.is_mandatory() && cheaply_remattable {
                    self.update_earliest_conflict_boundary(
                        fragment,
                        &mut earliest_conflict_boundary,
                        conflict_boundary(existing_range, conflicting_range),
                    );
                    saw_cheap_remat_conflicts = true;
                }
                continue;
            }

            self.collect_hint_at_instr(uncoalesced_copy.instr, hint_assignment, probe_order);
        }

        // If none of a cheaply-rematerializable fragment's mandatory hints can be satisfied due to
        // conflicts, report that to the caller and let them split/spill appropriately. It's better
        // to rematerialize than to copy all over the place.
        if saw_cheap_remat_conflicts && probe_order.is_empty() {
            return Err(earliest_conflict_boundary);
        }

        Ok(())
    }

    fn collect_hint_at_instr(
        &self,
        instr: Instr,
        hint_assignment: PhysReg,
        probe_order: &mut ProbeOrder,
    ) {
        let weight = get_weight_at_instr(self.lir, self.cfg_ctx, instr);
        probe_order.push(ProbeHint {
            preg: hint_assignment,
            hint_weight: 0.0,
            sort_weight: weight,
        });
    }

    fn update_earliest_conflict_boundary(
        &self,
        fragment: LiveSetFragment,
        earliest_conflict_boundary: &mut Option<ConflictBoundary>,
        new_conflict_boundary: Option<ConflictBoundary>,
    ) {
        // This boundary is interesting for splitting if it can non-degenerately split the fragment
        // in two.
        let split_boundary = new_conflict_boundary.filter(|new_conflict_boundary| {
            self.can_split_fragment_before(fragment, new_conflict_boundary.instr())
        });
        if let Some(split_boundary) = split_boundary {
            match earliest_conflict_boundary {
                Some(cur_boundary) => {
                    if split_boundary.instr() < cur_boundary.instr() {
                        *earliest_conflict_boundary = Some(split_boundary);
                    }
                }
                None => {
                    *earliest_conflict_boundary = Some(split_boundary);
                }
            }
        }
    }

    fn probe_phys_reg(&self, preg: PhysReg, fragment: LiveSetFragment) -> Option<RegProbeConflict> {
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
    ) -> Option<RegProbeConflict> {
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
                return Some(RegProbeConflict::Hard {
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
    ) -> Option<RegProbeConflict> {
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
                return Some(RegProbeConflict::Hard {
                    boundary: conflict_boundary(preg_range, fragment_range),
                });
            }

            soft_conflicts.push(allocated_fragment);
            soft_conflict_weight = soft_conflict_weight.max(allocated_weight);
            soft_conflict_hint_weight =
                soft_conflict_hint_weight.max(fragment_data.assignment_hint_weight);
        }

        if !soft_conflicts.is_empty() {
            Some(RegProbeConflict::Soft {
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

fn sort_probe_hints(probe_order: &mut ProbeOrder) {
    // First: group the hints by physical register.
    probe_order.sort_unstable_by_key(|hint| hint.preg.as_u8());

    // Coalesce adjacent hints for the same register, recording total weight for each.
    let new_len = coalesce_slice(probe_order, |prev_hint, cur_hint| {
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

    probe_order.truncate(new_len);

    // Now, sort the hints in order of decreasing weight.
    probe_order.sort_unstable_by(|lhs, rhs| {
        lhs.sort_weight
            .partial_cmp(&rhs.sort_weight)
            .unwrap()
            .reverse()
    });
}

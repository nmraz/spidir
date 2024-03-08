use core::{cmp::Ordering, mem};

use alloc::collections::BinaryHeap;
use itertools::Itertools;
use log::trace;
use smallvec::{smallvec, SmallVec};

use crate::{
    lir::{Instr, PhysReg, PhysRegSet},
    machine::MachineCore,
    regalloc::types::{LiveRangeData, ProgramPoint, TaggedLiveRange},
};

use super::{
    conflict::{iter_btree_ranges, iter_conflicts, iter_slice_ranges},
    context::RegAllocContext,
    types::{LiveSetFragment, ProgramRange, RangeEndKey},
};

#[derive(Eq)]
struct QueuedFragment {
    fragment: LiveSetFragment,
    size: u32,
}

impl PartialEq for QueuedFragment {
    fn eq(&self, other: &Self) -> bool {
        self.size == other.size
    }
}

impl PartialOrd for QueuedFragment {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for QueuedFragment {
    fn cmp(&self, other: &Self) -> Ordering {
        self.size.cmp(&other.size)
    }
}

type FragmentQueue = BinaryHeap<QueuedFragment>;

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
    },
    Hard {
        boundary: Option<ConflictBoundary>,
        weight: Option<f32>,
    },
}

impl<M: MachineCore> RegAllocContext<'_, M> {
    pub fn run_core_loop(&mut self) {
        // Process fragments in order of decreasing size, to try to fill in the larger ranges before
        // moving on to smaller ones. Because of weight-based eviction, we can still end up
        // revisiting a larger fragment later.
        let mut worklist = FragmentQueue::new();
        for (fragment, fragment_data) in self.live_set_fragments.iter() {
            if fragment_data.ranges.is_empty() {
                continue;
            }

            worklist.push(QueuedFragment {
                fragment,
                size: fragment_data.size,
            });
        }

        while let Some(fragment) = worklist.pop() {
            let fragment = fragment.fragment;
            trace!("process: {fragment}");
            self.try_allocate(fragment, &mut worklist);
        }
    }

    fn try_allocate(&mut self, fragment: LiveSetFragment, worklist: &mut FragmentQueue) {
        let class = self.live_set_fragments[fragment].class;

        // Start with hinted registers in order of decreasing weight, then move on to the default
        // allocation order requested by the machine backend.
        let probe_order = self.live_set_fragments[fragment]
            .hints
            .iter()
            .map(|hint| hint.preg)
            .chain(self.machine.usable_regs(class).iter().copied());

        let mut no_conflict_reg = None;
        let mut lightest_soft_conflict = None;
        let mut lightest_soft_conflict_weight = f32::INFINITY;
        let mut earliest_hard_conflict_boundary: Option<ConflictBoundary> = None;

        let mut probed_regs = PhysRegSet::empty();
        for preg in probe_order {
            if probed_regs.contains(preg) {
                continue;
            }

            probed_regs.add(preg);

            trace!("  probe: {}", M::reg_name(preg));
            match self.probe_phys_reg(preg, fragment) {
                None => {
                    trace!("    no conflict");
                    no_conflict_reg = Some(preg);
                    break;
                }
                Some(ProbeConflict::Soft { fragments, weight }) => {
                    trace!(
                        "    soft conflict with {} (weight {weight})",
                        fragments.iter().format(", ")
                    );
                    if weight < lightest_soft_conflict_weight {
                        lightest_soft_conflict = Some((preg, fragments));
                        lightest_soft_conflict_weight = weight;
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

        if let Some(no_conflict_reg) = no_conflict_reg {
            self.assign_fragment_to_phys_reg(no_conflict_reg, fragment);
        } else if let Some((preg, soft_conflicts)) = lightest_soft_conflict {
            for conflicting_fragment in soft_conflicts {
                self.evict_fragment(conflicting_fragment);
                worklist.push(QueuedFragment {
                    fragment: conflicting_fragment,
                    size: self.live_set_fragments[conflicting_fragment].size,
                });
            }
            self.assign_fragment_to_phys_reg(preg, fragment);
        } else if let Some(boundary) = earliest_hard_conflict_boundary {
            let new_fragment = self.split_at_boundary(fragment, boundary);
            worklist.push(QueuedFragment {
                fragment,
                size: self.live_set_fragments[fragment].size,
            });
            worklist.push(QueuedFragment {
                fragment: new_fragment,
                size: self.live_set_fragments[new_fragment].size,
            });
        } else {
            todo!("spill");
        }
    }

    fn split_at_boundary(
        &mut self,
        fragment: LiveSetFragment,
        boundary: ConflictBoundary,
    ) -> LiveSetFragment {
        // TODO: Search for a better split point by looking away from the boundary.
        let instr = boundary.instr();
        trace!("  split: {fragment} at {instr}");
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

                if pos.next() < prev_range_end {
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
        let class = self.live_set_fragments[fragment].class;
        let new_ranges = self.live_set_fragments[fragment]
            .ranges
            .drain(split_idx..)
            .collect();
        let new_fragment = self.create_live_fragment(live_set, class, new_ranges);

        if split_boundary_range {
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
            self.live_ranges[split_live_range].fragment = new_fragment;

            // Split the attached instructions on the boundary using a simple linear traversal: we
            // basically need to walk the entire instruction list anyway, and a linear traversal is
            // probably better for cache locality.

            let mut instrs = mem::take(&mut self.live_ranges[split_live_range].instrs);
            let instr_split_idx = instrs
                .iter()
                // Note: we're splitting before `instr`, so we want `instr` itself to be part of
                // the upper half of the range.
                .position(|range_instr| range_instr.pos >= instr);
            let high_instrs = if let Some(instr_split_idx) = instr_split_idx {
                instrs.drain(instr_split_idx..).collect()
            } else {
                smallvec![]
            };

            self.live_ranges[split_live_range].instrs = high_instrs;
            // `instrs` itself now contains the "low" instructions.

            // Create a new live range to append to the end of the original `fragment`.
            let vreg = self.live_ranges[split_live_range].vreg;
            let new_live_range = self.live_ranges.push(LiveRangeData {
                prog_range: low_range,
                vreg,
                fragment,
                instrs,
            });

            // Note: `vreg_ranges` will no longer be sorted by range order once we do this, but we
            // don't care within the core loop.
            self.vreg_ranges[vreg].push(new_live_range);
            self.live_set_fragments[fragment]
                .ranges
                .push(TaggedLiveRange {
                    prog_range: low_range,
                    live_range: new_live_range,
                });
        }

        self.compute_live_fragment_properties(fragment);
        self.compute_live_fragment_properties(new_fragment);

        new_fragment
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

            let copied_from_live_range = reservation_copy
                .is_some_and(|live_range_copy| live_range_copy.live_range == fragment_live_range);
            if !copied_from_live_range {
                return Some(ProbeConflict::Hard {
                    boundary: conflict_boundary(reservation_range, fragment_range),
                    weight: None,
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

        for ((preg_range, preg_assignment), (fragment_range, _)) in
            iter_conflicts(preg_ranges, fragment_ranges)
        {
            let allocated_fragment = self.live_ranges[preg_assignment].fragment;
            let allocated_weight = self.live_set_fragments[allocated_fragment].spill_weight;
            if allocated_weight >= fragment_weight {
                return Some(ProbeConflict::Hard {
                    boundary: conflict_boundary(preg_range, fragment_range),
                    weight: Some(allocated_weight),
                });
            }

            soft_conflicts.push(allocated_fragment);
            soft_conflict_weight = soft_conflict_weight.max(allocated_weight);
        }

        if !soft_conflicts.is_empty() {
            Some(ProbeConflict::Soft {
                fragments: soft_conflicts,
                weight: soft_conflict_weight,
            })
        } else {
            None
        }
    }

    fn assign_fragment_to_phys_reg(&mut self, preg: PhysReg, fragment: LiveSetFragment) {
        trace!("  assign: {fragment} -> {}", M::reg_name(preg));
        let data = &mut self.live_set_fragments[fragment];
        let preg_assignments = &mut self.phys_reg_assignments[preg.as_u8() as usize];
        assert!(data.assignment.is_none());
        data.assignment = preg.into();
        for range in &data.ranges {
            preg_assignments.insert(RangeEndKey(range.prog_range), range.live_range);
        }
    }

    fn evict_fragment(&mut self, fragment: LiveSetFragment) {
        let data = &mut self.live_set_fragments[fragment];
        let preg = data.assignment.take().unwrap();
        trace!("  evict: {fragment} from {}", M::reg_name(preg));
        let preg_assignments = &mut self.phys_reg_assignments[preg.as_u8() as usize];
        for range in &data.ranges {
            preg_assignments
                .remove(&RangeEndKey(range.prog_range))
                .unwrap();
        }
    }

    fn can_split_fragment_before(&self, fragment: LiveSetFragment, point: ProgramPoint) -> bool {
        self.fragment_hull(fragment).can_split_before(point)
    }

    fn fragment_hull(&self, fragment: LiveSetFragment) -> ProgramRange {
        let ranges = &self.live_set_fragments[fragment].ranges;
        ProgramRange::new(
            ranges.first().unwrap().prog_range.start,
            ranges.last().unwrap().prog_range.end,
        )
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

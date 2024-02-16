use core::cmp::Ordering;

use alloc::collections::BinaryHeap;
use log::trace;
use smallvec::SmallVec;

use crate::{
    lir::{PhysReg, PhysRegSet},
    machine::MachineCore,
};

use super::{
    context::RegAllocContext,
    types::{LiveSetFragment, ProgramPoint, RangeEndKey},
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

enum ProbeConflict {
    Soft {
        fragments: SmallVec<[LiveSetFragment; 4]>,
        weight: f32,
    },
    Hard {
        pos: ProgramPoint,
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
                    trace!("    soft conflict (weight {weight})");
                    if weight < lightest_soft_conflict_weight {
                        lightest_soft_conflict = Some((preg, fragments));
                        lightest_soft_conflict_weight = weight;
                    }
                }
                Some(ProbeConflict::Hard { pos, .. }) => {
                    trace!("    hard conflict at {pos:?}");
                    // TODO: use for split decisions.
                }
            }
        }

        if let Some(no_conflict_reg) = no_conflict_reg {
            self.assign_fragment_to_phys_reg(no_conflict_reg, fragment);
        } else if let Some((preg, soft_conflicts)) = lightest_soft_conflict {
            for conflicting_fragment in soft_conflicts {
                self.evict_fragment(conflicting_fragment);
                worklist.push(QueuedFragment {
                    fragment,
                    size: self.live_set_fragments[fragment].size,
                });
            }
            self.assign_fragment_to_phys_reg(preg, fragment);
        } else {
            todo!("split/spill");
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
        let reservations = &self.phys_reg_reservations[preg.as_u8() as usize];
        let fragment_ranges = &self.live_set_fragments[fragment].ranges;

        // Find the first reservation ending above our start point.
        let first_reservation = reservations
            .binary_search_by_key(&fragment_ranges[0].prog_range.start, |reservation| {
                reservation.prog_range.end
            })
            .unwrap_or_else(|i| i);

        let mut reservation_ranges = reservations[first_reservation..].iter().peekable();
        let mut fragment_ranges = fragment_ranges.iter().peekable();

        loop {
            let Some(&fragment_range) = fragment_ranges.peek() else {
                break;
            };

            let Some(&reservation_range) = reservation_ranges.peek() else {
                break;
            };

            if fragment_range
                .prog_range
                .intersects(reservation_range.prog_range)
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

                let copied_from_live_range =
                    reservation_range
                        .copied_live_range
                        .is_some_and(|live_range_copy| {
                            live_range_copy.live_range == fragment_range.live_range
                        });
                if !copied_from_live_range {
                    let pos = reservation_range.prog_range.start;
                    return Some(ProbeConflict::Hard { pos, weight: None });
                }
            }

            match fragment_range
                .prog_range
                .end
                .cmp(&reservation_range.prog_range.end)
            {
                Ordering::Less => {
                    fragment_ranges.next();
                }
                Ordering::Greater => {
                    reservation_ranges.next();
                }
                Ordering::Equal => {
                    // If both ranges end at the same point, we can consume them both without
                    // searching for more intersections, because we assume that the ranges are
                    // non-intersecting in each of the separate lists.
                    fragment_ranges.next();
                    reservation_ranges.next();
                }
            }
        }

        None
    }

    fn probe_phys_assignment(
        &self,
        preg: PhysReg,
        fragment: LiveSetFragment,
    ) -> Option<ProbeConflict> {
        let preg_map = &self.phys_reg_assignments[preg.as_u8() as usize];
        let fragment_weight = self.live_set_fragments[fragment].spill_weight;
        let fragment_ranges = &self.live_set_fragments[fragment].ranges;

        // Assumption: `preg_map` can generally be much larger than `ranges` (it tracks a physical
        // register throughout the entire function). Avoid a complete linear search through
        // `preg_map` by starting with the first range inside it containing the bottom endpoint
        // of `ranges`.
        let search_key = RangeEndKey::point(fragment_ranges[0].prog_range.start);

        // Once we've found the first range, do a linear walk the rest of the way to avoid logarithmic
        // complexity per iteration.
        let mut preg_ranges = preg_map.range(search_key..).peekable();
        let mut fragment_ranges = fragment_ranges.iter().copied().peekable();

        let mut soft_conflicts = SmallVec::new();
        let mut soft_conflict_weight = 0f32;

        loop {
            // If we've run out of either list without finding an intersection, there *is* no
            // intersection.
            let Some(live_range) = fragment_ranges.peek() else {
                break;
            };
            let Some((&preg_range, &allocated_live_range)) = preg_ranges.peek() else {
                break;
            };

            let preg_range = preg_range.0;

            if live_range.prog_range.intersects(preg_range) {
                let pos = preg_range.start;

                let allocated_fragment = self.live_ranges[allocated_live_range].fragment;
                let allocated_weight = self.live_set_fragments[allocated_fragment].spill_weight;
                if allocated_weight >= fragment_weight {
                    return Some(ProbeConflict::Hard {
                        pos,
                        weight: Some(allocated_weight),
                    });
                }

                soft_conflicts.push(allocated_fragment);
                soft_conflict_weight = soft_conflict_weight.max(allocated_weight);
            }

            match live_range.prog_range.end.cmp(&preg_range.end) {
                Ordering::Less => {
                    fragment_ranges.next();
                }
                Ordering::Greater => {
                    preg_ranges.next();
                }
                Ordering::Equal => {
                    // If both ranges end at the same point, we can consume them both without
                    // searching for more intersections, because we assume that the ranges are
                    // non-intersecting in each of the separate lists.
                    fragment_ranges.next();
                    preg_ranges.next();
                }
            }
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
}

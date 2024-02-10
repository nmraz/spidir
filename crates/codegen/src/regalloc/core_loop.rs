use core::cmp::Ordering;

use alloc::collections::BinaryHeap;
use smallvec::SmallVec;

use crate::{lir::PhysReg, machine::MachineCore};

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
        let mut worklist = BinaryHeap::new();
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
            self.try_allocate(fragment);
        }
    }

    fn try_allocate(&mut self, fragment: LiveSetFragment) {
        let class = self.live_set_fragments[fragment].class;

        // TODO: Take register hints into account once they exist.
        let probe_order = self.machine.usable_regs(class);

        let mut no_conflict_reg = None;
        let mut lightest_soft_conflict = None;
        let mut lightest_soft_conflict_weight = f32::INFINITY;

        for &preg in probe_order {
            match self.probe_phys_reg(preg, fragment) {
                None => {
                    no_conflict_reg = Some(preg);
                    break;
                }
                Some(ProbeConflict::Soft { fragments, weight }) => {
                    if weight < lightest_soft_conflict_weight {
                        lightest_soft_conflict = Some((preg, fragments));
                        lightest_soft_conflict_weight = weight;
                    }
                }
                Some(ProbeConflict::Hard { .. }) => {
                    // TODO: use for split decisions.
                }
            }
        }

        if let Some(no_conflict_reg) = no_conflict_reg {
            self.assign_fragment_to_phys_reg(no_conflict_reg, fragment);
        } else if let Some((preg, soft_conflicts)) = lightest_soft_conflict {
            for conflicting_fragment in soft_conflicts {
                self.evict_fragment(conflicting_fragment);
            }
            self.assign_fragment_to_phys_reg(preg, fragment);
        } else {
            todo!("split/spill");
        }
    }

    fn probe_phys_reg(&self, preg: PhysReg, fragment: LiveSetFragment) -> Option<ProbeConflict> {
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

                match allocated_live_range.expand() {
                    Some(allocated_live_range) => {
                        let allocated_fragment = self.live_ranges[allocated_live_range].fragment;
                        let allocated_weight =
                            self.live_set_fragments[allocated_fragment].spill_weight;
                        if allocated_weight >= fragment_weight {
                            return Some(ProbeConflict::Hard {
                                pos,
                                weight: Some(allocated_weight),
                            });
                        }

                        soft_conflicts.push(allocated_fragment);
                        soft_conflict_weight = soft_conflict_weight.max(allocated_weight);
                    }
                    None => return Some(ProbeConflict::Hard { pos, weight: None }),
                };
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
        let data = &mut self.live_set_fragments[fragment];
        let preg_assignments = &mut self.phys_reg_assignments[preg.as_u8() as usize];
        assert!(data.assignment.is_none());
        data.assignment = preg.into();
        for range in &data.ranges {
            preg_assignments.insert(RangeEndKey(range.prog_range), range.live_range.into());
        }
    }

    fn evict_fragment(&mut self, fragment: LiveSetFragment) {
        let data = &mut self.live_set_fragments[fragment];
        let preg = data.assignment.take().unwrap();
        let preg_assignments = &mut self.phys_reg_assignments[preg.as_u8() as usize];
        for range in &data.ranges {
            preg_assignments
                .remove(&RangeEndKey(range.prog_range))
                .unwrap();
        }
    }
}

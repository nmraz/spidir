use core::{cmp::Ordering, iter::Peekable, slice};

use alloc::collections::{btree_map, BTreeMap, BinaryHeap};
use log::trace;
use smallvec::SmallVec;

use crate::{
    lir::{PhysReg, PhysRegSet},
    machine::MachineCore,
};

use super::{
    context::RegAllocContext,
    types::{LiveSetFragment, ProgramPoint, ProgramRange, RangeEndKey, TaggedLiveRange},
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

struct RangeConflict<T> {
    fragment_range: TaggedLiveRange,
    preg_range: ProgramRange,
    value: T,
}

struct RangeConflictIter<'a, T> {
    fragment_ranges: Peekable<slice::Iter<'a, TaggedLiveRange>>,
    preg_ranges: Peekable<btree_map::Range<'a, RangeEndKey, T>>,
}

impl<'a, T> RangeConflictIter<'a, T> {
    fn new(preg_map: &'a BTreeMap<RangeEndKey, T>, fragment_ranges: &'a [TaggedLiveRange]) -> Self {
        // Assumption: `preg_map` can generally be much larger than `ranges` (it tracks a physical
        // register throughout the entire function). Avoid a complete linear search through
        // `preg_map` by starting with the first range inside it containing the bottom endpoint
        // of `ranges`.
        let search_key = RangeEndKey::point(fragment_ranges[0].prog_range.start);

        let preg_ranges = preg_map.range(search_key..).peekable();
        let fragment_ranges = fragment_ranges.iter().peekable();

        Self {
            fragment_ranges,
            preg_ranges,
        }
    }
}

impl<'a, T: Copy> Iterator for RangeConflictIter<'a, T> {
    type Item = RangeConflict<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let conflict = loop {
            let fragment_range = **self.fragment_ranges.peek()?;
            let (preg_range, &allocated_value) = self.preg_ranges.peek()?;

            let preg_range = preg_range.0;

            match fragment_range.prog_range.end.cmp(&preg_range.end) {
                Ordering::Less => {
                    self.fragment_ranges.next();
                }
                Ordering::Greater => {
                    self.preg_ranges.next();
                }
                Ordering::Equal => {
                    // If both ranges end at the same point, we can consume them both without
                    // searching for more intersections, because we assume that the ranges are
                    // non-intersecting in each of the separate lists.
                    self.fragment_ranges.next();
                    self.preg_ranges.next();
                }
            }

            if fragment_range.prog_range.intersects(preg_range) {
                break RangeConflict {
                    fragment_range,
                    preg_range,
                    value: allocated_value,
                };
            }
        };

        Some(conflict)
    }
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
        let preg_map = &self.phys_reg_assignments[preg.as_u8() as usize];
        let fragment_weight = self.live_set_fragments[fragment].spill_weight;
        let fragment_ranges = &self.live_set_fragments[fragment].ranges;

        let mut soft_conflicts = SmallVec::new();
        let mut soft_conflict_weight = 0f32;

        for range_conflict in RangeConflictIter::new(preg_map, fragment_ranges) {
            let pos = range_conflict.preg_range.start;
            let allocated_live_range = range_conflict.value.expand();

            match allocated_live_range {
                Some(allocated_live_range) => {
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
                None => return Some(ProbeConflict::Hard { pos, weight: None }),
            };
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
            preg_assignments.insert(RangeEndKey(range.prog_range), range.live_range.into());
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

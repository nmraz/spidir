use alloc::vec::Vec;
use core::mem;

use cranelift_entity::{
    packed_option::{PackedOption, ReservedValue},
    SecondaryMap,
};
use log::trace;
use smallvec::smallvec;

use crate::{
    lir::{UseOperandConstraint, VirtRegNum},
    machine::MachineRegalloc,
};

use super::{
    conflict::{iter_conflicts, iter_slice_ranges},
    context::RegAllocContext,
    types::{
        LiveSet, LiveSetData, LiveSetFragment, LiveSetFragmentData, PhysRegHint, PhysRegHints,
        ProgramRange, TaggedLiveRange, TaggedRangeList,
    },
    utils::{coalesce_slice, get_instr_weight},
};

type VirtRegFragmentMap = SecondaryMap<VirtRegNum, PackedOption<LiveSetFragment>>;

struct CopyCandidate {
    src: VirtRegNum,
    dest: VirtRegNum,
    weight: f32,
}

/// The spill weight assigned to all atomic fragments.
///
/// This is higher than all possible non-atomic fragment weights, so that atomic fragments always
/// evict everything else.
const ATOMIC_FRAGMENT_WEIGHT: f32 = (1 << f32::MANTISSA_DIGITS) as f32;

/// The highest spill weight that can be assigned to non-atomic fragments.
const MAX_NONATOMIC_FRAGMENT_WEIGHT: f32 = ATOMIC_FRAGMENT_WEIGHT - 1.0;

impl<M: MachineRegalloc> RegAllocContext<'_, M> {
    pub fn build_live_sets(&mut self) {
        let mut fragments_by_vreg = VirtRegFragmentMap::new();
        let mut candidates = Vec::new();

        for vreg in self.vreg_ranges.keys() {
            let fragment = self.create_live_fragment(
                LiveSet::reserved_value(),
                self.vreg_ranges[vreg]
                    .iter()
                    .map(|&live_range| TaggedLiveRange {
                        prog_range: self.live_ranges[live_range].prog_range,
                        live_range,
                    })
                    .collect(),
            );
            fragments_by_vreg[vreg] = fragment.into();
        }

        for &block in &self.cfg_ctx.block_order {
            for instr in self.lir.block_instrs(block) {
                for &use_op in self.lir.instr_uses(instr) {
                    if let UseOperandConstraint::TiedToDef(i) = use_op.constraint() {
                        let def = self.lir.instr_defs(instr)[i as usize];
                        candidates.push(CopyCandidate {
                            src: use_op.reg().reg_num(),
                            dest: def.reg().reg_num(),
                            weight: get_instr_weight(self.lir, self.cfg_ctx, instr),
                        });
                    }
                }
            }

            if let &[succ] = self.cfg_ctx.cfg.block_succs(block) {
                // If this block has a single successor, it might have outgoing params - try to
                // coalesce them now.
                for (&outgoing, &incoming) in self
                    .lir
                    .outgoing_block_params(block)
                    .iter()
                    .zip(self.lir.block_params(succ))
                {
                    // Since this block has a single successor, we can tack the copy itself onto the
                    // terminator for weight purposes.
                    let terminator = self.lir.block_instrs(block).end.prev();
                    candidates.push(CopyCandidate {
                        src: outgoing.reg_num(),
                        dest: incoming.reg_num(),
                        weight: get_instr_weight(self.lir, self.cfg_ctx, terminator),
                    });
                }
            }
        }

        // Now, greedily coalesce gathered candidates, processing in order of decreasing weight.
        candidates.sort_unstable_by(|lhs_candidate, rhs_candidate| {
            lhs_candidate
                .weight
                .partial_cmp(&rhs_candidate.weight)
                .unwrap()
                .reverse()
        });

        for candidate in &candidates {
            trace!(
                "coalesce candidate: {} -> {}, weight {}",
                candidate.src,
                candidate.dest,
                candidate.weight
            );
            self.try_coalesce(&mut fragments_by_vreg, candidate.dest, candidate.src);
        }

        // Fill in per-fragment data now that we actually know what the fragments are.
        for fragment in self.live_set_fragments.keys() {
            let Some(first_range) = self.live_set_fragments[fragment].ranges.first() else {
                continue;
            };
            let vreg = self.live_ranges[first_range.live_range].vreg;

            let live_set = self.live_sets.push(LiveSetData {
                class: self.lir.vreg_class(vreg),
                spill_hull: None,
                spill_slot: None.into(),
            });

            self.live_set_fragments[fragment].live_set = live_set;
            self.compute_live_fragment_properties(fragment);
        }
    }

    pub fn create_live_fragment(
        &mut self,
        live_set: LiveSet,
        ranges: TaggedRangeList,
    ) -> LiveSetFragment {
        self.live_set_fragments.push(LiveSetFragmentData {
            live_set,
            ranges,
            hints: smallvec![],
            assignment: None.into(),
            size: 0,
            spill_weight: 0.0,
            is_atomic: false,
        })
    }

    pub fn compute_live_fragment_properties(&mut self, fragment: LiveSetFragment) {
        let mut size = 0;
        let mut total_weight = 0.0;
        let mut some_instr_needs_reg = false;

        let fragment_data = &mut self.live_set_fragments[fragment];
        fragment_data.hints.clear();

        for range in &fragment_data.ranges {
            let range_data = &mut self.live_ranges[range.live_range];
            range_data.fragment = fragment;
            size += range.prog_range.len();

            for instr in &range_data.instrs {
                total_weight += instr.weight();
                some_instr_needs_reg |= instr.needs_reg();
            }

            if let Some(range_hints) = self.live_range_hints.get(&range.live_range) {
                fragment_data
                    .hints
                    .extend(range_hints.iter().map(|annotated_hint| annotated_hint.hint));
            }
        }

        sort_reg_hints(&mut fragment_data.hints);

        fragment_data.size = size;
        fragment_data.is_atomic = some_instr_needs_reg && covers_single_instr(fragment_data.hull());

        fragment_data.spill_weight = if fragment_data.is_atomic {
            ATOMIC_FRAGMENT_WEIGHT
        } else {
            (total_weight / (size as f32)).min(MAX_NONATOMIC_FRAGMENT_WEIGHT)
        };
    }

    fn try_coalesce(
        &mut self,
        fragments_by_vreg: &mut VirtRegFragmentMap,
        dest: VirtRegNum,
        src: VirtRegNum,
    ) {
        let dest_fragment = fragments_by_vreg[dest].unwrap();
        let src_fragment = fragments_by_vreg[src].unwrap();

        if src_fragment == dest_fragment {
            // A previous attempt already coalesced these vregs, nothing to try now.
            return;
        }

        if self.live_sets_interfere(
            &self.live_set_fragments[dest_fragment].ranges,
            &self.live_set_fragments[src_fragment].ranges,
        ) {
            return;
        }

        trace!("coalesce: {src} -> {dest}");

        // Empty the source's range list so we know to ignore it later.
        let src_ranges = mem::take(&mut self.live_set_fragments[src_fragment].ranges);

        // Merge the source set's ranges into the destination set's. We still need the destination's
        // ranges to be sorted for later coalesces/use in the allocator, so take a page out of
        // regalloc2's book and do an extend+sort, which they observed to be faster than a linear
        // merge.
        self.live_set_fragments[dest_fragment]
            .ranges
            .extend(src_ranges);
        self.live_set_fragments[dest_fragment]
            .ranges
            .sort_unstable_by_key(|range| range.prog_range.start);

        // The source vreg now belongs to the newly-coalesced fragment.
        fragments_by_vreg[src] = dest_fragment.into();
    }

    fn live_sets_interfere(&self, a: &[TaggedLiveRange], b: &[TaggedLiveRange]) -> bool {
        let a = iter_slice_ranges(a, |tagged_range| (tagged_range.prog_range, &()));
        let b = iter_slice_ranges(b, |tagged_range| (tagged_range.prog_range, &()));
        iter_conflicts(a, b).next().is_some()
    }
}

fn covers_single_instr(range: ProgramRange) -> bool {
    range.start.instr() == range.end.prev().instr()
}

fn sort_reg_hints(hints: &mut PhysRegHints) {
    // First: group the hints by physical register.
    hints.sort_unstable_by_key(|hint| hint.preg.as_u8());

    // Coalesce adjacent hints for the same register, recording total weight for each.
    let new_len = coalesce_slice(hints, |prev_hint, cur_hint| {
        if prev_hint.preg == cur_hint.preg {
            Some(PhysRegHint {
                preg: prev_hint.preg,
                weight: prev_hint.weight + cur_hint.weight,
            })
        } else {
            None
        }
    });
    hints.truncate(new_len);

    // Now, sort the hints in order of decreasing weight.
    hints.sort_unstable_by(|lhs, rhs| lhs.weight.partial_cmp(&rhs.weight).unwrap().reverse());
}

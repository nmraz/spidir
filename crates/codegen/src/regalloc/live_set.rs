use alloc::vec::Vec;
use core::mem;

use cranelift_entity::{
    packed_option::{PackedOption, ReservedValue},
    SecondaryMap,
};
use log::trace;

use crate::{
    lir::{UseOperandConstraint, VirtReg},
    machine::MachineRegalloc,
};

use super::{
    conflict::{iter_conflicts, iter_slice_ranges},
    context::RegAllocContext,
    types::{LiveSet, LiveSetData, LiveSetFragment, TaggedLiveRange},
    utils::get_weight_at_instr,
};

type VirtRegFragmentMap = SecondaryMap<VirtReg, PackedOption<LiveSetFragment>>;

struct CopyCandidate {
    src: VirtReg,
    dest: VirtReg,
    weight: f32,
}

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
                            src: use_op.reg(),
                            dest: def.reg(),
                            weight: get_weight_at_instr(self.lir, self.cfg_ctx, instr),
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
                    let terminator = self.lir.block_terminator(block);
                    candidates.push(CopyCandidate {
                        src: outgoing,
                        dest: incoming,
                        weight: get_weight_at_instr(self.lir, self.cfg_ctx, terminator),
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

    fn try_coalesce(
        &mut self,
        fragments_by_vreg: &mut VirtRegFragmentMap,
        dest: VirtReg,
        src: VirtReg,
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

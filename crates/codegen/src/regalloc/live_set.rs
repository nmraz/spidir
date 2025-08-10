use alloc::vec::Vec;
use core::mem;
use hashbrown::hash_map::Entry;

use cranelift_entity::{
    SecondaryMap,
    packed_option::{PackedOption, ReservedValue},
};
use log::trace;

use crate::{
    lir::{Instr, UseOperandConstraint, VirtReg},
    machine::MachineRegalloc,
};

use super::{
    context::RegAllocContext,
    types::{
        FragmentCopyHintData, LiveSet, LiveSetData, LiveSetFragment, TaggedFragmentCopyHint,
        TaggedLiveRange,
    },
    utils::get_weight_at_instr,
};

type VirtRegFragmentMap = SecondaryMap<VirtReg, PackedOption<LiveSetFragment>>;

struct CopyCandidate {
    instr: Instr,
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
                    let src = use_op.reg();

                    match use_op.constraint() {
                        UseOperandConstraint::TiedToDef(i) => {
                            let def = self.lir.instr_defs(instr)[i as usize];
                            candidates.push(CopyCandidate {
                                instr,
                                src,
                                dest: def.reg(),
                                weight: get_weight_at_instr(self.lir, self.cfg_ctx, instr),
                            });
                        }
                        UseOperandConstraint::SoftTiedToDef(i) => {
                            let def = self.lir.instr_defs(instr)[i as usize];
                            let src_fragment = fragments_by_vreg[src].unwrap();
                            let dest_fragment = fragments_by_vreg[def.reg()].unwrap();
                            self.hint_fragment_copy(instr, src_fragment, dest_fragment, false);
                        }
                        _ => {}
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
                        instr: terminator,
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
                candidate.src, candidate.dest, candidate.weight
            );
            self.try_coalesce(
                &mut fragments_by_vreg,
                candidate.instr,
                candidate.dest,
                candidate.src,
            );
        }

        // Make sure uncoalesced copy hints are sorted by instruction, as the rest of the code
        // depends on this.
        for hints in self.uncoalesced_fragment_copy_hints.values_mut() {
            hints.sort_unstable_by_key(|hint| hint.instr);
        }

        // Fill in per-fragment data now that we actually know what the fragments are.
        for fragment in self.live_set_fragments.keys() {
            let Some(first_range) = self.live_set_fragments[fragment].ranges.first() else {
                continue;
            };
            let vreg = self.live_ranges[first_range.live_range].vreg;

            let live_set = self.live_sets.push(LiveSetData {
                bank: self.lir.vreg_bank(vreg),
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
        instr: Instr,
        dest: VirtReg,
        src: VirtReg,
    ) {
        let dest_fragment = fragments_by_vreg[dest].unwrap();
        let src_fragment = fragments_by_vreg[src].unwrap();

        if src_fragment == dest_fragment {
            // A previous attempt already coalesced these vregs, nothing to try now.
            return;
        }

        if self
            .first_fragment_conflict(dest_fragment, src_fragment)
            .is_some()
        {
            // We couldn't coalesce these fragments now, but we might be able to later because of
            // splitting. Record a copy hint so we remember to do so.
            self.hint_fragment_copy(instr, src_fragment, dest_fragment, true);
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

        // Move in any uncoalesced copy hints from the source.
        if let Some(src_copy_hints) = self.uncoalesced_fragment_copy_hints.remove(&src_fragment) {
            for tagged_hint in &src_copy_hints {
                self.fragment_copy_hints[tagged_hint.hint]
                    .replace_fragment(src_fragment, dest_fragment);
            }

            match self.uncoalesced_fragment_copy_hints.entry(dest_fragment) {
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(src_copy_hints);
                }
                Entry::Occupied(mut occupied_entry) => {
                    occupied_entry.get_mut().extend_from_slice(&src_copy_hints);
                }
            }
        }

        // The source vreg now belongs to the newly-coalesced fragment.
        fragments_by_vreg[src] = dest_fragment.into();
    }

    fn hint_fragment_copy(
        &mut self,
        instr: Instr,
        a: LiveSetFragment,
        b: LiveSetFragment,
        is_mandatory: bool,
    ) {
        let hint = self
            .fragment_copy_hints
            .push(FragmentCopyHintData::new(a, b, is_mandatory));

        self.uncoalesced_fragment_copy_hints
            .entry(a)
            .or_default()
            .push(TaggedFragmentCopyHint { hint, instr });
        self.uncoalesced_fragment_copy_hints
            .entry(b)
            .or_default()
            .push(TaggedFragmentCopyHint { hint, instr });
    }
}

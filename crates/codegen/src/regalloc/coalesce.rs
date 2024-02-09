use core::{cmp::Ordering, mem};

use cranelift_entity::{
    packed_option::{PackedOption, ReservedValue},
    SecondaryMap,
};
use log::trace;

use crate::{
    lir::{UseOperandConstraint, VirtRegNum},
    machine::MachineCore,
};

use super::{
    context::RegAllocContext,
    types::{LiveSet, LiveSetFragment, LiveSetFragmentData, TaggedLiveRange},
};

type VirtRegFragmentMap = SecondaryMap<VirtRegNum, PackedOption<LiveSetFragment>>;

impl<M: MachineCore> RegAllocContext<'_, M> {
    pub fn coalesce_live_sets(&mut self) {
        let mut fragments_by_vreg = VirtRegFragmentMap::new();

        for vreg in self.vreg_ranges.keys() {
            let fragment = self.live_set_fragments.push(LiveSetFragmentData {
                live_set: LiveSet::reserved_value(),
                ranges: self.vreg_ranges[vreg].clone(),
                assignment: None.into(),
                size: 0,
                spill_weight: 0.0,
            });
            fragments_by_vreg[vreg] = fragment.into();
        }

        for &block in &self.cfg_ctx.block_order {
            for instr in self.lir.block_instrs(block) {
                for &use_op in self.lir.instr_uses(instr) {
                    if let UseOperandConstraint::TiedToDef(i) = use_op.constraint() {
                        let def = self.lir.instr_defs(instr)[i as usize];
                        self.try_coalesce(
                            &mut fragments_by_vreg,
                            def.reg().reg_num(),
                            use_op.reg().reg_num(),
                        );
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
                    self.try_coalesce(
                        &mut fragments_by_vreg,
                        incoming.reg_num(),
                        outgoing.reg_num(),
                    );
                }
            }
        }
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
        let mut a = a.iter().peekable();
        let mut b = b.iter().peekable();

        loop {
            let Some(a_range) = a.peek() else {
                break;
            };

            let Some(b_range) = b.peek() else {
                break;
            };

            if a_range.prog_range.intersects(b_range.prog_range) {
                return true;
            }

            match a_range.prog_range.end.cmp(&b_range.prog_range.end) {
                Ordering::Less => {
                    a.next();
                }
                Ordering::Greater => {
                    b.next();
                }
                Ordering::Equal => {
                    // This is impossible if both ranges are non-degenerate, which we assume them to be.
                    unreachable!("non-intersecting ranges had same end point")
                }
            }
        }

        false
    }
}

use core::{cmp::Ordering, mem};

use hashbrown::hash_map::Entry;
use log::trace;
use smallvec::SmallVec;

use crate::{
    lir::Instr,
    machine::MachineRegalloc,
    regalloc::types::{AnnotatedPhysRegHint, ProgramRange},
};

use super::{
    RematCost,
    context::RegAllocContext,
    types::{ConflictBoundary, LiveSetFragment, ProgramPoint, TaggedFragmentCopyHints},
    utils::get_block_weight,
};

#[derive(Debug)]
enum SplitKind {
    Single(Instr),
    Double(Instr, Instr),
}

impl<M: MachineRegalloc> RegAllocContext<'_, M> {
    pub fn try_split_fragment_for_conflict(
        &mut self,
        fragment: LiveSetFragment,
        boundary: ConflictBoundary,
    ) -> bool {
        trace!("  try split: {fragment} for conflict {boundary:?}");

        // To avoid pointlessly chopping everything up into tiny pieces, only split fragments that
        // actually contain instructions using their values; ones that just carry the value
        // elsewhere can be spilled.
        if !self.fragment_has_weight(fragment) {
            return false;
        }

        if let Some(global_split) = self.global_split_point(fragment) {
            trace!("    found global split: {global_split}");
            self.split_and_requeue_fragment(fragment, SplitKind::Single(global_split));
            return true;
        }

        if let Some(conflict_split) = self.split_kind_for_conflict(fragment, boundary) {
            trace!("    found conflict split: {conflict_split:?}");
            self.split_and_requeue_fragment(fragment, conflict_split);
            return true;
        };

        false
    }

    fn global_split_point(&self, fragment: LiveSetFragment) -> Option<Instr> {
        if !self.is_fragment_global(fragment) {
            return None;
        }

        let hull = self.fragment_hull(fragment);
        let start = hull.start.instr();
        let end = hull.end.instr();

        let mut block_idx = self.lir.instr_block_index(start);
        let mut last_block = self.cfg_ctx.block_order[block_idx];
        let mut last_weight = get_block_weight(self.cfg_ctx, last_block);

        let mut min_weight = f32::INFINITY;
        let mut split_point = None;

        loop {
            block_idx += 1;

            // If this fragment overlaps with the last block, make sure we don't run right off it.
            if block_idx >= self.cfg_ctx.block_order.len() {
                break;
            }

            let block = self.cfg_ctx.block_order[block_idx];
            let block_start = self.lir.block_instrs(block).start;

            if block_start >= end {
                break;
            }

            let weight = get_block_weight(self.cfg_ctx, block);
            match last_weight.partial_cmp(&weight).unwrap() {
                Ordering::Less if last_weight < min_weight => {
                    // We've found a transition from a low-weight block to a higher-weight block,
                    // with the low weight having reached a new minimum across our walk. Split just
                    // before the end of the low-weight block to circumvent the high-weight one.
                    let last_terminator = self.lir.block_terminator(last_block);
                    if hull.can_split_before(ProgramPoint::before(last_terminator)) {
                        split_point = Some(last_terminator);
                        min_weight = last_weight;
                    }
                }
                Ordering::Greater if weight < min_weight => {
                    // We've found a transition from a high-weight block to a lower-weight block,
                    // with the low weight having reached a new minimum across our walk. Split just
                    // before the start of the low-weight block, noting that any copies inserted
                    // later will never be placed in a more deeply-nested loop.
                    if hull.can_split_before(ProgramPoint::before(block_start)) {
                        split_point = Some(block_start);
                        min_weight = weight;
                    }
                }
                _ => {}
            }

            last_block = block;
            last_weight = weight;
        }

        split_point
    }

    fn split_kind_for_conflict(
        &self,
        fragment: LiveSetFragment,
        boundary: ConflictBoundary,
    ) -> Option<SplitKind> {
        if self.is_fragment_split(fragment) && self.fragment_only_instr(fragment).is_some() {
            // We don't want to repeatedly split single-instruction fragments on conflict
            // boundaries, as they can get us into pointless eviction/splitting/shuffling fights
            // under high register pressure. It's better to just give up early and spill in this
            // case; it frequently even reduces total spill count because there aren't a bunch of
            // small pieces to move around.
            return None;
        }

        let boundary_instr = boundary.instr();

        let mut can_remat_low_cheaply = true;
        let mut saw_low_use = false;

        let mut low_split_point = None;
        let mut high_split_point = None;

        'ranges: for tagged_range in &self.live_set_fragments[fragment].ranges {
            // Track rematerializability for everything that might end up in the first half of the
            // split. We're trying to split before the boundary instruction, so ranges starting
            // exactly at the boundary actually belong to the upper half.
            if tagged_range.prog_range.start.instr() < boundary_instr && can_remat_low_cheaply {
                let vreg = self.live_ranges[tagged_range.live_range].vreg;
                can_remat_low_cheaply = self.remattable_vreg_defs[vreg]
                    .expand()
                    .is_some_and(|def| def.cost() == RematCost::CheapAsCopy);
            }

            for &instr in &self.live_ranges[tagged_range.live_range].instrs {
                if instr.instr() < boundary_instr {
                    saw_low_use |= !instr.is_def();
                    // We always split *before* the selected instruction, but we want to split
                    // *after* the last use.
                    low_split_point = Some(instr.instr().next());
                } else {
                    high_split_point = Some(instr.instr());
                    // Once we've found the first instruction after the split point, we have all
                    // the information we need.
                    break 'ranges;
                }
            }
        }

        if can_remat_low_cheaply {
            let low_split_point =
                self.get_real_split_point(fragment, low_split_point, boundary_instr);

            let low_split_point = low_split_point.filter(|_| {
                // If the lower half of the range contains only a rematerializable definitions,
                // don't even bother to insert an extra split there, as everything is going to be
                // spilled anyway.
                saw_low_use
            });

            let high_split_point =
                self.get_real_split_point(fragment, high_split_point, boundary_instr);

            let split_kind = match (low_split_point, high_split_point) {
                (Some(low_split_point), Some(high_split_point))
                    if high_split_point != low_split_point =>
                {
                    SplitKind::Double(low_split_point, high_split_point)
                }
                (Some(low_split_point), _) => SplitKind::Single(low_split_point),
                (_, Some(high_split_point)) => SplitKind::Single(high_split_point),
                _ => return None,
            };

            return Some(split_kind);
        }

        let split_point = match boundary {
            ConflictBoundary::StartsAt(..) => low_split_point,
            ConflictBoundary::EndsAt(..) => high_split_point,
        };

        self.get_real_split_point(fragment, split_point, boundary_instr)
            .map(SplitKind::Single)
    }

    fn split_and_requeue_fragment(&mut self, fragment: LiveSetFragment, split: SplitKind) {
        match split {
            SplitKind::Single(instr) => {
                let new_fragment = self.split_fragment(fragment, instr);

                self.enqueue_fragment(fragment);
                self.enqueue_fragment(new_fragment);
            }
            SplitKind::Double(instr1, instr2) => {
                let mid = self.split_fragment(fragment, instr1);
                let high = self.split_fragment(mid, instr2);

                self.enqueue_fragment(fragment);
                self.enqueue_fragment(mid);
                self.enqueue_fragment(high);
            }
        }
    }

    fn split_fragment(&mut self, fragment: LiveSetFragment, instr: Instr) -> LiveSetFragment {
        trace!(
            "  split: {fragment} (hull {:?}) at {instr}",
            self.fragment_hull(fragment)
        );

        debug_assert!(self.can_split_fragment_before(fragment, instr));

        let new_fragment = self.split_fragment_ranges(fragment, instr);
        self.split_fragment_copy_hints(fragment, new_fragment, instr);
        self.mark_fragment_split_neighbors(fragment, new_fragment);

        self.compute_live_fragment_properties(fragment);
        self.compute_live_fragment_properties(new_fragment);

        new_fragment
    }

    fn split_fragment_ranges(
        &mut self,
        fragment: LiveSetFragment,
        instr: Instr,
    ) -> LiveSetFragment {
        let pos = ProgramPoint::before(instr);

        let (range_split_idx, split_boundary_range) = match self.live_set_fragments[fragment]
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
            .drain(range_split_idx..)
            .collect();
        let new_fragment = self.create_live_fragment(live_set, new_ranges);

        if split_boundary_range {
            self.split_fragment_boundary_range_before(fragment, new_fragment, instr);
        }

        new_fragment
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
        // Note: we're splitting before `instr`, so we want `instr` itself to be part of the upper
        // half of the range.
        let instr_split_idx = instrs.partition_point(|range_instr| range_instr.instr() < instr);
        let high_instrs = instrs.drain(instr_split_idx..).collect();

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

    fn split_fragment_copy_hints(
        &mut self,
        old_fragment: LiveSetFragment,
        new_fragment: LiveSetFragment,
        instr: Instr,
    ) {
        let Entry::Occupied(mut entry) = self.uncoalesced_fragment_copy_hints.entry(old_fragment)
        else {
            return;
        };

        let hints = entry.get_mut();

        let copy_hint_split_idx = hints.partition_point(|hint| hint.instr < instr);

        let new_copy_hints: TaggedFragmentCopyHints = hints.drain(copy_hint_split_idx..).collect();
        if hints.is_empty() {
            entry.remove();
        }

        if new_copy_hints.is_empty() {
            return;
        }

        // Make sure to replace all references to the old fragment with the new one in the hints
        // we've moved.
        for tagged_hint in &new_copy_hints {
            self.fragment_copy_hints[tagged_hint.hint].replace_fragment(old_fragment, new_fragment);
        }

        self.uncoalesced_fragment_copy_hints
            .insert(new_fragment, new_copy_hints);
    }

    fn mark_fragment_split_neighbors(
        &mut self,
        old_fragment: LiveSetFragment,
        new_fragment: LiveSetFragment,
    ) {
        self.live_set_fragments[new_fragment].prev_split_neighbor = old_fragment.into();
        self.live_set_fragments[new_fragment].next_split_neighbor =
            self.live_set_fragments[old_fragment].next_split_neighbor;
        self.live_set_fragments[old_fragment].next_split_neighbor = new_fragment.into();
    }

    fn get_real_split_point(
        &self,
        fragment: LiveSetFragment,
        split_point: Option<Instr>,
        fallback: Instr,
    ) -> Option<Instr> {
        let split_point = split_point.unwrap_or(fallback);
        // Note: we check whether the fragment can be split after selecting whether we want the
        // fallback, so that an unsuitable split point makes us select nothing rather than taking
        // the fallback. Experience shows that it is better to spill than to select a poor split
        // point.
        self.can_split_fragment_before(fragment, split_point)
            .then_some(split_point)
    }
}

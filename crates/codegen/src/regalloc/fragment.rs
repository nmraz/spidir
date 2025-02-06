use smallvec::smallvec;

use crate::{
    lir::{Instr, VirtReg},
    machine::MachineRegalloc,
    regalloc::RematCost,
};

use super::{
    context::RegAllocContext,
    types::{
        LiveRange, LiveRangeInstr, LiveRangeInstrs, LiveSet, LiveSetFragment, LiveSetFragmentData,
        LiveSetFragmentFlags, PhysRegHint, PhysRegHints, ProgramPoint, ProgramRange,
        TaggedLiveRange, TaggedRangeList,
    },
    utils::coalesce_slice,
};

/// The spill weight assigned to all atomic fragments.
///
/// This is higher than all possible non-atomic fragment weights, so that atomic fragments always
/// evict everything else.
const ATOMIC_FRAGMENT_WEIGHT: f32 = (1 << f32::MANTISSA_DIGITS) as f32;

/// The highest spill weight that can be assigned to non-atomic fragments.
const MAX_NONATOMIC_FRAGMENT_WEIGHT: f32 = ATOMIC_FRAGMENT_WEIGHT - 1.0;

impl<M: MachineRegalloc> RegAllocContext<'_, M> {
    pub fn create_live_fragment(
        &mut self,
        live_set: LiveSet,
        ranges: TaggedRangeList,
    ) -> LiveSetFragment {
        self.live_set_fragments.push(LiveSetFragmentData {
            live_set,
            prev_split_neighbor: None.into(),
            next_split_neighbor: None.into(),
            ranges,
            hints: smallvec![],
            assignment: None.into(),
            assignment_hint_weight: 0.0,
            size: 0,
            spill_weight: 0.0,
            flags: LiveSetFragmentFlags::empty(),
        })
    }

    pub fn compute_live_fragment_properties(&mut self, fragment: LiveSetFragment) {
        enum RematState {
            Uninit,
            Yes((VirtReg, RematCost)),
            No,
        }

        let mut size = 0;
        let mut total_weight = 0.0;
        let mut some_instr_needs_reg = false;

        // Track whether this fragment is completely rematerializable.
        let mut remat_state = RematState::Uninit;

        // Track whether this fragment contains any uses.
        let mut has_uses = false;

        let fragment_data = &mut self.live_set_fragments[fragment];
        fragment_data.hints.clear();

        for range in &fragment_data.ranges {
            let range_data = &mut self.live_ranges[range.live_range];
            let vreg = range_data.vreg;
            range_data.fragment = fragment;
            size += range.prog_range.len();

            // The fragment can only be rematerialized when all its ranges come from the same vreg,
            // and that vreg can itself be rematerialized.
            remat_state = match remat_state {
                RematState::Uninit => match self.remattable_vreg_defs[vreg].expand() {
                    Some(remat) => RematState::Yes((vreg, remat.cost())),
                    None => RematState::No,
                },
                RematState::Yes((existing_vreg, _)) if existing_vreg == vreg => remat_state,
                _ => RematState::No,
            };

            for instr in &range_data.instrs {
                total_weight += instr.weight();
                some_instr_needs_reg |= instr.needs_reg();
                has_uses |= !instr.is_def();
            }

            if let Some(range_hints) = self.live_range_hints.get(&range.live_range) {
                fragment_data
                    .hints
                    .extend(range_hints.iter().map(|annotated_hint| annotated_hint.hint));
            }
        }

        sort_reg_hints(&mut fragment_data.hints);

        fragment_data.size = size;

        let all_ranges_remat = matches!(remat_state, RematState::Yes(..));
        let remat_no_uses = all_ranges_remat && !has_uses;

        // Single-instruction fragments requiring a register _cannot_ be spilled (we don't have a
        // way to chop them smaller), unless they only cover a rematerializable definition, in which
        // case spilling will just kill the instruction.
        let is_atomic =
            !remat_no_uses && some_instr_needs_reg && covers_single_instr(fragment_data.hull());

        fragment_data
            .flags
            .set(LiveSetFragmentFlags::ATOMIC, is_atomic);

        fragment_data.flags.set(
            LiveSetFragmentFlags::CHEAPLY_REMATTABLE,
            matches!(remat_state, RematState::Yes((_, RematCost::CheapAsCopy))),
        );

        fragment_data.spill_weight = if is_atomic {
            ATOMIC_FRAGMENT_WEIGHT
        } else {
            (total_weight / (size as f32)).min(MAX_NONATOMIC_FRAGMENT_WEIGHT)
        };
    }

    pub fn push_vreg_fragment_live_range(
        &mut self,
        vreg: VirtReg,
        fragment: LiveSetFragment,
        prog_range: ProgramRange,
        instrs: LiveRangeInstrs,
        is_spill_connector: bool,
    ) -> LiveRange {
        let live_range =
            self.push_vreg_live_range(vreg, fragment, prog_range, instrs, is_spill_connector);
        self.live_set_fragments[fragment]
            .ranges
            .push(TaggedLiveRange {
                prog_range,
                live_range,
            });
        live_range
    }

    pub fn fragment_only_instr(&self, fragment: LiveSetFragment) -> Option<LiveRangeInstr> {
        let mut instrs = self.fragment_instrs(fragment);
        let instr = instrs.next()?;
        if instrs.next().is_some() {
            return None;
        }
        Some(instr)
    }

    pub fn fragment_has_instrs(&self, fragment: LiveSetFragment) -> bool {
        self.fragment_instrs(fragment).next().is_some()
    }

    pub fn fragment_instrs(
        &self,
        fragment: LiveSetFragment,
    ) -> impl Iterator<Item = LiveRangeInstr> + '_ {
        self.live_set_fragments[fragment]
            .ranges
            .iter()
            .flat_map(|range| self.live_ranges[range.live_range].instrs.iter().copied())
    }

    pub fn can_split_fragment_before(&self, fragment: LiveSetFragment, instr: Instr) -> bool {
        self.fragment_hull(fragment)
            .can_split_before(ProgramPoint::before(instr))
    }

    pub fn is_fragment_split(&self, fragment: LiveSetFragment) -> bool {
        let fragment = &self.live_set_fragments[fragment];
        fragment.prev_split_neighbor.is_some() || fragment.next_split_neighbor.is_some()
    }

    pub fn is_fragment_global(&self, fragment: LiveSetFragment) -> bool {
        let hull = self.fragment_hull(fragment);
        self.lir.instr_block_index(hull.start.instr())
            != self.lir.instr_block_index(hull.end.instr().prev())
    }

    pub fn is_fragment_spilled(&self, fragment: LiveSetFragment) -> bool {
        self.live_set_fragments[fragment]
            .flags
            .contains(LiveSetFragmentFlags::SPILLED)
    }

    pub fn is_fragment_atomic(&self, fragment: LiveSetFragment) -> bool {
        self.live_set_fragments[fragment]
            .flags
            .contains(LiveSetFragmentFlags::ATOMIC)
    }

    pub fn fragment_hull(&self, fragment: LiveSetFragment) -> ProgramRange {
        self.live_set_fragments[fragment].hull()
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

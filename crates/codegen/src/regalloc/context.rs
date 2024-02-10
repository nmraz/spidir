use alloc::{collections::BTreeMap, vec, vec::Vec};
use cranelift_entity::{packed_option::PackedOption, PrimaryMap, SecondaryMap};
use fx_utils::FxHashMap;
use itertools::Itertools;
use log::{debug, log_enabled};
use smallvec::SmallVec;

use crate::{
    cfg::CfgContext,
    lir::{Instr, Lir, PhysReg, VirtRegNum},
    machine::MachineCore,
};

use super::types::{
    LiveRange, LiveRangeData, LiveSet, LiveSetData, LiveSetFragment, LiveSetFragmentData,
    PhysRegCopy, PhysRegHint, RangeEndKey, TaggedLiveRange,
};

pub struct RegAllocContext<'a, M: MachineCore> {
    pub lir: &'a Lir<M>,
    pub cfg_ctx: &'a CfgContext,
    pub machine: &'a M,
    pub vreg_ranges: SecondaryMap<VirtRegNum, SmallVec<[TaggedLiveRange; 4]>>,
    pub pre_instr_preg_copies: SecondaryMap<Instr, SmallVec<[PhysRegCopy; 2]>>,
    pub post_instr_preg_copies: SecondaryMap<Instr, SmallVec<[PhysRegCopy; 2]>>,
    pub live_ranges: PrimaryMap<LiveRange, LiveRangeData>,
    pub live_range_hints: FxHashMap<LiveRange, SmallVec<[PhysRegHint; 2]>>,
    pub live_set_fragments: PrimaryMap<LiveSetFragment, LiveSetFragmentData>,
    pub live_sets: PrimaryMap<LiveSet, LiveSetData>,
    pub phys_reg_assignments: Vec<BTreeMap<RangeEndKey, PackedOption<LiveRange>>>,
}

impl<'a, M: MachineCore> RegAllocContext<'a, M> {
    pub fn new(lir: &'a Lir<M>, cfg_ctx: &'a CfgContext, machine: &'a M) -> Self {
        Self {
            lir,
            cfg_ctx,
            machine,
            vreg_ranges: SecondaryMap::new(),
            pre_instr_preg_copies: SecondaryMap::new(),
            post_instr_preg_copies: SecondaryMap::new(),
            live_ranges: PrimaryMap::new(),
            live_range_hints: FxHashMap::default(),
            live_set_fragments: PrimaryMap::new(),
            live_sets: PrimaryMap::new(),
            phys_reg_assignments: vec![BTreeMap::new(); M::phys_reg_count() as usize],
        }
    }

    pub fn dump(&self) {
        if !log_enabled!(log::Level::Debug) {
            return;
        }

        debug!("liveranges:");
        for (vreg, ranges) in self.vreg_ranges.iter() {
            if ranges.is_empty() {
                continue;
            }

            debug!(
                "{}: {}",
                vreg,
                ranges
                    .iter()
                    .format_with(" ", |range, f| f(&format_args!("{:?}", range.prog_range)))
            );
        }

        debug!("phys copies:");

        if !self.lir.live_in_regs().is_empty() {
            debug!(
                "E: {}",
                self.lir
                    .live_in_regs()
                    .iter()
                    .zip(self.lir.block_params(self.cfg_ctx.block_order[0]))
                    .format_with(", ", |(&preg, &vreg), f| {
                        f(&format_args!("{} -> {}", M::reg_name(preg), vreg.reg_num()))
                    })
            )
        }

        for instr in self.lir.all_instrs() {
            let pre_copies = &self.pre_instr_preg_copies[instr];
            if !pre_copies.is_empty() {
                debug!(
                    "B:{instr}: {}",
                    pre_copies.iter().format_with(", ", |copy, f| {
                        let vreg = self.live_ranges[copy.live_range].vreg;
                        f(&format_args!("{vreg} -> {}", M::reg_name(copy.preg)))
                    })
                );
            }

            let post_copies = &self.post_instr_preg_copies[instr];
            if !post_copies.is_empty() {
                debug!(
                    "A:{instr}: {}",
                    post_copies.iter().format_with(", ", |copy, f| {
                        let vreg = self.live_ranges[copy.live_range].vreg;
                        f(&format_args!("{} -> {vreg}", M::reg_name(copy.preg)))
                    })
                )
            }
        }

        debug!("live fragments:");
        for (fragment, fragment_data) in self.live_set_fragments.iter() {
            if fragment_data.ranges.is_empty() {
                continue;
            }

            debug!(
                "{fragment}({}): size {}, weight {}",
                fragment_data
                    .assignment
                    .expand()
                    .map_or("<none>", |reg| M::reg_name(reg)),
                fragment_data.size,
                fragment_data.spill_weight,
            );
            for range in &fragment_data.ranges {
                debug!("  {:?}", range.prog_range);
            }
        }

        debug!("phys regs:");
        for preg_num in 0..M::phys_reg_count() {
            let assignments = &self.phys_reg_assignments[preg_num as usize];
            if assignments.is_empty() {
                continue;
            }

            debug!("{}:", M::reg_name(PhysReg::new(preg_num as u8)));

            for (&range_key, &live_range) in assignments {
                let range = range_key.0;
                if let Some(live_range) = live_range.expand() {
                    debug!("  {range:?} ({})", self.live_ranges[live_range].vreg);
                } else {
                    debug!("  {range:?}");
                }
            }
        }
    }
}

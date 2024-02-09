use alloc::{collections::BTreeMap, vec, vec::Vec};
use cranelift_entity::{packed_option::PackedOption, PrimaryMap, SecondaryMap};
use itertools::Itertools;
use log::{debug, log_enabled};
use smallvec::SmallVec;

use crate::{
    cfg::CfgContext,
    lir::{Instr, Lir, VirtRegNum},
    machine::MachineCore,
};

use super::types::{LiveRange, LiveRangeData, PhysRegCopy, RangeEndKey, TaggedLiveRange};

pub struct RegAllocContext<'a, M: MachineCore> {
    pub lir: &'a Lir<M>,
    pub cfg_ctx: &'a CfgContext,
    pub vreg_ranges: SecondaryMap<VirtRegNum, SmallVec<[TaggedLiveRange; 4]>>,
    pub pre_instr_preg_copies: SecondaryMap<Instr, SmallVec<[PhysRegCopy; 2]>>,
    pub post_instr_preg_copies: SecondaryMap<Instr, SmallVec<[PhysRegCopy; 2]>>,
    pub live_ranges: PrimaryMap<LiveRange, LiveRangeData>,
    pub phys_reg_assignments: Vec<BTreeMap<RangeEndKey, PackedOption<LiveRange>>>,
}

impl<'a, M: MachineCore> RegAllocContext<'a, M> {
    pub fn new(lir: &'a Lir<M>, cfg_ctx: &'a CfgContext) -> Self {
        Self {
            lir,
            cfg_ctx,
            vreg_ranges: SecondaryMap::new(),
            pre_instr_preg_copies: SecondaryMap::new(),
            post_instr_preg_copies: SecondaryMap::new(),
            live_ranges: PrimaryMap::new(),
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
    }
}

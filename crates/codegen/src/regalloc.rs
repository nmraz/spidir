// TODO: Remove this once enough of the allocator is implemented.
#![allow(dead_code)]

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

use self::types::{LiveRange, LiveRangeData, PhysRegCopy, RangeEndKey, TaggedLiveRange};

mod liveness;
mod types;

pub fn run<M: MachineCore>(lir: &Lir<M>, cfg_ctx: &CfgContext) {
    let mut ctx = RegAllocContext::new(lir, cfg_ctx);
    ctx.compute_live_ranges();
    ctx.dump();
}

struct RegAllocContext<'a, M: MachineCore> {
    lir: &'a Lir<M>,
    cfg_ctx: &'a CfgContext,
    vreg_ranges: SecondaryMap<VirtRegNum, SmallVec<[TaggedLiveRange; 4]>>,
    pre_instr_preg_copies: SecondaryMap<Instr, SmallVec<[PhysRegCopy; 2]>>,
    post_instr_preg_copies: SecondaryMap<Instr, SmallVec<[PhysRegCopy; 2]>>,
    live_ranges: PrimaryMap<LiveRange, LiveRangeData>,
    phys_reg_assignments: Vec<BTreeMap<RangeEndKey, PackedOption<LiveRange>>>,
}

impl<'a, M: MachineCore> RegAllocContext<'a, M> {
    fn new(lir: &'a Lir<M>, cfg_ctx: &'a CfgContext) -> Self {
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

    fn dump(&self) {
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

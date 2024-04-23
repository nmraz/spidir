use core::cell::RefCell;

use alloc::{collections::BTreeMap, vec, vec::Vec};
use cranelift_entity::{EntitySet, PrimaryMap, SecondaryMap};
use fx_utils::FxHashMap;
use itertools::Itertools;
use log::{debug, log_enabled};
use smallvec::SmallVec;

use crate::{
    cfg::CfgContext,
    lir::{Lir, PhysReg, VirtRegNum},
    machine::MachineRegalloc,
    regalloc::types::PhysRegCopyDir,
};

use super::types::{
    AnnotatedPhysRegHint, FragmentQueue, LiveRange, LiveRangeData, LiveSet, LiveSetData,
    LiveSetFragment, LiveSetFragmentData, PhysRegReservation, RangeEndKey,
};

pub struct RegAllocContext<'a, M: MachineRegalloc> {
    pub lir: &'a Lir<M>,
    pub cfg_ctx: &'a CfgContext,
    pub machine: &'a M,
    pub vreg_ranges: SecondaryMap<VirtRegNum, SmallVec<[LiveRange; 4]>>,
    pub live_ranges: PrimaryMap<LiveRange, LiveRangeData>,
    pub live_range_hints: FxHashMap<LiveRange, SmallVec<[AnnotatedPhysRegHint; 2]>>,
    pub live_set_fragments: PrimaryMap<LiveSetFragment, LiveSetFragmentData>,
    pub live_sets: PrimaryMap<LiveSet, LiveSetData>,
    pub phys_reg_reservations: Vec<Vec<PhysRegReservation>>,
    pub phys_reg_assignments: Vec<BTreeMap<RangeEndKey, LiveRange>>,
    pub worklist: FragmentQueue,
    pub fragment_conflict_scratch: RefCell<EntitySet<LiveSetFragment>>,
}

impl<'a, M: MachineRegalloc> RegAllocContext<'a, M> {
    pub fn new(lir: &'a Lir<M>, cfg_ctx: &'a CfgContext, machine: &'a M) -> Self {
        let phys_reg_count = M::phys_reg_count() as usize;
        Self {
            lir,
            cfg_ctx,
            machine,
            vreg_ranges: SecondaryMap::new(),
            live_ranges: PrimaryMap::new(),
            live_range_hints: FxHashMap::default(),
            live_set_fragments: PrimaryMap::new(),
            live_sets: PrimaryMap::new(),
            phys_reg_reservations: vec![Vec::new(); phys_reg_count],
            phys_reg_assignments: vec![BTreeMap::new(); phys_reg_count],
            worklist: FragmentQueue::new(),
            fragment_conflict_scratch: Default::default(),
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

            debug!("{}:", vreg);

            for &range in ranges {
                debug!("  {:?}", self.live_ranges[range].prog_range);
                for instr in &self.live_ranges[range].instrs {
                    debug!("    {instr:?}");
                }
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

        if !self.lir.live_in_regs().is_empty() {
            debug!(
                "live-in copies: {}",
                self.lir
                    .live_in_regs()
                    .iter()
                    .zip(self.lir.block_params(self.cfg_ctx.block_order[0]))
                    .format_with(", ", |(&preg, &vreg), f| {
                        f(&format_args!("{} -> {}", M::reg_name(preg), vreg.reg_num()))
                    })
            )
        }

        debug!("phys reg reservations:");

        for preg_num in 0..M::phys_reg_count() {
            let reservations = &self.phys_reg_reservations[preg_num as usize];
            if reservations.is_empty() {
                continue;
            }

            debug!("{}:", M::reg_name(PhysReg::new(preg_num as u8)));

            for reservation in reservations {
                let range = reservation.prog_range;
                if let Some(copied_live_range) = reservation.copied_live_range {
                    let dir_arrow = match copied_live_range.direction {
                        PhysRegCopyDir::ToPhys => "<-",
                        PhysRegCopyDir::FromPhys => "->",
                    };
                    debug!(
                        "  {range:?} {} {}",
                        dir_arrow, self.live_ranges[copied_live_range.live_range].vreg
                    );
                } else {
                    debug!("  {range:?}");
                }
            }
        }

        debug!("phys reg assignments:");
        for preg_num in 0..M::phys_reg_count() {
            let assignments = &self.phys_reg_assignments[preg_num as usize];
            if assignments.is_empty() {
                continue;
            }

            debug!("{}:", M::reg_name(PhysReg::new(preg_num as u8)));

            for (&range_key, &live_range) in assignments {
                let range = range_key.0;
                debug!("  {range:?} ({})", self.live_ranges[live_range].vreg);
            }
        }
    }
}

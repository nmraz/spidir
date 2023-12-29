use core::fmt;

use alloc::collections::VecDeque;
use cranelift_entity::SecondaryMap;
use fx_utils::FxHashSet;
use log::trace;
use smallvec::SmallVec;

use crate::{
    cfg::{Block, CfgContext},
    lir::{Instr, Lir, OperandPos, VirtRegNum, VirtRegSet},
    machine::MachineCore,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ProgramPointDisp {
    Before,
    Early,
    Late,
    After,
}

impl fmt::Debug for ProgramPointDisp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Before => write!(f, "B"),
            Self::Early => write!(f, "E"),
            Self::Late => write!(f, "L"),
            Self::After => write!(f, "A"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ProgramPoint {
    pub instr: Instr,
    pub disp: ProgramPointDisp,
}

impl ProgramPoint {
    pub fn before(instr: Instr) -> Self {
        Self {
            instr,
            disp: ProgramPointDisp::Before,
        }
    }

    pub fn after(instr: Instr) -> Self {
        Self {
            instr,
            disp: ProgramPointDisp::After,
        }
    }

    pub fn for_operand(instr: Instr, pos: OperandPos) -> Self {
        let disp = match pos {
            OperandPos::Early => ProgramPointDisp::Early,
            OperandPos::Late => ProgramPointDisp::Late,
        };
        Self { instr, disp }
    }
}

impl fmt::Debug for ProgramPoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}", self.disp, self.instr)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ProgramSegment {
    pub start: ProgramPoint,
    pub end: ProgramPoint,
}

impl ProgramSegment {
    pub fn new(start: ProgramPoint, end: ProgramPoint) -> Self {
        Self { start, end }
    }
}

impl fmt::Debug for ProgramSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}

pub type LiveRange = SmallVec<[ProgramSegment; 2]>;
pub type LiveRanges = SecondaryMap<VirtRegNum, LiveRange>;

type ActiveSegmentEnds = SecondaryMap<VirtRegNum, Option<ProgramPoint>>;

pub fn compute_live_ranges<M: MachineCore>(lir: &Lir<M>, cfg_ctx: &CfgContext) -> LiveRanges {
    let mut raw_block_uses = make_block_vreg_map(lir, cfg_ctx);
    let mut raw_block_defs = make_block_vreg_map(lir, cfg_ctx);

    for &block in &cfg_ctx.block_order {
        let raw_uses = &mut raw_block_uses[block];
        let raw_defs = &mut raw_block_defs[block];

        for succ in 0..cfg_ctx.cfg.block_succs(block).len() {
            for &outgoing_vreg in lir.outgoing_block_params(block, succ as u32) {
                raw_uses.add(outgoing_vreg.reg_num());
            }
        }

        for instr in lir.block_instrs(block) {
            // We can process defs/uses in bulk here because everything is in SSA.
            for use_op in lir.instr_uses(instr) {
                raw_uses.add(use_op.reg().reg_num());
            }

            for def_op in lir.instr_defs(instr) {
                raw_defs.add(def_op.reg().reg_num());
            }
        }

        for &incoming_vreg in lir.block_params(block) {
            raw_defs.add(incoming_vreg.reg_num());
        }
    }

    let mut live_ins = make_block_vreg_map(lir, cfg_ctx);
    let mut live_outs = make_block_vreg_map(lir, cfg_ctx);

    let mut worklist: VecDeque<_> = cfg_ctx.block_order.iter().copied().rev().collect();
    let mut workset = FxHashSet::from_iter(worklist.iter().copied());

    while let Some(block) = worklist.pop_front() {
        workset.remove(&block);

        trace!("propagating liveness in {block}");

        // Note: we can do the union/subtraction in bulk here instead of iterating over individual
        // instructions because the LIR is already in SSA.
        let live_in = &mut live_ins[block];
        live_in.clone_from(&live_outs[block]);
        live_in.union(&raw_block_uses[block]);
        live_in.subtract(&raw_block_defs[block]);

        for &pred in cfg_ctx.cfg.block_preds(block) {
            let status = live_outs[pred].union(live_in);
            if status.is_changed() && !workset.contains(&pred) {
                trace!("  predecessor {pred} changed, requeuing");
                worklist.push_back(pred);
                workset.insert(pred);
            }
        }
    }

    let mut live_ranges = LiveRanges::new();
    let mut active_segment_ends = ActiveSegmentEnds::new();

    trace!("computing precise liveranges");

    for &block in cfg_ctx.block_order.iter().rev() {
        active_segment_ends.clear();

        let block_range = lir.block_instrs(block);
        let last_instr = block_range.end.prev();

        for live_out in &live_outs[block] {
            active_segment_ends[live_out] = Some(ProgramPoint::after(last_instr));
        }

        for instr in block_range.into_iter().rev() {
            // Note: we assume an instruction never uses vregs it defines.

            for def_op in lir.instr_defs(instr) {
                extend_to_def(
                    &mut live_ranges,
                    &active_segment_ends,
                    instr,
                    def_op.reg().reg_num(),
                    ProgramPoint::for_operand(instr, def_op.pos()),
                );
            }

            for use_op in lir.instr_uses(instr) {
                let vreg = use_op.reg().reg_num();
                if active_segment_ends[vreg].is_none() {
                    active_segment_ends[vreg] =
                        Some(ProgramPoint::for_operand(instr, use_op.pos()));
                }
            }
        }

        for live_in in &live_ins[block] {
            let end = active_segment_ends[live_in].expect("block live-in is dead");
            let start = ProgramPoint::before(block_range.start);
            push_live_segment(&mut live_ranges, live_in, start, end);
        }
    }

    // The live ranges have their segments sorted from last to first at this point; get them sorted
    // properly before returning them.
    for live_range in live_ranges.values_mut() {
        live_range.reverse();
    }

    live_ranges
}

fn extend_to_def(
    live_ranges: &mut LiveRanges,
    active_segment_ends: &ActiveSegmentEnds,
    instr: Instr,
    vreg: VirtRegNum,
    start: ProgramPoint,
) {
    let end = match active_segment_ends[vreg] {
        Some(segment_end) => segment_end,
        None => ProgramPoint::after(instr),
    };

    push_live_segment(live_ranges, vreg, start, end);
}

fn push_live_segment(
    live_ranges: &mut LiveRanges,
    vreg: VirtRegNum,
    start: ProgramPoint,
    end: ProgramPoint,
) {
    trace!("  extend {vreg}: {start:?}..{end:?}");
    live_ranges[vreg].push(ProgramSegment { start, end });
}

fn make_block_vreg_map<M: MachineCore>(
    lir: &Lir<M>,
    cfg_ctx: &CfgContext,
) -> SecondaryMap<Block, VirtRegSet> {
    let mut map = SecondaryMap::with_default(VirtRegSet::new(lir.vreg_count()));
    map.resize(cfg_ctx.block_order.len());
    map
}

#[cfg(test)]
mod tests {
    use cranelift_entity::EntityRef;

    use super::*;

    #[test]
    fn format_program_points() {
        let instr = Instr::new(0);
        assert_eq!(format!("{:?}", ProgramPoint::before(instr)), "B:i0");
        assert_eq!(format!("{:?}", ProgramPoint::after(instr)), "A:i0");
        assert_eq!(
            format!("{:?}", ProgramPoint::for_operand(instr, OperandPos::Early)),
            "E:i0"
        );
        assert_eq!(
            format!("{:?}", ProgramPoint::for_operand(instr, OperandPos::Late)),
            "L:i0"
        );
    }

    #[test]
    fn format_program_segments() {
        let instr = Instr::new(5);
        assert_eq!(
            format!(
                "{:?}",
                ProgramSegment::new(
                    ProgramPoint::before(instr),
                    ProgramPoint::for_operand(instr.next(), OperandPos::Early)
                )
            ),
            "B:i5..E:i6"
        );
    }

    #[test]
    fn program_point_disp_order() {
        assert!(ProgramPointDisp::Before < ProgramPointDisp::Early);
        assert!(ProgramPointDisp::Early < ProgramPointDisp::Late);
        assert!(ProgramPointDisp::Late < ProgramPointDisp::After);
    }

    #[test]
    fn program_point_distinct_instr_order() {
        assert!(ProgramPoint::after(Instr::new(7)) < ProgramPoint::before(Instr::new(8)));
    }

    #[test]
    fn program_point_same_instr_order() {
        assert!(ProgramPoint::after(Instr::new(7)) > ProgramPoint::before(Instr::new(7)));
        assert!(
            ProgramPoint::for_operand(Instr::new(7), OperandPos::Early)
                > ProgramPoint::before(Instr::new(7))
        );
        assert!(
            ProgramPoint::for_operand(Instr::new(7), OperandPos::Late)
                > ProgramPoint::before(Instr::new(7))
        );
    }
}

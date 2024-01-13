use core::{cmp::Ordering, fmt};

use alloc::{
    collections::{BTreeSet, VecDeque},
    vec,
};
use cranelift_entity::SecondaryMap;
use fx_utils::FxHashSet;
use log::trace;
use smallvec::SmallVec;

use crate::{
    cfg::{Block, CfgContext},
    lir::{Instr, Lir, OperandPos, PhysReg, VirtReg, VirtRegNum, VirtRegSet},
    machine::MachineCore,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ProgramPointDisp {
    Before = 0,
    After = 1,
}

impl fmt::Debug for ProgramPointDisp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Before => write!(f, "B"),
            Self::After => write!(f, "A"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ProgramPoint {
    index: u32,
}

impl ProgramPoint {
    pub fn new(instr: Instr, disp: ProgramPointDisp) -> Self {
        let instr = instr.as_u32();
        let disp = disp as u32;
        assert!(instr <= i32::MAX as u32);
        Self {
            index: instr << 1 | disp,
        }
    }

    pub fn before(instr: Instr) -> Self {
        Self::new(instr, ProgramPointDisp::Before)
    }

    pub fn after(instr: Instr) -> Self {
        Self::new(instr, ProgramPointDisp::After)
    }

    pub fn for_operand(instr: Instr, pos: OperandPos) -> Self {
        let disp = match pos {
            OperandPos::Early => ProgramPointDisp::Before,
            OperandPos::Late => ProgramPointDisp::After,
        };
        Self::new(instr, disp)
    }

    pub fn next(self) -> Self {
        Self {
            index: self.index + 1,
        }
    }

    pub fn prev(self) -> Self {
        Self {
            index: self.index - 1,
        }
    }

    pub fn instr(self) -> Instr {
        Instr::from_u32(self.index >> 1)
    }

    pub fn disp(self) -> ProgramPointDisp {
        match self.index & 1 {
            0 => ProgramPointDisp::Before,
            1 => ProgramPointDisp::After,
            _ => unreachable!(),
        }
    }

    pub fn index(self) -> u32 {
        self.index
    }
}

impl fmt::Debug for ProgramPoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}", self.disp(), self.instr())
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ProgramRange {
    pub start: ProgramPoint,
    pub end: ProgramPoint,
}

impl ProgramRange {
    pub fn new(start: ProgramPoint, end: ProgramPoint) -> Self {
        Self { start, end }
    }

    pub fn point(point: ProgramPoint) -> Self {
        Self::new(point, point.next())
    }

    pub fn intersects(self, other: ProgramRange) -> bool {
        self.end > other.start && other.end > self.start
    }
}

impl fmt::Debug for ProgramRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}

#[derive(Debug, Clone, Copy, Eq)]
pub struct RangeEndKey(pub ProgramRange);

impl RangeEndKey {
    fn point(point: ProgramPoint) -> Self {
        Self(ProgramRange::point(point))
    }
}

impl PartialEq for RangeEndKey {
    fn eq(&self, other: &Self) -> bool {
        self.0.end == other.0.end
    }
}

impl PartialOrd for RangeEndKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for RangeEndKey {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.end.cmp(&other.0.end)
    }
}

type MutableRangeSet = BTreeSet<RangeEndKey>;

pub type RangeSet = SmallVec<[ProgramRange; 2]>;
pub type LiveSets = SecondaryMap<VirtRegNum, RangeSet>;

pub fn allocate_regs<M: MachineCore>(lir: &Lir<M>, cfg_ctx: &CfgContext, machine: &M) {
    let allocated_regs = vec![MutableRangeSet::new(); M::phys_reg_count() as usize];
    let live_sets = compute_live_sets(lir, cfg_ctx);
    for (reg_num, live_set) in live_sets.iter() {
        let vreg = lir.vreg_from_num(reg_num);
        match find_non_interfering_phys_reg(vreg, live_set, &allocated_regs, machine) {
            Some(_reg) => todo!(),
            None => todo!(),
        }
    }
}

fn find_non_interfering_phys_reg<M: MachineCore>(
    vreg: VirtReg,
    live_set: &RangeSet,
    allocated_regs: &[MutableRangeSet],
    machine: &M,
) -> Option<PhysReg> {
    machine
        .usable_regs(vreg.class())
        .iter()
        .find(|phys_reg| !live_set_intersects(live_set, &allocated_regs[phys_reg.as_u8() as usize]))
        .copied()
}

fn live_set_intersects(live_set: &RangeSet, intersection_set: &MutableRangeSet) -> bool {
    // Assumption: `intersection_set` can generally be much larger than `live_set` (it tracks a
    // physical register throughout the entire function). Avoid a complete linear search through
    // `intersection_set` by starting with the first range inside it containing the bottom endpoint
    // of `live_set`.
    let search_key = RangeEndKey::point(live_set[0].start);

    // Once we've found the first range, do a linear walk the rest of the way to avoid logarithmic
    // complexity per iteration.
    let mut intersection_ranges = intersection_set.range(search_key..).copied().peekable();
    let mut live_ranges = live_set.iter().copied().peekable();

    loop {
        // If we've run out of either list without finding an intersection, there *is* no
        // intersection.
        let Some(&live_range) = live_ranges.peek() else {
            break;
        };
        let Some(&RangeEndKey(intersection_range)) = intersection_ranges.peek() else {
            break;
        };

        if live_range.intersects(intersection_range) {
            return true;
        }

        match live_range.end.cmp(&intersection_range.end) {
            Ordering::Less => {
                live_ranges.next();
            }
            Ordering::Greater => {
                intersection_ranges.next();
            }
            Ordering::Equal => {
                // This is impossible if both ranges are non-degenerate, which we assume them to be.
                unreachable!("non-intersecting ranges had same end point")
            }
        }
    }

    false
}

type ActiveRangeEnds = SecondaryMap<VirtRegNum, Option<ProgramPoint>>;

pub fn compute_live_sets<M: MachineCore>(lir: &Lir<M>, cfg_ctx: &CfgContext) -> LiveSets {
    let mut raw_block_uses = make_block_vreg_map(lir, cfg_ctx);
    let mut raw_block_defs = make_block_vreg_map(lir, cfg_ctx);

    trace!("collecting block defs/uses");

    for &block in &cfg_ctx.block_order {
        trace!("  {block}");

        let raw_uses = &mut raw_block_uses[block];
        let raw_defs = &mut raw_block_defs[block];

        for &outgoing_vreg in lir.outgoing_block_params(block) {
            trace!("    use {}", outgoing_vreg.reg_num());
            raw_uses.add(outgoing_vreg.reg_num());
        }

        for instr in lir.block_instrs(block) {
            // We can process defs/uses in bulk here because everything is in SSA.
            for use_op in lir.instr_uses(instr) {
                trace!("    use {}", use_op.reg().reg_num());
                raw_uses.add(use_op.reg().reg_num());
            }

            for def_op in lir.instr_defs(instr) {
                trace!("    def {}", def_op.reg().reg_num());
                raw_defs.add(def_op.reg().reg_num());
            }
        }

        for &incoming_vreg in lir.block_params(block) {
            trace!("    def {}", incoming_vreg.reg_num());
            raw_defs.add(incoming_vreg.reg_num());
        }
    }

    let mut live_ins = make_block_vreg_map(lir, cfg_ctx);
    let mut live_outs = make_block_vreg_map(lir, cfg_ctx);

    let mut worklist: VecDeque<_> = cfg_ctx.block_order.iter().copied().rev().collect();
    let mut workset = FxHashSet::from_iter(worklist.iter().copied());

    trace!("solving block liveness");

    while let Some(block) = worklist.pop_front() {
        workset.remove(&block);

        trace!("  {block}");

        // Note: we can do the union/subtraction in bulk here instead of iterating over individual
        // instructions because the LIR is already in SSA.
        let live_in = &mut live_ins[block];
        live_in.clone_from(&live_outs[block]);
        live_in.union(&raw_block_uses[block]);
        live_in.subtract(&raw_block_defs[block]);

        for &pred in cfg_ctx.cfg.block_preds(block) {
            let status = live_outs[pred].union(live_in);
            if status.is_changed() && !workset.contains(&pred) {
                trace!("    predecessor {pred} changed, requeuing");
                worklist.push_back(pred);
                workset.insert(pred);
            }
        }
    }

    let mut live_sets = LiveSets::new();
    let mut active_range_ends = ActiveRangeEnds::new();

    trace!("computing precise live sets");

    for &block in cfg_ctx.block_order.iter().rev() {
        active_range_ends.clear();

        trace!("  {block}");

        let block_range = lir.block_instrs(block);
        let last_instr = block_range.end.prev();

        for live_out in &live_outs[block] {
            trace!("    live-out {live_out}");
            // Note: ranges are half-open, and we want this range to end *after* the last
            // instruction. Even if the last block in the function has live-outs (which is unusual
            // but not technically disallowed by regalloc), the range will refer to the virtual
            // past-the-end instruction.
            active_range_ends[live_out] = Some(ProgramPoint::before(last_instr.next()));
        }

        for &outgoing_vreg in lir.outgoing_block_params(block) {
            let outgoing_vreg = outgoing_vreg.reg_num();
            trace!("    outgoing param {outgoing_vreg}");
            active_range_ends[outgoing_vreg] = Some(ProgramPoint::after(last_instr));
        }

        for instr in block_range.into_iter().rev() {
            // Note: we assume an instruction never uses vregs it defines.

            for def_op in lir.instr_defs(instr) {
                extend_to_def(
                    &mut live_sets,
                    &active_range_ends,
                    def_op.reg().reg_num(),
                    ProgramPoint::for_operand(instr, def_op.pos()),
                );
            }

            for use_op in lir.instr_uses(instr) {
                let vreg = use_op.reg().reg_num();
                if active_range_ends[vreg].is_none() {
                    // Note: ranges are half-open, so we actually mark the use as extending to the
                    // next program point.
                    let pos = ProgramPoint::for_operand(instr, use_op.pos()).next();
                    active_range_ends[vreg] = Some(pos);
                }
            }
        }

        for &incoming_vreg in lir.block_params(block) {
            let incoming_vreg = incoming_vreg.reg_num();
            trace!("    incoming param {incoming_vreg}");
            extend_to_def(
                &mut live_sets,
                &active_range_ends,
                incoming_vreg,
                ProgramPoint::before(block_range.start),
            );
        }

        for live_in in &live_ins[block] {
            trace!("    live-in {live_in}");
            let end = active_range_ends[live_in].expect("block live-in is dead");
            let start = ProgramPoint::before(block_range.start);
            push_live_range(&mut live_sets, live_in, start, end);
        }
    }

    // The live sets have their ranges sorted from last to first at this point; get them sorted
    // properly before returning them.
    for live_set in live_sets.values_mut() {
        live_set.reverse();
    }

    live_sets
}

fn extend_to_def(
    live_sets: &mut LiveSets,
    active_range_ends: &ActiveRangeEnds,
    vreg: VirtRegNum,
    start: ProgramPoint,
) {
    let end = active_range_ends[vreg].unwrap_or(start.next());
    push_live_range(live_sets, vreg, start, end);
}

fn push_live_range(
    live_sets: &mut LiveSets,
    vreg: VirtRegNum,
    start: ProgramPoint,
    end: ProgramPoint,
) {
    trace!("      extend {vreg}: {start:?}..{end:?}");
    assert!(start < end);
    let live_set = &mut live_sets[vreg];
    if let Some(last_range) = live_set.last_mut() {
        assert!(end <= last_range.start);
        if end == last_range.start {
            // Extend the next range down instead of creating a new one.
            last_range.start = start;
            return;
        }
    }
    live_set.push(ProgramRange { start, end });
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
            "B:i0"
        );
        assert_eq!(
            format!("{:?}", ProgramPoint::for_operand(instr, OperandPos::Late)),
            "A:i0"
        );
    }

    #[test]
    fn format_program_ranges() {
        let instr = Instr::new(5);
        assert_eq!(
            format!(
                "{:?}",
                ProgramRange::new(
                    ProgramPoint::before(instr),
                    ProgramPoint::for_operand(instr.next(), OperandPos::Early)
                )
            ),
            "B:i5..B:i6"
        );
    }

    #[test]
    fn program_point_disp_order() {
        assert!(ProgramPointDisp::Before < ProgramPointDisp::After);
    }

    #[test]
    fn program_point_distinct_instr_order() {
        assert!(ProgramPoint::after(Instr::new(7)) < ProgramPoint::before(Instr::new(8)));
    }

    #[test]
    fn program_point_same_instr_order() {
        assert!(ProgramPoint::after(Instr::new(7)) > ProgramPoint::before(Instr::new(7)));
        assert!(
            ProgramPoint::for_operand(Instr::new(7), OperandPos::Late)
                > ProgramPoint::before(Instr::new(7))
        );
    }

    #[test]
    fn range_intersects_overlapping() {
        let a = ProgramRange::new(
            ProgramPoint::before(Instr::new(2)),
            ProgramPoint::after(Instr::new(5)),
        );
        let b = ProgramRange::new(
            ProgramPoint::after(Instr::new(2)),
            ProgramPoint::before(Instr::new(20)),
        );
        assert!(a.intersects(b));
        assert!(b.intersects(a));
    }

    #[test]
    fn range_intersects_tangent() {
        let a = ProgramRange::new(
            ProgramPoint::before(Instr::new(2)),
            ProgramPoint::after(Instr::new(5)),
        );
        let b = ProgramRange::new(
            ProgramPoint::after(Instr::new(5)),
            ProgramPoint::before(Instr::new(20)),
        );
        assert!(!a.intersects(b));
        assert!(!b.intersects(a));
    }

    #[test]
    fn range_intersects_disjoint() {
        let a = ProgramRange::new(
            ProgramPoint::before(Instr::new(2)),
            ProgramPoint::after(Instr::new(5)),
        );
        let b = ProgramRange::new(
            ProgramPoint::after(Instr::new(17)),
            ProgramPoint::before(Instr::new(20)),
        );
        assert!(!a.intersects(b));
        assert!(!b.intersects(a));
    }

    #[test]
    fn range_intersects_nested() {
        let a = ProgramRange::new(
            ProgramPoint::before(Instr::new(2)),
            ProgramPoint::after(Instr::new(30)),
        );
        let b = ProgramRange::new(
            ProgramPoint::after(Instr::new(17)),
            ProgramPoint::before(Instr::new(20)),
        );
        assert!(a.intersects(b));
        assert!(b.intersects(a));
    }
}

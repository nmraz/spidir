use core::{cmp::Ordering, fmt};

use alloc::{
    collections::{BTreeSet, VecDeque},
    vec,
};
use cranelift_entity::{packed_option::PackedOption, SecondaryMap};
use fx_utils::FxHashSet;
use itertools::izip;
use log::trace;
use smallvec::SmallVec;

use crate::{
    cfg::{Block, CfgContext},
    lir::{Instr, Lir, OperandPos, PhysReg, RegClass, VirtRegNum, VirtRegSet},
    machine::MachineCore,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum InstrSlot {
    Before = 0,
    PreCopy = 1,
    Early = 2,
    Late = 3,
}

impl fmt::Debug for InstrSlot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Before => write!(f, "B"),
            Self::PreCopy => write!(f, "P"),
            Self::Early => write!(f, "E"),
            Self::Late => write!(f, "L"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ProgramPoint {
    index: u32,
}

impl ProgramPoint {
    pub fn new(instr: Instr, slot: InstrSlot) -> Self {
        let instr = instr.as_u32();
        let slot = slot as u32;
        assert!(instr <= u32::MAX >> 2);
        Self {
            index: instr << 2 | slot,
        }
    }

    pub fn before(instr: Instr) -> Self {
        Self::new(instr, InstrSlot::Before)
    }

    pub fn pre_copy(instr: Instr) -> Self {
        Self::new(instr, InstrSlot::PreCopy)
    }

    pub fn early(instr: Instr) -> Self {
        Self::new(instr, InstrSlot::Early)
    }

    pub fn late(instr: Instr) -> Self {
        Self::new(instr, InstrSlot::Late)
    }

    pub fn for_operand(instr: Instr, pos: OperandPos) -> Self {
        let slot = match pos {
            OperandPos::Early => InstrSlot::Early,
            OperandPos::Late => InstrSlot::Late,
        };
        Self::new(instr, slot)
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
        Instr::from_u32(self.index >> 2)
    }

    pub fn slot(self) -> InstrSlot {
        match self.index & 3 {
            0 => InstrSlot::Before,
            1 => InstrSlot::PreCopy,
            2 => InstrSlot::Early,
            3 => InstrSlot::Late,
            _ => unreachable!(),
        }
    }

    pub fn index(self) -> u32 {
        self.index
    }
}

impl fmt::Debug for ProgramPoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}", self.slot(), self.instr())
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
struct RangeEndKey(ProgramRange);

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

type RangeSet = BTreeSet<RangeEndKey>;

pub type RangeList = SmallVec<[ProgramRange; 2]>;
pub type LiveSets = SecondaryMap<VirtRegNum, RangeList>;

pub fn allocate_regs<M: MachineCore>(lir: &Lir<M>, cfg_ctx: &CfgContext, machine: &M) {
    let mut allocated_regs = vec![RangeSet::new(); M::phys_reg_count() as usize];
    let mut vreg_allocations: SecondaryMap<_, PackedOption<PhysReg>> = SecondaryMap::new();

    let live_sets = compute_live_sets(lir, cfg_ctx);
    for (vreg, live_set) in live_sets.iter() {
        let class = lir.vreg_class(vreg);
        match find_non_interfering_phys_reg(class, live_set, &allocated_regs, machine) {
            Some(reg) => {
                vreg_allocations[vreg] = reg.into();
                let phys_set = &mut allocated_regs[reg.as_u8() as usize];
                for &range in live_set {
                    phys_set.insert(RangeEndKey(range));
                }
            }
            None => todo!(),
        }
    }
}

fn find_non_interfering_phys_reg<M: MachineCore>(
    class: RegClass,
    live_set: &RangeList,
    allocated_regs: &[RangeSet],
    machine: &M,
) -> Option<PhysReg> {
    machine
        .usable_regs(class)
        .iter()
        .find(|phys_reg| !live_set_intersects(live_set, &allocated_regs[phys_reg.as_u8() as usize]))
        .copied()
}

fn live_set_intersects(live_set: &RangeList, intersection_set: &RangeSet) -> bool {
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
    let (live_ins, live_outs) = compute_block_liveness(lir, cfg_ctx);

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

        let outgoing_block_params = lir.outgoing_block_params(block);
        if !outgoing_block_params.is_empty() {
            // Note: when there are outgoing block params, we know there is exactly 1 successor.
            let succ = cfg_ctx.cfg.block_succs(block)[0];
            let incoming_succ_params = lir.block_params(succ);

            for (&outgoing_vreg, &incoming_vreg) in
                izip!(outgoing_block_params, incoming_succ_params)
            {
                let outgoing_vreg = outgoing_vreg.reg_num();
                let incoming_vreg = incoming_vreg.reg_num();
                trace!("    outgoing param {outgoing_vreg} -> {incoming_vreg}");

                let pre_copy = ProgramPoint::pre_copy(last_instr);
                active_range_ends[outgoing_vreg] = Some(pre_copy);

                push_live_range(
                    &mut live_sets,
                    incoming_vreg,
                    pre_copy,
                    ProgramPoint::before(last_instr.next()),
                );
            }
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

fn compute_block_liveness<M: MachineCore>(
    lir: &Lir<M>,
    cfg_ctx: &CfgContext,
) -> (
    SecondaryMap<Block, VirtRegSet>,
    SecondaryMap<Block, VirtRegSet>,
) {
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
    (live_ins, live_outs)
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
        assert_eq!(format!("{:?}", ProgramPoint::early(instr)), "E:i0");
        assert_eq!(format!("{:?}", ProgramPoint::late(instr)), "L:i0");
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
    fn format_program_ranges() {
        let instr = Instr::new(5);
        assert_eq!(
            format!(
                "{:?}",
                ProgramRange::new(
                    ProgramPoint::early(instr),
                    ProgramPoint::for_operand(instr.next(), OperandPos::Early)
                )
            ),
            "E:i5..E:i6"
        );
    }

    #[test]
    fn program_point_slot_order() {
        assert!(InstrSlot::Early < InstrSlot::Late);
    }

    #[test]
    fn program_point_distinct_instr_order() {
        assert!(ProgramPoint::late(Instr::new(7)) < ProgramPoint::early(Instr::new(8)));
    }

    #[test]
    fn program_point_same_instr_order() {
        assert!(ProgramPoint::late(Instr::new(7)) > ProgramPoint::early(Instr::new(7)));
        assert!(
            ProgramPoint::for_operand(Instr::new(7), OperandPos::Late)
                > ProgramPoint::early(Instr::new(7))
        );
    }

    #[test]
    fn range_intersects_overlapping() {
        let a = ProgramRange::new(
            ProgramPoint::early(Instr::new(2)),
            ProgramPoint::late(Instr::new(5)),
        );
        let b = ProgramRange::new(
            ProgramPoint::late(Instr::new(2)),
            ProgramPoint::early(Instr::new(20)),
        );
        assert!(a.intersects(b));
        assert!(b.intersects(a));
    }

    #[test]
    fn range_intersects_tangent() {
        let a = ProgramRange::new(
            ProgramPoint::early(Instr::new(2)),
            ProgramPoint::late(Instr::new(5)),
        );
        let b = ProgramRange::new(
            ProgramPoint::late(Instr::new(5)),
            ProgramPoint::early(Instr::new(20)),
        );
        assert!(!a.intersects(b));
        assert!(!b.intersects(a));
    }

    #[test]
    fn range_intersects_disjoint() {
        let a = ProgramRange::new(
            ProgramPoint::early(Instr::new(2)),
            ProgramPoint::late(Instr::new(5)),
        );
        let b = ProgramRange::new(
            ProgramPoint::late(Instr::new(17)),
            ProgramPoint::early(Instr::new(20)),
        );
        assert!(!a.intersects(b));
        assert!(!b.intersects(a));
    }

    #[test]
    fn range_intersects_nested() {
        let a = ProgramRange::new(
            ProgramPoint::early(Instr::new(2)),
            ProgramPoint::late(Instr::new(30)),
        );
        let b = ProgramRange::new(
            ProgramPoint::late(Instr::new(17)),
            ProgramPoint::early(Instr::new(20)),
        );
        assert!(a.intersects(b));
        assert!(b.intersects(a));
    }
}

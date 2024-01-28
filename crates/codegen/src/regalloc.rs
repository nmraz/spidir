use core::{cmp::Ordering, fmt};

use alloc::{
    collections::{BTreeMap, VecDeque},
    vec,
    vec::Vec,
};
use cranelift_entity::{
    entity_impl,
    packed_option::{PackedOption, ReservedValue},
    PrimaryMap, SecondaryMap,
};
use fx_utils::FxHashSet;
use log::trace;
use smallvec::{smallvec, SmallVec};

use crate::{
    cfg::{Block, CfgContext},
    lir::{
        DefOperandConstraint, Instr, Lir, OperandPos, PhysReg, UseOperandConstraint, VirtRegNum,
        VirtRegSet,
    },
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

struct LiveRangeInstr {
    pos: Instr,
    _weight: f32,
}

struct LiveRangeData {
    prog_range: ProgramRange,
    _vreg: VirtRegNum,
    _fragment: LiveSetFragment,
    instrs: SmallVec<[LiveRangeInstr; 4]>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct LiveRange(u32);
entity_impl!(LiveRange, "lr");

#[derive(Debug, Clone, Copy)]
pub struct TaggedLiveRange {
    pub prog_range: ProgramRange,
    live_range: LiveRange,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct LiveSetFragment(u32);
entity_impl!(LiveSetFragment, "lf");

pub struct RegAllocContext<'a, M: MachineCore> {
    lir: &'a Lir<M>,
    cfg_ctx: &'a CfgContext,
    pub vreg_ranges: SecondaryMap<VirtRegNum, SmallVec<[TaggedLiveRange; 4]>>,
    live_ranges: PrimaryMap<LiveRange, LiveRangeData>,
    phys_reg_assignments: Vec<BTreeMap<RangeEndKey, PackedOption<LiveRange>>>,
}

impl<'a, M: MachineCore> RegAllocContext<'a, M> {
    pub fn new(lir: &'a Lir<M>, cfg_ctx: &'a CfgContext) -> Self {
        Self {
            lir,
            cfg_ctx,
            live_ranges: PrimaryMap::new(),
            vreg_ranges: SecondaryMap::new(),
            phys_reg_assignments: vec![BTreeMap::new(); M::phys_reg_count() as usize],
        }
    }

    pub fn compute_live_ranges(&mut self) {
        let live_outs = compute_block_liveouts(self.lir, self.cfg_ctx);

        trace!("computing precise live ranges");

        let mut tied_defs = SmallVec::<[bool; 4]>::new();

        for &block in self.cfg_ctx.block_order.iter().rev() {
            trace!("  {block}");

            let block_range = self.lir.block_instrs(block);
            let last_instr = block_range.end.prev();

            // In case we need to insert a copy just before a jump (either because of outgoing block
            // params or because of a split live range), we don't model the small live range of the
            // target block's vreg after the pre-copy before the jump to the target block, so we
            // need to make sure no unaccounted-for ranges might interfere with that small range.
            // For now, we achieve this by completely disallowing block terminators to define vregs.
            assert!(
                self.lir.instr_defs(last_instr).is_empty()
                    && self.lir.instr_clobbers(last_instr).is_empty()
            );

            for live_out in &live_outs[block] {
                trace!("    live-out {live_out}");
                self.open_live_out_range(live_out, block);
            }

            let outgoing_block_params = self.lir.outgoing_block_params(block);
            if !outgoing_block_params.is_empty() {
                for &outgoing_vreg in outgoing_block_params {
                    let outgoing_vreg = outgoing_vreg.reg_num();
                    trace!("    outgoing param {outgoing_vreg}");

                    // We can safely treat the last instruction in the block as using the value for
                    // spill weight purposes, because we know the copy (if there is one) will
                    // ultimately be placed here, as critical edges are already split.
                    self.record_live_use(
                        outgoing_vreg,
                        last_instr,
                        ProgramPoint::pre_copy(last_instr),
                        block,
                    );
                }
            }

            for instr in block_range.into_iter().rev() {
                // Note: we assume an instruction never uses vregs it defines.

                let defs = self.lir.instr_defs(instr);
                let uses = self.lir.instr_uses(instr);

                tied_defs.clear();
                tied_defs.resize(defs.len(), false);

                for use_op in uses {
                    let vreg = use_op.reg().reg_num();

                    let op_pos = ProgramPoint::for_operand(instr, use_op.pos()).next();
                    let pos = match use_op.constraint() {
                        UseOperandConstraint::TiedToDef(i) => {
                            let i = i as usize;
                            assert!(
                                !tied_defs[i],
                                "multiple uses tied to same def in instr {instr}"
                            );

                            let tied_def = defs[i];
                            assert!(use_op.pos() == OperandPos::Early);
                            assert!(tied_def.pos() == OperandPos::Late);

                            tied_defs[i] = true;
                            ProgramPoint::pre_copy(instr)
                        }
                        UseOperandConstraint::Fixed(preg) => {
                            // We model fixed register constraints as a pre-copy into the correct
                            // physical register, and directly reserve the relevant register for
                            // the correct range within the instruction.
                            let pre_copy = ProgramPoint::pre_copy(instr);
                            self.reserve_phys_reg(preg, ProgramRange::new(pre_copy, op_pos));
                            pre_copy
                        }
                        _ => {
                            // Note: ranges are half-open, so we actually mark the use as extending
                            // to the next program point.
                            op_pos
                        }
                    };

                    self.record_live_use(vreg, instr, pos, block);
                }

                for (i, def_op) in defs.iter().enumerate() {
                    let mut def_point = if tied_defs[i] {
                        ProgramPoint::pre_copy(instr)
                    } else {
                        ProgramPoint::for_operand(instr, def_op.pos())
                    };

                    // If the value is constrained to a single physical register, insert a copy
                    // before the next instruction and start the live range there.
                    if let DefOperandConstraint::Fixed(preg) = def_op.constraint() {
                        let copy_point = ProgramPoint::before(instr.next());
                        self.reserve_phys_reg(preg, ProgramRange::new(def_point, copy_point));
                        def_point = copy_point;
                    }

                    self.record_def(
                        def_op.reg().reg_num(),
                        def_point,
                        // If this def is dead, make sure its range always gets extended past the
                        // end of the instruction so that dead early defs always interfere with
                        // late defs.
                        ProgramPoint::before(instr.next()),
                    );
                }

                let clobbers = self.lir.instr_clobbers(instr);
                if !clobbers.is_empty() {
                    // Clobbers are equivalent to dead late defs with physical register constraints.
                    let clobber_range = ProgramRange::new(
                        ProgramPoint::late(instr),
                        ProgramPoint::before(instr.next()),
                    );
                    for clobber in clobbers.iter() {
                        self.reserve_phys_reg(clobber, clobber_range);
                    }
                }
            }

            for &incoming_vreg in self.lir.block_params(block) {
                let incoming_vreg = incoming_vreg.reg_num();
                trace!("    incoming param {incoming_vreg}");
                self.record_def(
                    incoming_vreg,
                    ProgramPoint::before(block_range.start),
                    ProgramPoint::pre_copy(block_range.start),
                );
            }
        }

        // The live range lists have their ranges sorted from last to first at this point; get them
        // sorted properly now.
        for live_ranges in self.vreg_ranges.values_mut() {
            live_ranges.reverse();
        }
    }

    fn reserve_phys_reg(&mut self, preg: PhysReg, range: ProgramRange) {
        let assignments = &mut self.phys_reg_assignments[preg.as_u8() as usize];

        // Recall that non-degenerate ranges intersect iff each range's start start point precedes
        // the other's end point. Our allocation tree is keyed by end point, so start by performing
        // the `new_range.start < other_ranges.end` comparison.
        if let Some((intersection_candidate, _)) =
            assignments.range(RangeEndKey::point(range.start)..).next()
        {
            // The ranges really will intersect if the reverse start/end comparison holds. If that
            // is the case, report the incompatibility now.
            assert!(
                intersection_candidate.0.start >= range.end,
                "attempted to reserve physical register with overlapping ranges"
            );
        }

        assignments.insert(RangeEndKey(range), None.into());
    }

    fn record_def(
        &mut self,
        vreg: VirtRegNum,
        def_point: ProgramPoint,
        dead_use_point: ProgramPoint,
    ) {
        if let Some(last_range) = self.vreg_ranges[vreg].last_mut() {
            // This def is live *somewhere*, so it must be live in the current block as well. That
            // means it should already have a block-covering range created by a use somewhere else
            // in the block (possibly a live-out).
            assert!(def_point >= last_range.prog_range.start);
            last_range.prog_range.start = def_point;
            self.live_ranges[last_range.live_range].prog_range.start = def_point;
        } else if def_point < dead_use_point {
            // If the range for a dead def would be non-degenerate, create it now. The range might
            // be degenerate for dead physical-register-constrained defs, which push the def point
            // to before the next instruction.
            self.create_vreg_live_range(vreg, def_point, dead_use_point);
        }
    }

    fn record_live_use(
        &mut self,
        vreg: VirtRegNum,
        instr: Instr,
        use_point: ProgramPoint,
        block: Block,
    ) {
        let live_range = self.create_use_range(vreg, use_point, block);
        let range_instrs = &mut self.live_ranges[live_range].instrs;
        if let Some(last_instr) = range_instrs.last() {
            if last_instr.pos == instr {
                // Avoid recording the same instruction multiple times if it uses this vreg in
                // several operands.
                return;
            }
        }

        range_instrs.push(LiveRangeInstr {
            pos: instr,
            // TODO
            _weight: 0.0,
        });
    }

    fn create_use_range(
        &mut self,
        vreg: VirtRegNum,
        use_point: ProgramPoint,
        block: Block,
    ) -> LiveRange {
        let block_start = ProgramPoint::before(self.lir.block_instrs(block).start);
        if let Some(last_range) = self.vreg_ranges[vreg].last() {
            debug_assert!(last_range.prog_range.start >= block_start);
            if last_range.prog_range.start == block_start {
                // If a later use in this block has already "opened" a range, just use it.
                return last_range.live_range;
            }
        }

        self.create_vreg_live_range(vreg, block_start, use_point)
    }

    fn open_live_out_range(&mut self, vreg: VirtRegNum, block: Block) {
        let block_range = self.lir.block_instrs(block);

        // Note: ranges are half-open, and we want this range to end *after* the last
        // instruction. Even if the last block in the function has live-outs (which is unusual
        // but not technically disallowed by regalloc), the range will refer to the virtual
        // past-the-end instruction.
        self.create_vreg_live_range(
            vreg,
            ProgramPoint::before(block_range.start),
            ProgramPoint::before(block_range.end),
        );
    }

    fn create_vreg_live_range(
        &mut self,
        vreg: VirtRegNum,
        start: ProgramPoint,
        end: ProgramPoint,
    ) -> LiveRange {
        debug_assert!(end > start);
        trace!("    assigning {start:?}..{end:?} to {vreg}");
        if let Some(last_range) = self.vreg_ranges[vreg].last_mut() {
            assert!(end <= last_range.prog_range.start);
            // If possible, merge with the next range instead of creating a new one.
            if end == last_range.prog_range.start {
                last_range.prog_range.start = start;
                self.live_ranges[last_range.live_range].prog_range.start = start;
                return last_range.live_range;
            }
        }

        let prog_range = ProgramRange::new(start, end);
        let range_data = LiveRangeData {
            prog_range,
            _vreg: vreg,
            _fragment: LiveSetFragment::reserved_value(),
            instrs: smallvec![],
        };
        let live_range = self.live_ranges.push(range_data);
        self.vreg_ranges[vreg].push(TaggedLiveRange {
            prog_range,
            live_range,
        });
        live_range
    }
}

fn compute_block_liveouts<M: MachineCore>(
    lir: &Lir<M>,
    cfg_ctx: &CfgContext,
) -> SecondaryMap<Block, VirtRegSet> {
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

    live_outs
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

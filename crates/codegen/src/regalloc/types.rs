use core::{cmp::Ordering, fmt};

use alloc::collections::BinaryHeap;
use bitflags::bitflags;
use cranelift_entity::{entity_impl, packed_option::PackedOption};
use smallvec::SmallVec;

use crate::lir::{Instr, OperandPos, PhysReg, RegClass, VirtRegNum};

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
        match pos {
            OperandPos::Early => Self::early(instr),
            OperandPos::Late => Self::late(instr),
        }
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

    pub fn instr_rounded_up(self) -> Instr {
        Instr::from_u32((self.index + 3) >> 2)
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

    pub fn len(self) -> u32 {
        self.end.index() - self.start.index()
    }

    pub fn can_split_before(self, point: ProgramPoint) -> bool {
        point > self.start && point < self.end
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
    pub fn point(point: ProgramPoint) -> Self {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiveRangeOpPos {
    PreCopy = 0,
    Early = 1,
    Late = 2,
}

impl LiveRangeOpPos {
    pub fn for_instr_slot(slot: InstrSlot) -> Self {
        match slot {
            InstrSlot::Before => panic!("attempted to create `LiveRangeOpPos` for 'Before' slot"),
            InstrSlot::PreCopy => Self::PreCopy,
            InstrSlot::Early => Self::Early,
            InstrSlot::Late => Self::Late,
        }
    }

    pub fn for_lir_op_pos(lir_op_pos: OperandPos) -> Self {
        match lir_op_pos {
            OperandPos::Early => Self::Early,
            OperandPos::Late => Self::Late,
        }
    }
}

bitflags! {
    #[derive(Clone, Copy)]
    struct LiveRangeInstrFlags: u8 {
        const IS_DEF = 0b01;
        const NEEDS_REG = 0b10;
    }
}

#[derive(Clone, Copy)]
pub struct LiveRangeInstr {
    instr: Instr,
    weight: u16,
    flags: LiveRangeInstrFlags,
    op_pos: Option<LiveRangeOpPos>,
}

impl LiveRangeInstr {
    pub fn new(
        instr: Instr,
        weight: f32,
        is_def: bool,
        needs_reg: bool,
        op_pos: Option<LiveRangeOpPos>,
    ) -> Self {
        let mut flags = LiveRangeInstrFlags::empty();
        flags.set(LiveRangeInstrFlags::IS_DEF, is_def);
        flags.set(LiveRangeInstrFlags::NEEDS_REG, needs_reg);

        Self {
            instr,
            weight: encode_weight(weight),
            flags,
            op_pos,
        }
    }

    pub fn instr(self) -> Instr {
        self.instr
    }

    pub fn is_def(self) -> bool {
        self.flags.contains(LiveRangeInstrFlags::IS_DEF)
    }

    pub fn set_needs_reg(&mut self, needs_reg: bool) {
        self.flags.set(LiveRangeInstrFlags::NEEDS_REG, needs_reg);
    }

    pub fn needs_reg(self) -> bool {
        self.flags.contains(LiveRangeInstrFlags::NEEDS_REG)
    }

    pub fn op_pos(self) -> Option<LiveRangeOpPos> {
        self.op_pos
    }

    pub fn set_op_pos(&mut self, op_pos: LiveRangeOpPos) {
        self.op_pos = Some(op_pos);
    }

    pub fn weight(self) -> f32 {
        decode_weight(self.weight)
    }
}

impl fmt::Debug for LiveRangeInstr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let def_use_mark = if self.is_def() { "D" } else { "U" };
        let mem_reg_mark = if self.needs_reg() { "R" } else { "M" };
        write!(f, "{} [{}{}]", self.instr(), def_use_mark, mem_reg_mark)
    }
}

const F32_EXPONENT_BITS: u32 = 8;
const F32_MANTISSA_BITS: u32 = 23;
const WEIGHT_ENCODE_SHIFT: u32 = F32_EXPONENT_BITS + F32_MANTISSA_BITS - 16;

fn encode_weight(weight: f32) -> u16 {
    debug_assert!(weight >= 0.0);
    let bits = weight.to_bits();
    (bits >> WEIGHT_ENCODE_SHIFT) as u16
}

fn decode_weight(encoded_weight: u16) -> f32 {
    f32::from_bits((encoded_weight as u32) << WEIGHT_ENCODE_SHIFT)
}

pub type LiveRangeInstrs = SmallVec<[LiveRangeInstr; 4]>;

pub struct LiveRangeData {
    pub prog_range: ProgramRange,
    pub vreg: VirtRegNum,
    pub fragment: LiveSetFragment,
    pub instrs: LiveRangeInstrs,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct LiveRange(u32);
entity_impl!(LiveRange, "lr");

#[derive(Debug, Clone, Copy)]
pub struct TaggedLiveRange {
    pub prog_range: ProgramRange,
    pub live_range: LiveRange,
}

pub type TaggedRangeList = SmallVec<[TaggedLiveRange; 4]>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct LiveSet(u32);
entity_impl!(LiveSet, "ls");

pub struct LiveSetData {
    pub spill_hull: Option<ProgramRange>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct LiveSetFragment(u32);
entity_impl!(LiveSetFragment);

impl fmt::Display for LiveSetFragment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", self.0)
    }
}

impl fmt::Debug for LiveSetFragment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[derive(Clone, Copy)]
pub struct PhysRegHint {
    pub preg: PhysReg,
    pub weight: f32,
}

#[derive(Clone, Copy)]
pub struct AnnotatedPhysRegHint {
    pub hint: PhysRegHint,
    pub instr: Instr,
}

pub type PhysRegHints = SmallVec<[PhysRegHint; 2]>;

pub struct LiveSetFragmentData {
    pub live_set: LiveSet,
    pub ranges: TaggedRangeList,
    pub class: RegClass,
    pub hints: PhysRegHints,
    pub assignment: PackedOption<PhysReg>,
    pub size: u32,
    pub spill_weight: f32,
    pub is_atomic: bool,
}

impl LiveSetFragmentData {
    pub fn hull(&self) -> ProgramRange {
        ProgramRange::new(
            self.ranges.first().unwrap().prog_range.start,
            self.ranges.last().unwrap().prog_range.end,
        )
    }
}

#[derive(Clone, Copy)]
pub enum PhysRegCopyDir {
    ToPhys,
    FromPhys,
}

#[derive(Clone, Copy)]
pub struct LiveRangePhysCopy {
    pub live_range: LiveRange,
    pub direction: PhysRegCopyDir,
}

#[derive(Clone, Copy)]
pub struct PhysRegReservation {
    pub prog_range: ProgramRange,
    pub copied_live_range: Option<LiveRangePhysCopy>,
}

#[derive(Clone, Copy, Eq)]
pub struct QueuedFragment {
    pub fragment: LiveSetFragment,
    pub size: u32,
}

impl PartialEq for QueuedFragment {
    fn eq(&self, other: &Self) -> bool {
        self.size == other.size
    }
}

impl PartialOrd for QueuedFragment {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for QueuedFragment {
    fn cmp(&self, other: &Self) -> Ordering {
        self.size.cmp(&other.size)
    }
}

pub type FragmentQueue = BinaryHeap<QueuedFragment>;

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
    fn program_point_instr() {
        assert_eq!(ProgramPoint::early(Instr::new(5)).instr(), Instr::new(5));
        assert_eq!(ProgramPoint::late(Instr::new(5)).instr(), Instr::new(5));
        assert_eq!(ProgramPoint::before(Instr::new(5)).instr(), Instr::new(5));

        assert_eq!(
            ProgramPoint::early(Instr::new(5)).instr_rounded_up(),
            Instr::new(6)
        );
        assert_eq!(
            ProgramPoint::late(Instr::new(5)).instr_rounded_up(),
            Instr::new(6)
        );
        assert_eq!(
            ProgramPoint::before(Instr::new(5)).instr_rounded_up(),
            Instr::new(5)
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

    #[test]
    fn range_can_split_before() {
        let range = ProgramRange::new(
            ProgramPoint::early(Instr::new(7)),
            ProgramPoint::pre_copy(Instr::new(32)),
        );
        assert!(!range.can_split_before(range.start));
        assert!(!range.can_split_before(range.end));

        assert!(range.can_split_before(ProgramPoint::late(Instr::new(7))));
        assert!(range.can_split_before(ProgramPoint::before(Instr::new(10))));
        assert!(range.can_split_before(ProgramPoint::before(Instr::new(32))));
        assert!(range.can_split_before(ProgramPoint::late(Instr::new(31))));
    }

    #[test]
    fn encode_weight_precision() {
        fn check(weight: f32) {
            assert_eq!(weight, decode_weight(encode_weight(weight)));
        }

        check(1.0);
        check(10.0);
        check(500.0);
        check(1000.0);
        check(2000.0);
        check(3000.0);
        check(4000.0);
        // We start losing precision around 5000, but 4 nested loops is pretty extreme...
        check(8000.0);
    }
}

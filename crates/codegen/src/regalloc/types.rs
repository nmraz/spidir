use core::{cmp::Ordering, fmt, marker::PhantomData};

use alloc::{collections::BinaryHeap, vec::Vec};
use bitflags::bitflags;
use cranelift_entity::{
    entity_impl,
    packed_option::{PackedOption, ReservedValue},
};
use smallvec::SmallVec;

use crate::{
    cfg::Block,
    lir::{Instr, Lir, MemLayout, OperandPos, PhysReg, RegClass, VirtReg},
    machine::MachineCore,
};

use super::display::{DisplayAssignmentCopySource, DisplayOperandAssignment};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SpillSlot(u32);
entity_impl!(SpillSlot, "spill");

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
            index: (instr << 2) | slot,
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

    pub fn contains(self, other: ProgramRange) -> bool {
        other.start >= self.start && other.end <= self.end
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

bitflags! {
    #[derive(Clone, Copy)]
    pub struct LiveRangeFlags: u8 {
        const SPILL_CONNECTOR = 0b01;
    }
}

pub struct LiveRangeData {
    pub prog_range: ProgramRange,
    pub vreg: VirtReg,
    pub fragment: LiveSetFragment,
    pub instrs: LiveRangeInstrs,
    pub flags: LiveRangeFlags,
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
    pub class: RegClass,
    pub spill_hull: Option<ProgramRange>,
    pub spill_slot: PackedOption<SpillSlot>,
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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FragmentCopyHint(u32);
entity_impl!(FragmentCopyHint);

#[derive(Clone, Copy)]
pub struct FragmentCopyHintData {
    raw: u32,
}

impl FragmentCopyHintData {
    pub fn new(a: LiveSetFragment, b: LiveSetFragment, is_mandatory: bool) -> Self {
        let a = a.as_u32();
        let b = b.as_u32();

        debug_assert!(a & FRAGMENT_COPY_MANDATORY_HINT == 0);
        debug_assert!(b & FRAGMENT_COPY_MANDATORY_HINT == 0);

        let mut raw = a ^ b;
        if is_mandatory {
            raw |= FRAGMENT_COPY_MANDATORY_HINT;
        }

        Self { raw }
    }

    pub fn get_other_fragment(self, fragment: LiveSetFragment) -> LiveSetFragment {
        LiveSetFragment::from_u32((self.raw ^ fragment.as_u32()) & !FRAGMENT_COPY_MANDATORY_HINT)
    }

    pub fn is_mandatory(self) -> bool {
        self.raw & FRAGMENT_COPY_MANDATORY_HINT != 0
    }

    pub fn replace_fragment(
        &mut self,
        old_fragment: LiveSetFragment,
        new_fragment: LiveSetFragment,
    ) {
        debug_assert!(new_fragment.as_u32() & FRAGMENT_COPY_MANDATORY_HINT == 0);
        self.raw ^= old_fragment.as_u32() ^ new_fragment.as_u32();
    }
}

const FRAGMENT_COPY_MANDATORY_HINT: u32 = 1 << 31;

#[derive(Clone, Copy)]
pub struct TaggedFragmentCopyHint {
    pub hint: FragmentCopyHint,
    pub instr: Instr,
}

pub type TaggedFragmentCopyHints = SmallVec<[TaggedFragmentCopyHint; 2]>;

bitflags! {
    #[derive(Clone, Copy)]
    pub struct LiveSetFragmentFlags: u8 {
        const ATOMIC = 0b0001;
        const SPILLED = 0b0010;
        const CHEAPLY_REMATTABLE = 0b0100;
        const HAS_UNCOALESCED_COPY_HINTS = 0b1000;
    }
}

pub struct LiveSetFragmentData {
    pub live_set: LiveSet,
    pub prev_split_neighbor: PackedOption<LiveSetFragment>,
    pub next_split_neighbor: PackedOption<LiveSetFragment>,
    pub ranges: TaggedRangeList,
    pub phys_hints: PhysRegHints,
    pub assignment: PackedOption<PhysReg>,
    pub assignment_hint_weight: f32,
    pub size: u32,
    pub spill_weight: f32,
    pub flags: LiveSetFragmentFlags,
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
pub struct PhysRegReservation {
    pub prog_range: ProgramRange,
    pub copied_live_range: PackedOption<LiveRange>,
}

#[derive(Clone, Copy, Eq)]
pub struct QueuedFragment {
    pub fragment: LiveSetFragment,
    pub prio: u32,
}

impl PartialEq for QueuedFragment {
    fn eq(&self, other: &Self) -> bool {
        self.prio == other.prio
    }
}

impl PartialOrd for QueuedFragment {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for QueuedFragment {
    fn cmp(&self, other: &Self) -> Ordering {
        self.prio.cmp(&other.prio)
    }
}

pub type FragmentQueue = BinaryHeap<QueuedFragment>;

#[derive(Debug, Clone, Copy)]
pub enum ConflictBoundary {
    StartsAt(Instr),
    EndsAt(Instr),
}

impl ConflictBoundary {
    pub fn instr(self) -> Instr {
        match self {
            ConflictBoundary::StartsAt(instr) => instr,
            ConflictBoundary::EndsAt(instr) => instr,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParallelCopyPhase {
    /// Corresponds to the `Before` instruction slot:
    /// * Copies out of fixed def operands
    /// * Intra-block copies between fragments belonging to the same vreg
    /// * Copies for block live-ins when there is a unique predecessor
    /// * Copies into spill slots for def operands that have been spilled
    Before,
    /// Corresponds to the `Before` instruction slot:
    /// * Reloads from spill slots for use operands that have been spilled
    ///
    /// This is a dedicated phase so that spill -> reload sequences between adjacent instructions
    /// work correctly.
    Reload,
    /// Corresponds to the `PreCopy` instruction slot:
    /// * Copies into fixed use operands
    /// * Copies into tied def operands
    /// * Copies for block live-outs when there is a unique successor
    /// * Copies for outgoing block params
    PreCopy,
}

impl ParallelCopyPhase {
    pub fn slot(self) -> InstrSlot {
        match self {
            ParallelCopyPhase::Before | ParallelCopyPhase::Reload => InstrSlot::Before,
            ParallelCopyPhase::PreCopy => InstrSlot::PreCopy,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum RematCost {
    CheapAsCopy = 0,
    CheapAsLoad = 1,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct InstrWithRematCost {
    packed: u32,
}

impl InstrWithRematCost {
    pub fn new(instr: Instr, cost: RematCost) -> Self {
        let instr = instr.as_u32();
        let cost = cost as u32;
        assert!(instr <= u32::MAX >> 1);
        Self {
            packed: (instr << 1) | cost,
        }
    }

    pub fn instr(self) -> Instr {
        Instr::from_u32(self.packed >> 1)
    }

    pub fn cost(self) -> RematCost {
        match self.packed & 1 {
            0 => RematCost::CheapAsCopy,
            1 => RematCost::CheapAsLoad,
            _ => unreachable!(),
        }
    }
}

impl ReservedValue for InstrWithRematCost {
    fn reserved_value() -> Self {
        Self { packed: u32::MAX }
    }

    fn is_reserved_value(&self) -> bool {
        self.packed == u32::MAX
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum OperandAssignment {
    Reg(PhysReg),
    Spill(SpillSlot),
}

impl OperandAssignment {
    pub fn is_spill(self) -> bool {
        matches!(self, Self::Spill(..))
    }

    pub fn is_reg(&self) -> bool {
        matches!(self, Self::Reg(..))
    }

    pub fn as_reg(&self) -> Option<PhysReg> {
        match self {
            &Self::Reg(reg) => Some(reg),
            _ => None,
        }
    }

    pub fn as_spill(&self) -> Option<SpillSlot> {
        match self {
            &Self::Spill(spill) => Some(spill),
            _ => None,
        }
    }

    pub fn display<M: MachineCore>(self) -> DisplayOperandAssignment<M> {
        DisplayOperandAssignment {
            assignment: self,
            _marker: PhantomData,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum CopySourceAssignment {
    Operand(OperandAssignment),
    Remat(Instr),
}

impl CopySourceAssignment {
    pub fn is_spill(self) -> bool {
        self.as_spill().is_some()
    }

    pub fn is_reg(&self) -> bool {
        self.as_reg().is_some()
    }

    pub fn is_remat(&self) -> bool {
        matches!(self, Self::Remat(..))
    }

    pub fn as_operand(&self) -> Option<OperandAssignment> {
        match self {
            &Self::Operand(operand) => Some(operand),
            _ => None,
        }
    }

    pub fn as_spill(&self) -> Option<SpillSlot> {
        match self {
            &Self::Operand(OperandAssignment::Spill(spill)) => Some(spill),
            _ => None,
        }
    }

    pub fn as_reg(&self) -> Option<PhysReg> {
        match self {
            &Self::Operand(OperandAssignment::Reg(reg)) => Some(reg),
            _ => None,
        }
    }

    pub fn display<M: MachineCore>(self, lir: &Lir<M>) -> DisplayAssignmentCopySource<'_, M> {
        DisplayAssignmentCopySource { lir, source: self }
    }
}

impl From<OperandAssignment> for CopySourceAssignment {
    fn from(operand: OperandAssignment) -> Self {
        Self::Operand(operand)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SpillSlotData {
    pub layout: MemLayout,
}

#[derive(Clone, Copy, Default)]
pub struct InstrAssignmentData {
    pub def_base: u32,
    pub def_len: u16,
    pub use_base: u32,
    pub use_len: u16,
}

#[derive(Clone, Copy)]
pub struct ParallelCopy {
    pub instr: Instr,
    pub phase: ParallelCopyPhase,
    pub class: RegClass,
    pub from: CopySourceAssignment,
    pub to: OperandAssignment,
}

pub type ParallelCopies = Vec<ParallelCopy>;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct AssignmentCopy {
    pub from: CopySourceAssignment,
    pub to: OperandAssignment,
}

#[derive(Clone, Copy)]
pub struct TaggedAssignmentCopy {
    pub instr: Instr,
    pub copy: AssignmentCopy,
}

#[derive(Clone, Copy)]
pub struct BlockExitGhostCopy {
    pub block: Block,
    pub assignment: OperandAssignment,
    pub from_vreg: VirtReg,
    pub to_vreg: VirtReg,
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
    fn range_contains_overlapping() {
        let a = ProgramRange::new(
            ProgramPoint::early(Instr::new(2)),
            ProgramPoint::late(Instr::new(5)),
        );
        let b = ProgramRange::new(
            ProgramPoint::late(Instr::new(2)),
            ProgramPoint::early(Instr::new(20)),
        );
        assert!(!a.contains(b));
        assert!(!b.contains(a));
    }

    #[test]
    fn range_contains_tangent() {
        let a = ProgramRange::new(
            ProgramPoint::early(Instr::new(2)),
            ProgramPoint::late(Instr::new(5)),
        );
        let b = ProgramRange::new(
            ProgramPoint::late(Instr::new(5)),
            ProgramPoint::early(Instr::new(20)),
        );
        assert!(!a.contains(b));
        assert!(!b.contains(a));
    }

    #[test]
    fn range_contains_disjoint() {
        let a = ProgramRange::new(
            ProgramPoint::early(Instr::new(2)),
            ProgramPoint::late(Instr::new(5)),
        );
        let b = ProgramRange::new(
            ProgramPoint::late(Instr::new(17)),
            ProgramPoint::early(Instr::new(20)),
        );
        assert!(!a.contains(b));
        assert!(!b.contains(a));
    }

    #[test]
    fn range_contains_nested() {
        let a = ProgramRange::new(
            ProgramPoint::early(Instr::new(2)),
            ProgramPoint::late(Instr::new(30)),
        );
        let b = ProgramRange::new(
            ProgramPoint::late(Instr::new(17)),
            ProgramPoint::early(Instr::new(20)),
        );
        assert!(a.contains(b));
        assert!(!b.contains(a));
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

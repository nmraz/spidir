use smallvec::{SmallVec, smallvec};

use crate::lir::{Instr, PhysReg, PhysRegSet};

use super::{
    OperandAssignment, SpillSlot,
    types::{AssignmentCopy, CopySourceAssignment, ParallelCopy},
};

pub trait RegScavenger {
    fn emergency_reg(&self) -> PhysReg;
    fn available_regs(&self) -> impl Iterator<Item = PhysReg>;
    fn alloc_tmp_spill(&mut self) -> SpillSlot;
    fn expand_tmp_spill(&mut self, spill: SpillSlot);
}

pub struct ResolverState {
    available_spills: SmallVec<[SpillSlot; 2]>,
    used_regs: PhysRegSet,
}

impl ResolverState {
    pub fn new() -> Self {
        Self {
            available_spills: SmallVec::new(),
            used_regs: PhysRegSet::empty(),
        }
    }
}

pub fn resolve(
    parallel_copies: &[ParallelCopy],
    state: &mut ResolverState,
    scavenger: &mut impl RegScavenger,
    mut collect: impl FnMut(&AssignmentCopy),
) {
    let mut operands = SmallVec::<[OperandAssignment; 16]>::new();

    for copy in parallel_copies {
        if let CopySourceAssignment::Operand(from) = copy.from {
            operands.push(from);
        }
        operands.push(copy.to);
    }

    operands.sort_unstable_by_key(operand_assignment_sort_key);
    operands.dedup();

    let mut copy_sources: SmallVec<[Option<TrackedCopySource>; 8]> =
        smallvec![None; operands.len()];

    let mut ctx = ResolvedCopyContext::prepare(state, scavenger);

    for copy in parallel_copies {
        // To keep complexity (somewhat) tame, we let the scavenger model copy sources/destinations
        // as "available" during resolution and manually ignore them ourselves. This lets scavengers
        // that actually probe live-range/allocation structures avoid doing extra work to deal with
        // annoying special cases, such as:
        //
        // * Copies at the end of live ranges, where the source might not be marked as live _at_ the
        //   copy point.
        //
        // * Copies across CFG edges, where the complexities of tracking liveness along edges mean
        //   the destination might not be marked as live in the source block (where the copies
        //   occur).
        if let CopySourceAssignment::Operand(OperandAssignment::Reg(from)) = copy.from {
            ctx.state.used_regs.insert(from);
        }
        if let OperandAssignment::Reg(to) = copy.to {
            ctx.state.used_regs.insert(to);
        }

        let from = match copy.from {
            CopySourceAssignment::Operand(from) => {
                TrackedCopySource::Operand(find_operand(&operands, from) as u32)
            }
            CopySourceAssignment::Remat(instr) => TrackedCopySource::Remat(instr),
        };

        let to = find_operand(&operands, copy.to);

        debug_assert!(
            copy_sources[to].is_none(),
            "multiple parallel copies into same destination"
        );

        copy_sources[to] = Some(from);
    }

    // We now view the set of parallel copies as a directed graph, with edges running from copy
    // destinations to their respective sources. If this graph is acyclic, sorting it topologically
    // (i.e., taking an RPO) will give us a valid order in which the copies can be performed.
    //
    // If the graph does contain cycles, we take advantage of the fact that every node has at most
    // one outgoing edge. That property means that every disjoint subgraph contains at most one
    // simple cycle, which we can easily break apart by introducing a temporary.
    //
    // Perform a modified DFS of the graph to find the RPO we want (modulo discovered cycles, which
    // require a bit of special handling):

    let mut stack = SmallVec::<[usize; 8]>::new();
    let mut visit_states: SmallVec<[_; 8]> = smallvec![VisitState::Unvisited; operands.len()];

    for copy in parallel_copies {
        debug_assert!(stack.is_empty());

        let to = find_operand(&operands, copy.to);
        if visit_states[to] != VisitState::Unvisited {
            continue;
        }

        stack.push(to);

        // Step 1: Follow the newly-discovered `to -> from` copy "chain" as far in as possible.
        loop {
            let operand = *stack.last().unwrap();

            match visit_states[operand] {
                VisitState::Unvisited => {
                    visit_states[operand] = VisitState::Visiting;

                    // Only copies out of other operands can extend the chain (if those operands
                    // themselves need to be copied into); remats don't depend on any other operand
                    // values.
                    if let Some(TrackedCopySource::Operand(src)) = copy_sources[operand] {
                        stack.push(src as usize);
                        continue;
                    }
                }
                VisitState::Visiting => {
                    // We've encountered a copy cycle `r -> ... -> s -> r`; we'll break it up below
                    // by introducing a temporary `t`, saving `r` into it before the cyclic copies,
                    // and copying from `t` into `s` (instead of from `r`).

                    // Remove the duplicate `r` from the top of the stack, and let the logic below
                    // deal with breaking the cycle.
                    stack.pop();
                }
                VisitState::Visited => {
                    // We've hit something in a previously-resolved copy chain, so back up out of
                    // it and terminate our current chain.
                    stack.pop();
                }
            }

            // If we haven't found more unvisited operands copied out of, terminate this chain now.
            break;
        }

        let mut copy_cycle_break = None;

        // Step 2: Walk back out of the chain and record copies (in reverse order!) as we go.
        while let Some(to) = stack.pop() {
            visit_states[to] = VisitState::Visited;

            // If this operand isn't actually copied out of another one, we have nothing more to do.
            let Some(from) = copy_sources[to] else {
                continue;
            };

            match from {
                TrackedCopySource::Operand(from) => {
                    let from = from as usize;

                    match visit_states[from] {
                        VisitState::Unvisited | VisitState::Visited => {
                            // `from` isn't currently on the stack anywhere, so we can just emit
                            // a direct copy without being worried about cycles.
                            let from = operands[from];
                            let to = operands[to];
                            ctx.emit(from.into(), to);
                        }
                        VisitState::Visiting => {
                            // `from` is itself somewhere lower on the destination stack (i.e.,
                            // there is a copy cycle), so we need to back it up into a temporary
                            // before it is overwritten and copy out of the temporary instead.

                            let tmp_op = ctx.alloc_tmp_op();
                            copy_cycle_break = Some((from, tmp_op));

                            // Insert the final copy out of the temporary here (resolved
                            // assignments are in reverse order).
                            ctx.emit(tmp_op.into(), operands[to]);
                        }
                    }
                }
                TrackedCopySource::Remat(instr) => {
                    ctx.emit(CopySourceAssignment::Remat(instr), operands[to]);
                }
            }

            match copy_cycle_break {
                Some((cycle_operand, tmp_op)) if cycle_operand == to => {
                    // We've found the start of a broken parallel copy cycle - make sure the
                    // original value of `operand` is saved before it is overwritten by the copy
                    // inserted above.
                    ctx.emit(operands[to].into(), tmp_op);
                    ctx.free_tmp_op(tmp_op);
                }
                _ => {}
            }
        }
    }

    // To get a proper topological sort of the copies, we need to invert our post-order traversal.
    for copy in ctx.copies.iter().rev() {
        collect(copy);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VisitState {
    Unvisited,
    Visiting,
    Visited,
}

#[derive(Debug, Clone, Copy)]
enum TrackedCopySource {
    Operand(u32),
    Remat(Instr),
}

fn find_operand(operands: &[OperandAssignment], op: OperandAssignment) -> usize {
    // This array is expected to be so small that binary search probably isn't worth it.
    operands.iter().position(|&it| it == op).unwrap()
}

fn operand_assignment_sort_key(op: &OperandAssignment) -> u32 {
    match op {
        OperandAssignment::Reg(reg) => reg.as_u8() as u32,
        OperandAssignment::Spill(slot) => {
            const SPILL_BIT: u32 = 1 << 31;
            let val = slot.as_u32();
            debug_assert!((val & SPILL_BIT) == 0);
            val | SPILL_BIT
        }
    }
}

struct ResolvedCopyContext<'s, S> {
    copies: SmallVec<[AssignmentCopy; 8]>,
    state: &'s mut ResolverState,
    scavenger: &'s mut S,
}

impl<'s, S: RegScavenger> ResolvedCopyContext<'s, S> {
    fn prepare(state: &'s mut ResolverState, scavenger: &'s mut S) -> Self {
        state.used_regs = PhysRegSet::empty();

        Self {
            copies: SmallVec::new(),
            state,
            scavenger,
        }
    }

    fn emit(&mut self, from: CopySourceAssignment, to: OperandAssignment) {
        if from.is_reg() || to.is_reg() {
            // Copies involving at least one register can be performed directly.
            self.emit_raw(from, to);
        } else {
            // Stack-to-stack copies and remat-to-stack need to go through a temporary register.

            let (tmp_reg, tmp_op) = match self.alloc_tmp_reg() {
                Some(tmp_reg) => (tmp_reg, OperandAssignment::Reg(tmp_reg)),
                None => {
                    let tmp_reg = self.scavenger.emergency_reg();
                    let emergency_spill = self.alloc_tmp_spill();

                    // Restore the original value of `tmp_reg` from the emergency spill.
                    self.emit_raw(
                        OperandAssignment::Spill(emergency_spill).into(),
                        OperandAssignment::Reg(tmp_reg),
                    );

                    (tmp_reg, OperandAssignment::Spill(emergency_spill))
                }
            };

            self.emit_raw(OperandAssignment::Reg(tmp_reg).into(), to);
            self.emit_raw(from, OperandAssignment::Reg(tmp_reg));

            match tmp_op {
                OperandAssignment::Spill(emergency_spill) => {
                    // Back up `tmp_reg` into the emergency spill before it is used above.
                    self.emit_raw(
                        OperandAssignment::Reg(tmp_reg).into(),
                        OperandAssignment::Spill(emergency_spill),
                    );
                    self.free_tmp_spill(emergency_spill);
                }
                OperandAssignment::Reg(reg) => self.free_tmp_reg(reg),
            }
        }
    }

    fn emit_raw(&mut self, from: CopySourceAssignment, to: OperandAssignment) {
        self.copies.push(AssignmentCopy { from, to });
    }

    fn alloc_tmp_op(&mut self) -> OperandAssignment {
        match self.alloc_tmp_reg() {
            Some(reg) => OperandAssignment::Reg(reg),
            None => OperandAssignment::Spill(self.alloc_tmp_spill()),
        }
    }

    fn free_tmp_op(&mut self, op: OperandAssignment) {
        match op {
            OperandAssignment::Reg(reg) => self.free_tmp_reg(reg),
            OperandAssignment::Spill(spill) => self.free_tmp_spill(spill),
        }
    }

    fn alloc_tmp_reg(&mut self) -> Option<PhysReg> {
        let reg = self
            .scavenger
            .available_regs()
            .find(|&reg| !self.state.used_regs.contains(reg))?;
        self.state.used_regs.insert(reg);
        Some(reg)
    }

    fn free_tmp_reg(&mut self, reg: PhysReg) {
        debug_assert!(self.state.used_regs.contains(reg));
        self.state.used_regs.remove(reg);
    }

    fn alloc_tmp_spill(&mut self) -> SpillSlot {
        if let Some(spill) = self.state.available_spills.pop() {
            self.scavenger.expand_tmp_spill(spill);
            return spill;
        }
        self.scavenger.alloc_tmp_spill()
    }

    fn free_tmp_spill(&mut self, spill: SpillSlot) {
        self.state.available_spills.push(spill);
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Write;

    use cranelift_entity::EntityRef;
    use expect_test::{Expect, expect};

    use crate::{
        lir::{Instr, PhysReg, RegClass},
        regalloc::{
            SpillSlot,
            test_utils::{
                copy_source_to_string, operand_to_string, parse_copy_source, parse_operand,
            },
            types::ParallelCopyPhase,
        },
    };

    use super::*;

    struct DummyRegScavenger {
        reg_count: u8,
        next_spill: u32,
    }

    impl RegScavenger for DummyRegScavenger {
        fn emergency_reg(&self) -> PhysReg {
            PhysReg::new(0)
        }

        fn available_regs(&self) -> impl Iterator<Item = PhysReg> {
            (0..self.reg_count).map(PhysReg::new)
        }

        fn alloc_tmp_spill(&mut self) -> SpillSlot {
            let spill = SpillSlot::from_u32(self.next_spill);
            self.next_spill += 1;
            spill
        }

        fn expand_tmp_spill(&mut self, _spill: SpillSlot) {
            // We don't actually reuse the spill slots across multiple distinct resolutions here.
        }
    }

    fn check_resolution_with_reg_count(reg_count: u8, input: &str, expected: Expect) {
        let mut parallel_copies = Vec::new();

        for line in input.lines() {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }

            let mut parts = line.split('=');
            let to = parts.next().unwrap();
            let from = parts.next().unwrap();
            parallel_copies.push(ParallelCopy {
                instr: Instr::new(0),
                class: RegClass::new(0),
                phase: ParallelCopyPhase::InterInstr,
                from: parse_copy_source(from.trim()),
                to: parse_operand(to.trim()),
            });
        }

        let mut scavenger = DummyRegScavenger {
            reg_count,
            next_spill: 1234,
        };

        let mut resolved = Vec::new();
        let mut state = ResolverState::new();
        resolve(&parallel_copies, &mut state, &mut scavenger, |copy| {
            resolved.push(*copy)
        });

        let mut output = String::new();
        for copy in &resolved {
            writeln!(
                output,
                "{} = {}",
                operand_to_string(copy.to),
                copy_source_to_string(copy.from),
            )
            .unwrap();
        }

        expected.assert_eq(&output);
    }

    fn check_resolution(input: &str, expected: Expect) {
        check_resolution_with_reg_count(32, input, expected);
    }

    #[test]
    fn single_copy() {
        check_resolution(
            "
            r0 = r1
            ",
            expect![[r#"
                r0 = r1
            "#]],
        )
    }

    #[test]
    fn simple_remat() {
        check_resolution(
            "
            r0 = i7
            ",
            expect![[r#"
                r0 = i7
            "#]],
        );
    }

    #[test]
    fn disjoint_copies() {
        check_resolution(
            "
            r0 = r1
            r2 = r3
            r4 = r5
            r6 = r7
            ",
            expect![[r#"
                r6 = r7
                r4 = r5
                r2 = r3
                r0 = r1
            "#]],
        )
    }

    #[test]
    fn disjoint_copies_with_remat() {
        check_resolution(
            "
            r0 = r1
            r2 = i9
            r4 = r5
            r6 = r7
            ",
            expect![[r#"
                r6 = r7
                r4 = r5
                r2 = i9
                r0 = r1
            "#]],
        )
    }

    #[test]
    fn overlapping_copy_chain() {
        check_resolution(
            "
            r0 = r1
            r1 = r2
            r2 = r3
            r3 = r4
            ",
            expect![[r#"
                r0 = r1
                r1 = r2
                r2 = r3
                r3 = r4
            "#]],
        )
    }

    #[test]
    fn overlapping_copy_chain_rev() {
        check_resolution(
            "
            r3 = r4
            r2 = r3
            r1 = r2
            r0 = r1
            ",
            expect![[r#"
                r0 = r1
                r1 = r2
                r2 = r3
                r3 = r4
            "#]],
        )
    }

    #[test]
    fn overlapping_copy_chain_with_remat() {
        check_resolution(
            "
            r0 = r1
            r1 = r2
            r2 = r3
            r3 = i0
            ",
            expect![[r#"
                r0 = r1
                r1 = r2
                r2 = r3
                r3 = i0
            "#]],
        )
    }

    #[test]
    fn overlapping_copy_chain_with_remat_rev() {
        check_resolution(
            "
            r3 = i0
            r2 = r3
            r1 = r2
            r0 = r1
            ",
            expect![[r#"
                r0 = r1
                r1 = r2
                r2 = r3
                r3 = i0
            "#]],
        )
    }

    #[test]
    fn disjoint_overlapping_copy_chains() {
        check_resolution(
            "
            r0 = r1
            r6 = r7
            r2 = r3
            r4 = r5
            r1 = r2
            r5 = r6
            ",
            expect![[r#"
                r4 = r5
                r5 = r6
                r6 = r7
                r0 = r1
                r1 = r2
                r2 = r3
            "#]],
        )
    }

    #[test]
    fn copy_chain_merge() {
        check_resolution(
            "
            r0 = r1
            r1 = r2
            r6 = r8
            r4 = r5
            r2 = r3
            r3 = r6
            r7 = r6
            r5 = r3
            ",
            expect![[r#"
                r7 = r6
                r4 = r5
                r5 = r3
                r0 = r1
                r1 = r2
                r2 = r3
                r3 = r6
                r6 = r8
            "#]],
        )
    }

    #[test]
    fn copy_chain_merge_with_remat() {
        check_resolution(
            "
            r0 = r1
            r1 = r2
            r6 = i8
            r4 = r5
            r2 = r3
            r3 = r6
            r7 = r6
            r5 = r3
            ",
            expect![[r#"
                r7 = r6
                r4 = r5
                r5 = r3
                r0 = r1
                r1 = r2
                r2 = r3
                r3 = r6
                r6 = i8
            "#]],
        )
    }

    #[test]
    fn swap_regs() {
        check_resolution(
            "
            r0 = r1
            r1 = r0
            ",
            expect![[r#"
                r2 = r0
                r0 = r1
                r1 = r2
            "#]],
        )
    }

    #[test]
    fn swap_regs_with_chain_merge() {
        check_resolution(
            "
            r4 = r2
            r0 = r1
            r1 = r0
            r2 = r0
            r3 = r1
            ",
            expect![[r#"
                r3 = r1
                r4 = r2
                r2 = r0
                r5 = r0
                r0 = r1
                r1 = r5
            "#]],
        )
    }

    #[test]
    fn disjoint_swaps() {
        check_resolution(
            "
            r0 = r1
            r2 = r3
            r1 = r0
            r3 = r2
            ",
            expect![[r#"
                r4 = r2
                r2 = r3
                r3 = r4
                r4 = r0
                r0 = r1
                r1 = r4
            "#]],
        )
    }

    #[test]
    fn large_copy_cycle() {
        check_resolution(
            "
            r0 = r1
            r1 = r2
            r2 = r3
            r3 = r4
            r4 = r5
            r5 = r0
            ",
            expect![[r#"
                r6 = r0
                r0 = r1
                r1 = r2
                r2 = r3
                r3 = r4
                r4 = r5
                r5 = r6
            "#]],
        )
    }

    #[test]
    fn disjoint_large_copy_cycles_with_chain_merge() {
        check_resolution(
            "
            r1 = r2
            r2 = r3
            r10 = r11
            r12 = r8
            r3 = r0
            r6 = r4
            r0 = r1
            r5 = r4
            r4 = r1
            r8 = r9
            r9 = r10
            r11 = r8
            ",
            expect![[r#"
                r5 = r4
                r6 = r4
                r4 = r1
                r12 = r8
                r7 = r10
                r10 = r11
                r11 = r8
                r8 = r9
                r9 = r7
                r7 = r1
                r1 = r2
                r2 = r3
                r3 = r0
                r0 = r7
            "#]],
        )
    }

    #[test]
    fn copy_stack_to_reg() {
        check_resolution(
            "
            r0 = s0
            ",
            expect![[r#"
                r0 = s0
            "#]],
        )
    }

    #[test]
    fn copy_reg_to_stack() {
        check_resolution(
            "
            s0 = r0
            ",
            expect![[r#"
                s0 = r0
            "#]],
        )
    }

    #[test]
    fn copy_stack_to_stack() {
        check_resolution(
            "
            s1 = s0
            ",
            expect![[r#"
                r0 = s0
                s1 = r0
            "#]],
        )
    }

    #[test]
    fn remat_to_stack() {
        check_resolution(
            "
            s1 = i0
            ",
            expect![[r#"
                r0 = i0
                s1 = r0
            "#]],
        )
    }

    #[test]
    fn disjoint_stack_copies() {
        check_resolution(
            "
            s1 = s0
            s3 = s2
            ",
            expect![[r#"
                r0 = s2
                s3 = r0
                r0 = s0
                s1 = r0
            "#]],
        )
    }

    #[test]
    fn overlapping_stack_copy_chain() {
        check_resolution(
            "
            s1 = s0
            s3 = s2
            s2 = s1
            ",
            expect![[r#"
                r0 = s2
                s3 = r0
                r0 = s1
                s2 = r0
                r0 = s0
                s1 = r0
            "#]],
        )
    }

    #[test]
    fn large_stack_copy_cycle() {
        check_resolution(
            "
            s0 = s1
            s1 = s2
            s2 = s3
            s3 = s4
            s4 = s5
            s5 = s0
            ",
            expect![[r#"
                r0 = s0
                r1 = s1
                s0 = r1
                r1 = s2
                s1 = r1
                r1 = s3
                s2 = r1
                r1 = s4
                s3 = r1
                r1 = s5
                s4 = r1
                s5 = r0
            "#]],
        )
    }

    #[test]
    fn copy_stack_to_stack_no_regs() {
        check_resolution_with_reg_count(
            0,
            "
            s0 = s1
            ",
            expect![[r#"
                s1234 = r0
                r0 = s1
                s0 = r0
                r0 = s1234
            "#]],
        )
    }

    #[test]
    fn copy_multiple_stack_to_stack_no_regs() {
        check_resolution_with_reg_count(
            0,
            "
            s1 = s2
            s0 = s1
            ",
            expect![[r#"
                s1234 = r0
                r0 = s1
                s0 = r0
                r0 = s1234
                s1234 = r0
                r0 = s2
                s1 = r0
                r0 = s1234
            "#]],
        )
    }

    #[test]
    fn large_copy_cycle_no_regs() {
        check_resolution_with_reg_count(
            6,
            "
            r0 = r1
            r1 = r2
            r2 = r3
            r3 = r4
            r4 = r5
            r5 = r0
            ",
            expect![[r#"
                s1234 = r0
                r0 = r1
                r1 = r2
                r2 = r3
                r3 = r4
                r4 = r5
                r5 = s1234
            "#]],
        )
    }

    #[test]
    fn large_stack_copy_cycle_single_reg() {
        check_resolution_with_reg_count(
            1,
            "
            s0 = s1
            s1 = s2
            s2 = s3
            s3 = s4
            s4 = s5
            s5 = s0
            ",
            expect![[r#"
                r0 = s0
                s1234 = r0
                r0 = s1
                s0 = r0
                r0 = s1234
                s1234 = r0
                r0 = s2
                s1 = r0
                r0 = s1234
                s1234 = r0
                r0 = s3
                s2 = r0
                r0 = s1234
                s1234 = r0
                r0 = s4
                s3 = r0
                r0 = s1234
                s1234 = r0
                r0 = s5
                s4 = r0
                r0 = s1234
                s5 = r0
            "#]],
        )
    }

    #[test]
    fn large_stack_copy_cycle_no_regs() {
        check_resolution_with_reg_count(
            0,
            "
            s0 = s1
            s1 = s2
            s2 = s3
            s3 = s4
            s4 = s5
            s5 = s0
            ",
            expect![[r#"
                s1235 = r0
                r0 = s0
                s1234 = r0
                r0 = s1235
                s1235 = r0
                r0 = s1
                s0 = r0
                r0 = s1235
                s1235 = r0
                r0 = s2
                s1 = r0
                r0 = s1235
                s1235 = r0
                r0 = s3
                s2 = r0
                r0 = s1235
                s1235 = r0
                r0 = s4
                s3 = r0
                r0 = s1235
                s1235 = r0
                r0 = s5
                s4 = r0
                r0 = s1235
                s1235 = r0
                r0 = s1234
                s5 = r0
                r0 = s1235
            "#]],
        )
    }
}

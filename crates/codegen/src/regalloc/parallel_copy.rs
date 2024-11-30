use smallvec::{smallvec, SmallVec};

use crate::lir::PhysReg;

use super::{
    types::{AssignmentCopy, ParallelCopy},
    OperandAssignment, SpillSlot,
};

pub trait RegScavenger {
    fn emergency_reg(&self) -> PhysReg;
    fn get_fresh_tmp_reg(&mut self) -> Option<PhysReg>;
    fn get_fresh_tmp_spill(&mut self) -> SpillSlot;
}

pub fn resolve(
    parallel_copies: &[ParallelCopy],
    scavenger: &mut impl RegScavenger,
    mut collect: impl FnMut(&AssignmentCopy),
) {
    let mut operands = SmallVec::<[OperandAssignment; 16]>::new();

    for copy in parallel_copies {
        operands.push(copy.from);
        operands.push(copy.to);
    }

    operands.sort_unstable_by_key(operand_assignment_sort_key);
    operands.dedup();

    let mut copy_sources: SmallVec<[Option<u32>; 8]> = smallvec![None; operands.len()];

    for copy in parallel_copies {
        let from = find_operand(&operands, copy.from);
        let to = find_operand(&operands, copy.to);
        debug_assert!(
            copy_sources[to].is_none(),
            "multiple parallel copies into same destination"
        );

        copy_sources[to] = Some(from as u32);
    }

    let mut ctx = ResolvedCopyContext::new(scavenger);

    let mut stack = SmallVec::<[usize; 8]>::new();
    let mut visit_states: SmallVec<[_; 8]> = smallvec![VisitState::Unvisited; operands.len()];
    for copy in parallel_copies {
        debug_assert!(stack.is_empty());

        let to = find_operand(&operands, copy.to);
        if visit_states[to] != VisitState::Unvisited {
            continue;
        }

        stack.push(to);

        let mut last_copy_src = None;
        let mut copy_cycle_break = None;

        // Step 1: Follow the newly-discovered copy chain as far in as possible.
        loop {
            let operand = *stack.last().unwrap();

            match visit_states[operand] {
                VisitState::Unvisited => {
                    visit_states[operand] = VisitState::Visiting;

                    if let Some(src) = copy_sources[operand] {
                        stack.push(src as usize);
                    } else {
                        // No more copies, terminate our current chain.
                        break;
                    }
                }
                VisitState::Visiting => {
                    // We've encountered a copy cycle `r -> ... -> s -> r`; break it up by
                    // introducing a temporary `t`, saving `r` into it before the cyclic copies, and
                    // copying from `t` into `s` (instead of from `r`).

                    // Remove the duplicate `r` from the top of the stack.
                    stack.pop();

                    // We should always have a previous copy destination on the stack.
                    let prev = *stack.last().unwrap();

                    let tmp_op = ctx.get_cycle_break_op();
                    copy_cycle_break = Some((operand, tmp_op));

                    // Insert the final copy out of the temporary here (resolved assignments are in
                    // reverse order).
                    ctx.emit(tmp_op, operands[prev]);

                    break;
                }
                VisitState::Visited => {
                    // We've hit something in a previously-resolved copy chain, so back up out of
                    // it and terminate our current chain.
                    stack.pop();
                    last_copy_src = Some(operand);
                    break;
                }
            }
        }

        // Step 2: Walk back out of the chain and record copies as we go.
        while let Some(operand) = stack.pop() {
            visit_states[operand] = VisitState::Visited;

            if let Some(src) = last_copy_src {
                let from = operands[src];
                let to = operands[operand];
                ctx.emit(from, to);
            }

            match copy_cycle_break {
                Some((cycle_operand, tmp_op)) if cycle_operand == operand => {
                    // We've found the start of a broken parallel copy cycle - make sure the
                    // original value of `operand` is saved before it is overwritten by the copy
                    // inserted above.
                    ctx.emit(operands[operand], tmp_op);
                }
                _ => {}
            }

            last_copy_src = Some(operand);
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
    stack_copy_reg: Option<PhysReg>,
    stack_copy_spill: Option<SpillSlot>,
    cycle_break_reg: Option<PhysReg>,
    cycle_break_spill: Option<SpillSlot>,
    scavenger: &'s mut S,
}

impl<'s, S: RegScavenger> ResolvedCopyContext<'s, S> {
    fn new(scavenger: &'s mut S) -> Self {
        Self {
            copies: SmallVec::new(),
            stack_copy_reg: None,
            stack_copy_spill: None,
            cycle_break_reg: None,
            cycle_break_spill: None,
            scavenger,
        }
    }

    fn get_cycle_break_op(&mut self) -> OperandAssignment {
        let reg = get_tmp_reg(&mut self.cycle_break_reg, self.scavenger);
        match reg {
            Some(reg) => OperandAssignment::Reg(reg),
            None => {
                OperandAssignment::Spill(get_tmp_spill(&mut self.cycle_break_spill, self.scavenger))
            }
        }
    }

    fn emit(&mut self, from: OperandAssignment, to: OperandAssignment) {
        if from.is_reg() || to.is_reg() {
            // Copies involving at least one register can be performed directly.
            self.emit_raw(from, to);
        } else {
            // Stack-to-stack copies need to go through a temporary register.

            let (tmp_reg, emergency_spill) =
                match get_tmp_reg(&mut self.stack_copy_reg, self.scavenger) {
                    Some(tmp_reg) => (tmp_reg, None),
                    None => {
                        let tmp_reg = self.scavenger.emergency_reg();
                        let emergency_spill =
                            get_tmp_spill(&mut self.stack_copy_spill, self.scavenger);

                        // Restore the original value of `tmp_reg` from the emergency spill.
                        self.emit_raw(
                            OperandAssignment::Spill(emergency_spill),
                            OperandAssignment::Reg(tmp_reg),
                        );

                        (tmp_reg, Some(emergency_spill))
                    }
                };

            self.emit_raw(OperandAssignment::Reg(tmp_reg), to);
            self.emit_raw(from, OperandAssignment::Reg(tmp_reg));

            if let Some(emergency_spill) = emergency_spill {
                // If we're using an emergency spill, back up the original value of `tmp_reg` before
                // using it.
                self.emit_raw(
                    OperandAssignment::Reg(tmp_reg),
                    OperandAssignment::Spill(emergency_spill),
                );
            }
        }
    }

    fn emit_raw(&mut self, from: OperandAssignment, to: OperandAssignment) {
        self.copies.push(AssignmentCopy { from, to });
    }
}

fn get_tmp_reg(
    cached_tmp_reg: &mut Option<PhysReg>,
    scavenger: &mut impl RegScavenger,
) -> Option<PhysReg> {
    *cached_tmp_reg = cached_tmp_reg.or_else(|| scavenger.get_fresh_tmp_reg());
    *cached_tmp_reg
}

fn get_tmp_spill(
    cached_tmp_spill: &mut Option<SpillSlot>,
    scavenger: &mut impl RegScavenger,
) -> SpillSlot {
    *cached_tmp_spill.get_or_insert_with(|| scavenger.get_fresh_tmp_spill())
}

#[cfg(test)]
mod tests {
    use std::fmt::Write;

    use cranelift_entity::EntityRef;
    use expect_test::{expect, Expect};

    use crate::{
        lir::{Instr, PhysReg, PhysRegSet, RegClass},
        regalloc::{
            test_utils::{operand_to_string, parse_operand},
            types::ParallelCopyPhase,
            SpillSlot,
        },
    };

    use super::*;

    struct DummyRegScavenger {
        reg_count: u8,
        tmp_spill: u32,
        used_regs: PhysRegSet,
    }

    impl RegScavenger for DummyRegScavenger {
        fn emergency_reg(&self) -> PhysReg {
            PhysReg::new(0)
        }

        fn get_fresh_tmp_reg(&mut self) -> Option<PhysReg> {
            let reg = (0..self.reg_count)
                .map(PhysReg::new)
                .find(|&reg| !self.used_regs.contains(reg))?;
            self.used_regs.insert(reg);
            Some(reg)
        }

        fn get_fresh_tmp_spill(&mut self) -> SpillSlot {
            let spill = SpillSlot::from_u32(self.tmp_spill);
            self.tmp_spill += 1;
            spill
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
                phase: ParallelCopyPhase::Before,
                from: parse_operand(from.trim()),
                to: parse_operand(to.trim()),
            });
        }

        let mut scavenger = DummyRegScavenger {
            reg_count,
            tmp_spill: 1234,
            used_regs: PhysRegSet::empty(),
        };

        for copy in &parallel_copies {
            if let OperandAssignment::Reg(from) = copy.from {
                scavenger.used_regs.insert(from);
            }
            if let OperandAssignment::Reg(to) = copy.to {
                scavenger.used_regs.insert(to);
            }
        }

        let mut resolved = Vec::new();
        resolve(&parallel_copies, &mut scavenger, |copy| {
            resolved.push(*copy)
        });

        let mut output = String::new();
        for copy in &resolved {
            writeln!(
                output,
                "{} = {}",
                operand_to_string(copy.to),
                operand_to_string(copy.from)
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

use smallvec::{smallvec, SmallVec};

use crate::lir::PhysReg;

use super::{
    types::{AssignmentCopy, ParallelCopy},
    OperandAssignment,
};

pub fn resolve(
    parallel_copies: &[ParallelCopy],
    mut get_tmp_reg: impl FnMut() -> PhysReg,
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

    let mut cycle_break_tmp_reg = None;
    let mut stack_copy_reg = None;

    // Resolved copies are held in reverse order during the traversal.
    let mut resolved = SmallVec::<[AssignmentCopy; 8]>::new();

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

                    let tmp_reg = *cycle_break_tmp_reg.get_or_insert_with(&mut get_tmp_reg);
                    copy_cycle_break = Some((operand, tmp_reg));

                    // Insert the final copy out of the temporary here (resolved assignments are in
                    // reverse order).
                    resolved.push(AssignmentCopy {
                        from: OperandAssignment::Reg(tmp_reg),
                        to: operands[prev],
                    });

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

                if from.is_reg() || to.is_reg() {
                    // Copies involving at least one register can be performed directly.
                    resolved.push(AssignmentCopy { from, to });
                } else {
                    // Stack-to-stack copies need to go through a temporary register.
                    let tmp_reg = *stack_copy_reg.get_or_insert_with(&mut get_tmp_reg);
                    resolved.push(AssignmentCopy {
                        from: OperandAssignment::Reg(tmp_reg),
                        to,
                    });
                    resolved.push(AssignmentCopy {
                        from,
                        to: OperandAssignment::Reg(tmp_reg),
                    });
                }
            }

            match copy_cycle_break {
                Some((cycle_operand, tmp_reg)) if cycle_operand == operand => {
                    // We've found the start of a broken parallel copy cycle - make sure the
                    // original value of `operand` is saved before it is overwritten by the copy
                    // inserted above.
                    resolved.push(AssignmentCopy {
                        from: operands[operand],
                        to: OperandAssignment::Reg(tmp_reg),
                    });
                }
                _ => {}
            }

            last_copy_src = Some(operand);
        }
    }

    // To get a proper topological sort of the copies, we need to invert our post-order traversal.
    for copy in resolved.iter().rev() {
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

#[cfg(test)]
mod tests {
    use std::fmt::Write;

    use cranelift_entity::EntityRef;
    use expect_test::{expect, Expect};

    use crate::{
        lir::{Instr, PhysReg, RegClass},
        regalloc::{types::ParallelCopyPhase, SpillSlot},
    };

    use super::*;

    fn parse_operand(s: &str) -> OperandAssignment {
        match s.as_bytes()[0] {
            b'r' => {
                let num: u8 = s[1..].parse().unwrap();
                OperandAssignment::Reg(PhysReg::new(num))
            }
            b's' => {
                let num: u32 = s[1..].parse().unwrap();
                OperandAssignment::Spill(SpillSlot::from_u32(num))
            }
            _ => panic!("invalid operand"),
        }
    }

    fn operand_to_string(operand: OperandAssignment) -> String {
        match operand {
            OperandAssignment::Reg(r) => {
                format!("r{}", r.as_u8())
            }
            OperandAssignment::Spill(s) => {
                format!("s{}", s.as_u32())
            }
        }
    }

    fn check_resolution(input: &str, expected: Expect) {
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

        let mut resolved = Vec::new();
        let mut tmp_reg_num = 64;
        resolve(
            &parallel_copies,
            || {
                let num = tmp_reg_num;
                tmp_reg_num += 1;
                PhysReg::new(num)
            },
            |copy| resolved.push(*copy),
        );

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
                r64 = r0
                r0 = r1
                r1 = r64
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
                r64 = r0
                r0 = r1
                r1 = r64
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
                r64 = r2
                r2 = r3
                r3 = r64
                r64 = r0
                r0 = r1
                r1 = r64
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
                r64 = r0
                r0 = r1
                r1 = r2
                r2 = r3
                r3 = r4
                r4 = r5
                r5 = r64
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
                r64 = r10
                r10 = r11
                r11 = r8
                r8 = r9
                r9 = r64
                r64 = r1
                r1 = r2
                r2 = r3
                r3 = r0
                r0 = r64
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
                r64 = s0
                s1 = r64
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
                r64 = s2
                s3 = r64
                r64 = s0
                s1 = r64
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
                r64 = s2
                s3 = r64
                r64 = s1
                s2 = r64
                r64 = s0
                s1 = r64
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
                r64 = s0
                r65 = s1
                s0 = r65
                r65 = s2
                s1 = r65
                r65 = s3
                s2 = r65
                r65 = s4
                s3 = r65
                r65 = s5
                s4 = r65
                s5 = r64
            "#]],
        )
    }
}

use smallvec::{smallvec, SmallVec};

use super::{
    types::{AssignmentCopy, ParallelCopy},
    OperandAssignment,
};

pub fn resolve(parallel_copies: &[ParallelCopy], mut f: impl FnMut(&AssignmentCopy)) {
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

        // Step 1: Follow the newly-discovered copy chain as far in as possible.
        while let Some(&operand) = stack.last() {
            match visit_states[operand] {
                VisitState::Unvisited => {}
                VisitState::Visiting => {
                    todo!("break parallel copy cycle at {operand}: {stack:?} {visit_states:?}");
                }
                VisitState::Visited => {
                    // We've hit something in a previously-resolved copy chain, so back up out of
                    // it and terminate our current chain.
                    stack.pop();
                    last_copy_src = Some(operand);
                    break;
                }
            }
            visit_states[operand] = VisitState::Visiting;

            if let Some(src) = copy_sources[operand] {
                stack.push(src as usize);
            } else {
                // No more copies, terminate our current chain.
                break;
            }
        }

        // Step 2: Walk back out of the chain and record copies as we go.
        while let Some(operand) = stack.pop() {
            visit_states[operand] = VisitState::Visited;

            if let Some(src) = last_copy_src {
                resolved.push(AssignmentCopy {
                    from: operands[src],
                    to: operands[operand],
                });
            }

            last_copy_src = Some(operand);
        }
    }

    // To get a proper topological sort of the copies, we need to invert our post-order traversal.
    for copy in resolved.iter().rev() {
        f(copy);
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
        lir::{Instr, PhysReg},
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
                phase: ParallelCopyPhase::Before,
                from: parse_operand(from.trim()),
                to: parse_operand(to.trim()),
            });
        }

        let mut resolved = Vec::new();
        resolve(&parallel_copies, |copy| resolved.push(*copy));

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
}

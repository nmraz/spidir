use fx_utils::FxHashMap;
use smallvec::SmallVec;

use super::{types::CopySourceAssignment, AssignmentCopy, OperandAssignment};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RedundantCopyVerdict {
    Necessary,
    Redundant,
}

#[derive(Default)]
pub struct RedundantCopyTracker {
    copy_sources: FxHashMap<OperandAssignment, CopySourceAssignment>,
    copy_targets: FxHashMap<CopySourceAssignment, SmallVec<[OperandAssignment; 4]>>,
}

impl RedundantCopyTracker {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn reset(&mut self) {
        self.copy_sources.clear();
        self.copy_targets.clear();
    }

    pub fn process_copy(&mut self, copy: &AssignmentCopy) -> RedundantCopyVerdict {
        let from = match copy.from {
            CopySourceAssignment::Operand(from) => self.assignment_source(from),
            from => from,
        };
        let to = self.assignment_source(copy.to);

        if from == to {
            RedundantCopyVerdict::Redundant
        } else {
            self.remove_copies_from(copy.to);
            self.copy_sources.insert(copy.to, from);
            self.copy_targets.entry(from).or_default().push(copy.to);
            RedundantCopyVerdict::Necessary
        }
    }

    pub fn assignment_clobbered(&mut self, assignment: OperandAssignment) {
        self.remove_copies_from(assignment);
        self.copy_sources.remove(&assignment);
    }

    fn remove_copies_from(&mut self, assignment: OperandAssignment) {
        if let Some(targets) = self
            .copy_targets
            .remove(&CopySourceAssignment::Operand(assignment))
        {
            for target in targets {
                self.copy_sources.remove(&target);
            }
        }
    }

    fn assignment_source(&self, assignment: OperandAssignment) -> CopySourceAssignment {
        self.copy_sources
            .get(&assignment)
            .copied()
            .unwrap_or(assignment.into())
    }
}

#[cfg(test)]
mod tests {
    use core::fmt::Write;

    use expect_test::{expect, Expect};

    use crate::regalloc::test_utils::{parse_copy_source, parse_operand};

    use super::*;

    fn check_copy_elim(input: &str, expected: Expect) {
        let mut tracker = RedundantCopyTracker::new();
        let mut result = String::new();

        for line in input.lines() {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }

            if line == "R" {
                tracker.reset();
                writeln!(result, "{line}").unwrap();
            } else if let Some(clobbers) = line.strip_prefix("C") {
                let clobbers = clobbers
                    .split(' ')
                    .filter(|s| !s.is_empty())
                    .map(parse_operand);
                for clobber in clobbers {
                    tracker.assignment_clobbered(clobber);
                }
                writeln!(result, "{line}").unwrap();
            } else {
                let mut parts = line.split('=');
                let to = parse_operand(parts.next().unwrap().trim());
                let from = parse_copy_source(parts.next().unwrap().trim());

                if tracker.process_copy(&AssignmentCopy { from, to })
                    == RedundantCopyVerdict::Necessary
                {
                    writeln!(result, "{line}").unwrap();
                }
            }
        }

        expected.assert_eq(&result);
    }

    #[test]
    fn single_copy() {
        check_copy_elim(
            "
            r1 = r0
            ",
            expect![[r#"
                r1 = r0
            "#]],
        );
    }

    #[test]
    fn no_redundant_copies() {
        check_copy_elim(
            "
            r7 = r3
            r1 = r0
            r2 = r1
            ",
            expect![[r#"
                r7 = r3
                r1 = r0
                r2 = r1
            "#]],
        );
    }

    #[test]
    fn copy_to_self() {
        check_copy_elim(
            "
            r0 = r0
            ",
            expect![""],
        );
    }

    #[test]
    fn copy_again() {
        check_copy_elim(
            "
            r1 = r0
            r1 = r0
            ",
            expect![[r#"
                r1 = r0
            "#]],
        );
    }

    #[test]
    fn copy_again_after_reset() {
        check_copy_elim(
            "
            r1 = r0
            R
            r1 = r0
            ",
            expect![[r#"
                r1 = r0
                R
                r1 = r0
            "#]],
        );
    }

    #[test]
    fn copy_again_after_unrelated_clobber() {
        check_copy_elim(
            "
            r1 = r0
            C r2
            r1 = r0
            ",
            expect![[r#"
                r1 = r0
                C r2
            "#]],
        );
    }

    #[test]
    fn copy_again_after_clobber_from() {
        check_copy_elim(
            "
            r1 = r0
            C r0
            r1 = r0
            ",
            expect![[r#"
                r1 = r0
                C r0
                r1 = r0
            "#]],
        );
    }

    #[test]
    fn copy_again_after_copy_clobber_from() {
        check_copy_elim(
            "
            r1 = r0
            r0 = r2
            r1 = r0
            ",
            expect![[r#"
                r1 = r0
                r0 = r2
                r1 = r0
            "#]],
        );
    }

    #[test]
    fn copy_again_after_clobber_to() {
        check_copy_elim(
            "
            r1 = r0
            C r1
            r1 = r0
            ",
            expect![[r#"
                r1 = r0
                C r1
                r1 = r0
            "#]],
        );
    }

    #[test]
    fn copy_again_after_copy_clobber_to() {
        check_copy_elim(
            "
            r1 = r0
            r1 = r2
            r1 = r0
            ",
            expect![[r#"
                r1 = r0
                r1 = r2
                r1 = r0
            "#]],
        );
    }

    #[test]
    fn copy_back() {
        check_copy_elim(
            "
            r1 = r0
            r0 = r1
            ",
            expect![[r#"
                r1 = r0
            "#]],
        );
    }

    #[test]
    fn copy_back_after_reset() {
        check_copy_elim(
            "
            r1 = r0
            R
            r0 = r1
            ",
            expect![[r#"
                r1 = r0
                R
                r0 = r1
            "#]],
        );
    }

    #[test]
    fn copy_back_after_unrelated_clobber() {
        check_copy_elim(
            "
            r1 = r0
            C r2
            r0 = r1
            ",
            expect![[r#"
                r1 = r0
                C r2
            "#]],
        );
    }

    #[test]
    fn copy_back_after_clobber_from() {
        check_copy_elim(
            "
            r1 = r0
            C r0
            r0 = r1
            ",
            expect![[r#"
                r1 = r0
                C r0
                r0 = r1
            "#]],
        );
    }

    #[test]
    fn copy_back_after_clobber_to() {
        check_copy_elim(
            "
            r1 = r0
            C r1
            r0 = r1
            ",
            expect![[r#"
                r1 = r0
                C r1
                r0 = r1
            "#]],
        );
    }

    #[test]
    fn copy_again_chain() {
        check_copy_elim(
            "
            r1 = r0
            r2 = r1
            r3 = r2
            r1 = r3
            ",
            expect![[r#"
                r1 = r0
                r2 = r1
                r3 = r2
            "#]],
        );
    }

    #[test]
    fn copy_back_chain() {
        check_copy_elim(
            "
            r1 = r0
            r2 = r1
            r0 = r2
            ",
            expect![[r#"
                r1 = r0
                r2 = r1
            "#]],
        );
    }

    #[test]
    fn copy_back_chain_after_clobber_tmp() {
        check_copy_elim(
            "
            r1 = r0
            r2 = r1
            C r1
            r0 = r2
            ",
            expect![[r#"
                r1 = r0
                r2 = r1
                C r1
            "#]],
        );
    }

    #[test]
    fn repeated_call_pattern() {
        check_copy_elim(
            "
            r12 = r0
            r13 = r1
            r0 = r12
            r1 = r13
            C r0 r1
            r0 = r12
            r1 = r13
            C r0 r0
            ",
            expect![[r#"
                r12 = r0
                r13 = r1
                C r0 r1
                r0 = r12
                r1 = r13
                C r0 r0
            "#]],
        );
    }

    #[test]
    fn new_copy_removes_old_targets() {
        check_copy_elim(
            "
            r2 = r1
            r1 = r0
            C r0
            r2 = r1
            ",
            expect![[r#"
                r2 = r1
                r1 = r0
                C r0
                r2 = r1
            "#]],
        );
    }
}

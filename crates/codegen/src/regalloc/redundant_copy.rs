use super::{AssignmentCopy, OperandAssignment};

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RedundantCopyVerdict {
    Necessary,
    Redundant,
}

#[derive(Default)]
pub struct RedundantCopyTracker {}

impl RedundantCopyTracker {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn reset(&mut self) {}

    pub fn process_copy(&mut self, copy: &AssignmentCopy) -> RedundantCopyVerdict {
        let _ = copy;
        RedundantCopyVerdict::Necessary
    }

    pub fn assignment_clobbered(&mut self, assignment: OperandAssignment) {
        let _ = assignment;
    }
}

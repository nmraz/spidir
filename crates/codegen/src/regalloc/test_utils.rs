use crate::lir::{Instr, PhysReg};

use super::{types::AssignmentCopySource, OperandAssignment, SpillSlot};

pub fn parse_operand(s: &str) -> OperandAssignment {
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

pub fn parse_copy_source(s: &str) -> AssignmentCopySource {
    match s.as_bytes()[0] {
        b'i' => {
            let num: u32 = s[1..].parse().unwrap();
            AssignmentCopySource::Remat(Instr::from_u32(num))
        }
        _ => AssignmentCopySource::Operand(parse_operand(s)),
    }
}

pub fn operand_to_string(operand: OperandAssignment) -> String {
    match operand {
        OperandAssignment::Reg(r) => {
            format!("r{}", r.as_u8())
        }
        OperandAssignment::Spill(s) => {
            format!("s{}", s.as_u32())
        }
    }
}

pub fn copy_source_to_string(source: AssignmentCopySource) -> String {
    match source {
        AssignmentCopySource::Operand(operand) => operand_to_string(operand),
        AssignmentCopySource::Remat(instr) => instr.to_string(),
    }
}

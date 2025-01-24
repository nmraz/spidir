use crate::lir::{Instr, PhysReg};

use super::{types::CopySourceAssignment, OperandAssignment, SpillSlot};

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

pub fn parse_copy_source(s: &str) -> CopySourceAssignment {
    match s.as_bytes()[0] {
        b'i' => {
            let num: u32 = s[1..].parse().unwrap();
            CopySourceAssignment::Remat(Instr::from_u32(num))
        }
        _ => parse_operand(s).into(),
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

pub fn copy_source_to_string(source: CopySourceAssignment) -> String {
    match source {
        CopySourceAssignment::Operand(operand) => operand_to_string(operand),
        CopySourceAssignment::Remat(instr) => instr.to_string(),
    }
}

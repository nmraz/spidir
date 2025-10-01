use crate::lir::{Instr, PhysReg, RegWidth};

use super::{OperandAssignment, SpillSlot, types::CopySourceAssignment};

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

pub fn parse_copy_dest(s: &str) -> (OperandAssignment, RegWidth) {
    let mut parts = s.split(':');
    let operand = parse_operand(parts.next().unwrap());

    let width_val = parts.next().map_or(0, |width_str| {
        width_str.strip_prefix("w").unwrap().parse().unwrap()
    });

    (operand, RegWidth::new(width_val))
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

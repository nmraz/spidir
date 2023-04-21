use cranelift_entity::EntityList;
use smallvec::SmallVec;

use crate::entities::{Instruction, Operand, Region, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    I32,
    I64,
    F64,
    Ptr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueData {
    pub ty: Type,
    pub uses: SmallVec<[Instruction; 2]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    StartRegion,
    IConst,
    FConst,
    Iadd,
    Load,
    Store,
    Br,
    BrCond,
    Call,
    Return,
}

impl Opcode {
    pub fn may_need_ctrl_dep(&self) -> bool {
        match self {
            Opcode::StartRegion | Opcode::IConst | Opcode::FConst | Opcode::Iadd => false,
            Opcode::Load
            | Opcode::Store
            | Opcode::Br
            | Opcode::BrCond
            | Opcode::Call
            | Opcode::Return => true,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum OperandData {
    Value(Value),
    Iconst(i64),
    Fconst(f64),
    RegionCall {
        region: Region,
        args: EntityList<Value>,
    },
}

pub type InstructionList = EntityList<Instruction>;
pub type OperandList = EntityList<Operand>;
pub type ValueList = EntityList<Value>;

#[derive(Clone, Copy)]
pub struct InstructionData {
    pub opcode: Opcode,
    pub ctrl_deps: InstructionList,
    pub inputs: OperandList,
    pub outputs: ValueList,
}

#[cfg(test)]
mod tests {
    use core::mem;

    use super::*;

    #[test]
    fn instruction_data_size() {
        assert_eq!(mem::size_of::<InstructionData>(), 16);
    }

    #[test]
    fn operand_size() {
        assert_eq!(mem::size_of::<OperandData>(), 16);
    }
}

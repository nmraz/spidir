use cranelift_entity::{EntityList, ListPool, PrimaryMap};

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

pub type InstructionPool = ListPool<Instruction>;
pub type OperandPool = ListPool<Operand>;
pub type ValuePool = ListPool<Value>;

pub type InstructionList = EntityList<Instruction>;
pub type OperandList = EntityList<Operand>;
pub type ValueList = EntityList<Value>;

#[derive(Clone, Copy)]
pub struct InstructionData {
    pub opcode: Opcode,
    pub inputs: OperandList,
    // These fields have more global invariants, so we don't allow direct access
    pub(crate) outputs: ValueList,
    pub(crate) ctrl_deps: InstructionList,
}

impl InstructionData {
    pub fn ctrl_deps<'a>(&self, pool: &'a InstructionPool) -> &'a [Instruction] {
        self.ctrl_deps.as_slice(pool)
    }

    pub fn ctrl_deps_raw(&self) -> InstructionList {
        self.ctrl_deps
    }

    pub fn input_data<'a>(
        &self,
        pool: &'a OperandPool,
        map: &'a PrimaryMap<Operand, OperandData>,
    ) -> impl Iterator<Item = &'a OperandData> {
        self.inputs.as_slice(pool).iter().map(|&op| &map[op])
    }

    pub fn inputs<'a>(&self, pool: &'a OperandPool) -> &'a [Operand] {
        self.inputs.as_slice(pool)
    }

    pub fn outputs<'a>(&self, pool: &'a ValuePool) -> &'a [Value] {
        self.outputs.as_slice(pool)
    }

    pub fn outputs_raw(&self) -> ValueList {
        self.outputs
    }
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

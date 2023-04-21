use cranelift_entity::EntityList;

use crate::entities::{BasicBlock, Operand, Value};

pub enum Opcode {
    StartBlock,
    Iadd,
    Load,
    Store,
    Call,
    Return,
}

pub enum OperandData {
    Value(Value),
    Iconst(i64),
    Fconst(f64),
    BlockCall {
        block: BasicBlock,
        args: EntityList<Value>,
    },
}

pub struct InstructionData {
    pub opcode: Opcode,
    pub outputs: EntityList<Value>,
    pub inputs: EntityList<Operand>,
}

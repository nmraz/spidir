use cranelift_entity::entity_impl;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Instruction(u32);
entity_impl!(Instruction, "inst");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Operand(u32);
entity_impl!(Operand, "op");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct BasicBlock(u32);
entity_impl!(BasicBlock, "bb");

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Value(u32);
entity_impl!(Value, "val");

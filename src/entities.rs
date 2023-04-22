use cranelift_entity::entity_impl;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Instruction(u32);
entity_impl!(Instruction, "inst");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Operand(u32);
entity_impl!(Operand, "op");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Region(u32);
entity_impl!(Region, "r");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(u32);
entity_impl!(Value, "val");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueUse(u32);
entity_impl!(ValueUse, "use");

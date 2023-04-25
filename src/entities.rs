use cranelift_entity::entity_impl;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Node(u32);
entity_impl!(Node, "node");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DepValue(u32);
entity_impl!(DepValue, "gv");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Use(u32);
entity_impl!(Use, "use");

use core::{
    fmt,
    hash::{Hash, Hasher},
};

use crate::module::{ExternFunction, Function};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    I64,
    F64,
    Ptr,
}

impl Type {
    pub fn as_str(self) -> &'static str {
        match self {
            Type::I32 => "i32",
            Type::I64 => "i64",
            Type::F64 => "f64",
            Type::Ptr => "ptr",
        }
    }

    pub fn is_integer(self) -> bool {
        matches!(self, Type::I32 | Type::I64)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IcmpKind {
    Eq,
    Ne,
    Slt,
    Sle,
    Ult,
    Ule,
}

impl IcmpKind {
    pub fn as_str(self) -> &'static str {
        match self {
            IcmpKind::Eq => "eq",
            IcmpKind::Ne => "ne",
            IcmpKind::Slt => "slt",
            IcmpKind::Sle => "sle",
            IcmpKind::Ult => "ult",
            IcmpKind::Ule => "ule",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MemSize {
    S1,
    S2,
    S4,
    S8,
}

impl MemSize {
    pub fn as_str(self) -> &'static str {
        match self {
            MemSize::S1 => "1",
            MemSize::S2 => "2",
            MemSize::S4 => "4",
            MemSize::S8 => "8",
        }
    }
}

impl fmt::Display for IcmpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionRef {
    Internal(Function),
    External(ExternFunction),
}

#[derive(Debug, Clone, Copy)]
pub struct BitwiseF64(pub f64);

impl PartialEq for BitwiseF64 {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}

impl Eq for BitwiseF64 {}

impl Hash for BitwiseF64 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state)
    }
}

impl fmt::Display for BitwiseF64 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeKind {
    Entry,
    Return,
    Region,
    Phi,
    IConst(u64),
    Iadd,
    Isub,
    And,
    Or,
    Xor,
    Shl,
    Lshr,
    Ashr,
    Imul,
    Sdiv,
    Udiv,
    Iext,
    Itrunc,
    Sfill(u8),
    Icmp(IcmpKind),
    FConst(BitwiseF64),
    PtrOff,
    Load(MemSize),
    Store(MemSize),
    StackSlot { size: u32, align: u32 },
    BrCond,
    Call(FunctionRef),
}

impl NodeKind {
    pub fn has_identity(&self) -> bool {
        // Distinct stack slot nodes must always return distinct stack locations - the identity of
        // the stack location is tied to the node itself.
        matches!(self, Self::StackSlot { .. })
    }

    pub fn has_control_flow(&self) -> bool {
        matches!(
            self,
            Self::Entry
                | Self::Return
                | Self::Region
                | Self::Sdiv
                | Self::Udiv
                | Self::Load(..)
                | Self::Store(..)
                | Self::BrCond
                | Self::Call(..)
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DepValueKind {
    /// An "ordinary" value representing a computation.
    Value(Type),
    /// Indicates a control flow dependency between nodes. Every region takes in a number of control
    /// values indicating the predecessors of the region, while every branch produces a number of
    /// control values that are then consumed by the regions to which they branch.
    Control,
    /// Special value produced only by region instructions to attach their phi nodes.
    PhiSelector,
}

impl DepValueKind {
    pub fn as_value(self) -> Option<Type> {
        match self {
            Self::Value(v) => Some(v),
            _ => None,
        }
    }
}

impl fmt::Display for DepValueKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DepValueKind::Value(ty) => write!(f, "{ty}"),
            DepValueKind::Control => f.write_str("ctrl"),
            DepValueKind::PhiSelector => f.write_str("phisel"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_is_integer() {
        assert!(Type::I32.is_integer());
        assert!(Type::I64.is_integer());
        assert!(!Type::F64.is_integer());
        assert!(!Type::Ptr.is_integer());
    }

    #[test]
    fn display_value_kind() {
        assert_eq!(DepValueKind::Value(Type::I32).to_string(), "i32");
        assert_eq!(DepValueKind::Value(Type::I64).to_string(), "i64");
        assert_eq!(DepValueKind::Value(Type::F64).to_string(), "f64");
        assert_eq!(DepValueKind::Value(Type::Ptr).to_string(), "ptr");
        assert_eq!(DepValueKind::Control.to_string(), "ctrl");
        assert_eq!(DepValueKind::PhiSelector.to_string(), "phisel");
    }
}

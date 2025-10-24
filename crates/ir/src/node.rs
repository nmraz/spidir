use core::{
    fmt,
    hash::{Hash, Hasher},
};

use hexfloat2::HexFloat;

use crate::{
    function::SignatureRef,
    module::{ExternFunction, Function},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    Ptr,
}

impl Type {
    #[inline]
    pub fn as_str(self) -> &'static str {
        match self {
            Type::I32 => "i32",
            Type::I64 => "i64",
            Type::F32 => "f32",
            Type::F64 => "f64",
            Type::Ptr => "ptr",
        }
    }

    #[inline]
    pub fn is_integer(self) -> bool {
        matches!(self, Type::I32 | Type::I64)
    }

    #[inline]
    pub fn is_integer_or_pointer(self) -> bool {
        self.is_integer() || matches!(self, Type::Ptr)
    }

    #[inline]
    pub fn byte_size(self) -> usize {
        match self {
            Type::I32 | Type::F32 => 4,
            Type::I64 | Type::F64 | Type::Ptr => 8,
        }
    }

    #[inline]
    pub fn bit_width(self) -> usize {
        self.byte_size() * 8
    }

    #[inline]
    pub fn extend_s32(self, val: i32) -> u64 {
        if self == Self::I32 {
            // Keep the value as 32 bits.
            val as u32 as u64
        } else {
            // Sign-extend to 64 bits.
            val as u64
        }
    }

    #[inline]
    pub fn all_ones_val(self) -> u64 {
        self.extend_s32(-1)
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
    #[inline]
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

    #[inline]
    pub fn is_commutative(self) -> bool {
        matches!(self, Self::Eq | Self::Ne)
    }
}

impl fmt::Display for IcmpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FcmpKind {
    Oeq,
    One,
    Olt,
    Ole,
    Ueq,
    Une,
    Ult,
    Ule,
}

impl FcmpKind {
    #[inline]
    pub fn as_str(self) -> &'static str {
        match self {
            FcmpKind::Oeq => "oeq",
            FcmpKind::One => "one",
            FcmpKind::Olt => "olt",
            FcmpKind::Ole => "ole",
            FcmpKind::Ueq => "ueq",
            FcmpKind::Une => "une",
            FcmpKind::Ult => "ult",
            FcmpKind::Ule => "ule",
        }
    }
}

impl fmt::Display for FcmpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
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
    #[inline]
    pub fn as_str(self) -> &'static str {
        match self {
            MemSize::S1 => "1",
            MemSize::S2 => "2",
            MemSize::S4 => "4",
            MemSize::S8 => "8",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionRef {
    Internal(Function),
    External(ExternFunction),
}

macro_rules! bitwise_float {
    ($name:ident, $float_ty:ident, $bit_ty:ident) => {
        #[derive(Debug, Clone, Copy)]
        pub struct $name(pub $float_ty);

        impl $name {
            pub fn bits(self) -> $bit_ty {
                self.0.to_bits()
            }
        }

        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                self.bits() == other.bits()
            }
        }

        impl Eq for $name {}

        impl Hash for $name {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.bits().hash(state)
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", HexFloat(self.0))
            }
        }
    };
}

bitwise_float! { BitwiseF32, f32, u32 }
bitwise_float! { BitwiseF64, f64, u64 }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeKind {
    Entry,
    Return,
    Region,
    Unreachable,
    Phi,
    Iconst(u64),
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
    Srem,
    Urem,
    Iext,
    Itrunc,
    Sfill(u8),
    Icmp(IcmpKind),
    Fconst32(BitwiseF32),
    Fconst64(BitwiseF64),
    Fadd,
    Fsub,
    Fmul,
    Fdiv,
    Fwiden,
    Fnarrow,
    Fcmp(FcmpKind),
    SintToFloat,
    UintToFloat,
    FloatToSint,
    FloatToUint,
    PtrOff,
    IntToPtr,
    PtrToInt,
    Load(MemSize),
    Store(MemSize),
    StackSlot { size: u32, align: u32 },
    BrCond,
    FuncAddr(FunctionRef),
    Call(FunctionRef),
    CallInd(SignatureRef),
}

impl NodeKind {
    #[inline]
    pub fn has_identity(&self) -> bool {
        // Distinct stack slot nodes must always return distinct stack locations - the identity of
        // the stack location is tied to the node itself.
        matches!(self, Self::StackSlot { .. })
    }

    #[inline]
    pub fn has_control_flow(&self) -> bool {
        matches!(
            self,
            Self::Entry
                | Self::Return
                | Self::Region
                | Self::Unreachable
                | Self::Sdiv
                | Self::Udiv
                | Self::Srem
                | Self::Urem
                | Self::Load(..)
                | Self::Store(..)
                | Self::BrCond
                | Self::Call(..)
                | Self::CallInd(..)
        )
    }

    #[inline]
    pub fn has_side_effects(&self) -> bool {
        // For now
        self.has_control_flow()
    }

    #[inline]
    pub fn is_terminator(&self) -> bool {
        matches!(self, Self::Return | Self::Unreachable | Self::BrCond)
    }

    #[inline]
    pub fn is_cacheable(&self) -> bool {
        // Never hash-cons nodes that actually need to be distinct (such as stack slots).
        !self.has_identity() &&
        // Also avoid storing nodes that are control-dependent, since it will never be possible to
        // combine them anyway. This is an optimization and is not strictly necessary for
        // correctness.
        !self.has_control_flow()
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
    #[inline]
    pub fn is_value(self) -> bool {
        matches!(self, Self::Value(..))
    }

    #[inline]
    pub fn as_value(self) -> Option<Type> {
        match self {
            Self::Value(v) => Some(v),
            _ => None,
        }
    }

    #[inline]
    pub fn is_control(self) -> bool {
        self == Self::Control
    }

    #[inline]
    pub fn is_phisel(self) -> bool {
        self == Self::PhiSelector
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

use core::ops::{BitAnd, BitOr, BitXor};

use ir::node::{IcmpKind, Type};

macro_rules! fold_binop {
    ($ty:expr, $lhs:expr, $rhs:expr, $func:ident) => {
        if $ty == Type::I32 {
            (($lhs as u32).$func($rhs as u32)) as u64
        } else {
            $lhs.$func($rhs)
        }
    };
}

pub fn fold_iadd(ty: Type, lhs: u64, rhs: u64) -> u64 {
    fold_binop!(ty, lhs, rhs, wrapping_add)
}

pub fn fold_isub(ty: Type, lhs: u64, rhs: u64) -> u64 {
    fold_binop!(ty, lhs, rhs, wrapping_sub)
}

pub fn fold_and(ty: Type, lhs: u64, rhs: u64) -> u64 {
    fold_binop!(ty, lhs, rhs, bitand)
}

pub fn fold_or(ty: Type, lhs: u64, rhs: u64) -> u64 {
    fold_binop!(ty, lhs, rhs, bitor)
}

pub fn fold_xor(ty: Type, lhs: u64, rhs: u64) -> u64 {
    fold_binop!(ty, lhs, rhs, bitxor)
}

pub fn fold_shl(arg_ty: Type, arg: u64, amt: u64) -> u64 {
    let amt = amt as u32;
    // Shifting past the bit width produces an unspecified value anyway, so it's
    // easiest to just mask here.
    fold_binop!(arg_ty, arg, amt, wrapping_shl)
}

pub fn fold_lshr(arg_ty: Type, arg: u64, amt: u64) -> u64 {
    let amt = amt as u32;
    // Shifting past the bit width produces an unspecified value anyway, so it's
    // easiest to just mask here.
    fold_binop!(arg_ty, arg, amt, wrapping_shr)
}

pub fn fold_ashr(arg_ty: Type, arg: u64, amt: u64) -> u64 {
    let amt = amt as u32;

    // Shifting past the bit width produces an unspecified value anyway, so it's
    // easiest to just mask here.

    // Note: make sure to shift signed values so we end up with an arithmetic shift.
    if arg_ty == Type::I32 {
        (arg as i32).wrapping_shr(amt) as u32 as u64
    } else {
        (arg as i64).wrapping_shr(amt) as u64
    }
}

pub fn fold_imul(ty: Type, lhs: u64, rhs: u64) -> u64 {
    fold_binop!(ty, lhs, rhs, wrapping_mul)
}

pub fn fold_sdiv(ty: Type, lhs: u64, rhs: u64) -> Option<u64> {
    if ty == Type::I32 {
        (lhs as i32)
            .checked_div(rhs as i32)
            .map(|quotient| quotient as u32 as u64)
    } else {
        (lhs as i64)
            .checked_div(rhs as i64)
            .map(|quotient| quotient as u64)
    }
}

pub fn fold_udiv(ty: Type, lhs: u64, rhs: u64) -> Option<u64> {
    if ty == Type::I32 {
        (lhs as u32)
            .checked_div(rhs as u32)
            .map(|quotient| quotient as u64)
    } else {
        lhs.checked_div(rhs)
    }
}

pub fn fold_srem(ty: Type, lhs: u64, rhs: u64) -> Option<u64> {
    if ty == Type::I32 {
        (lhs as i32)
            .checked_rem(rhs as i32)
            .map(|rem| rem as u32 as u64)
    } else {
        (lhs as i64).checked_rem(rhs as i64).map(|rem| rem as u64)
    }
}

pub fn fold_urem(ty: Type, lhs: u64, rhs: u64) -> Option<u64> {
    if ty == Type::I32 {
        (lhs as u32).checked_rem(rhs as u32).map(|rem| rem as u64)
    } else {
        lhs.checked_rem(rhs)
    }
}

pub fn fold_iext(val: u64) -> u64 {
    val
}

pub fn fold_itrunc(val: u64) -> u64 {
    val as u32 as u64
}

pub fn fold_sfill(ty: Type, val: u64, width: u8) -> u64 {
    let shift_width = 64 - width;

    let mut new_value = (((val << shift_width) as i64) >> shift_width) as u64;
    if ty == Type::I32 {
        new_value = new_value as u32 as u64;
    }

    new_value
}

pub fn fold_icmp(arg_ty: Type, kind: IcmpKind, lhs: u64, rhs: u64) -> u64 {
    macro_rules! cmp_signed {
        ($ty:expr, $a:expr, $b:expr, $op:tt) => {
            if $ty == Type::I32 {
                ($a as i32) $op ($b as i32)
            } else {
                ($a as i64) $op ($b as i64)
            }
        };
    }

    let res = match kind {
        IcmpKind::Eq => lhs == rhs,
        IcmpKind::Ne => lhs != rhs,
        IcmpKind::Slt => cmp_signed!(arg_ty, lhs, rhs, <),
        IcmpKind::Sle => cmp_signed!(arg_ty, lhs, rhs, <=),
        // Note: these compares always behave the same for 32-bit and 64-bit values because
        // 32-bit constants aren't supposed to have their high bits set.
        IcmpKind::Ult => lhs < rhs,
        IcmpKind::Ule => lhs <= rhs,
    };

    res as u64
}

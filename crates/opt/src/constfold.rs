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

#[cfg(test)]
mod tests {
    use ir::node::Type;

    use super::*;

    fn is_u32(val: u64) -> bool {
        u32::try_from(val).is_ok()
    }

    #[test]
    fn iadd() {
        assert_eq!(fold_iadd(Type::I32, 1, 2), 3);
        assert_eq!(fold_iadd(Type::I32, 4294967295, 2), 1);
        assert_eq!(fold_iadd(Type::I64, 1, 2), 3);
        assert_eq!(fold_iadd(Type::I64, 4294967295, 2), 4294967297);
        assert_eq!(fold_iadd(Type::I64, 18446744073709551615, 2), 1);
    }

    #[test]
    fn isub() {
        assert_eq!(fold_isub(Type::I32, 3, 2), 1);
        assert_eq!(fold_isub(Type::I32, 1, 2), 4294967295);
        assert_eq!(fold_isub(Type::I64, 3, 2), 1);
        assert_eq!(fold_isub(Type::I64, 4294967297, 2), 4294967295);
        assert_eq!(fold_isub(Type::I64, 1, 2), 18446744073709551615);
    }

    #[test]
    fn and() {
        assert_eq!(fold_and(Type::I32, 3, 2), 2);
        assert_eq!(fold_and(Type::I32, 3, 4294967295), 3);
        assert_eq!(fold_and(Type::I32, 75, 38), 2);
        assert_eq!(fold_and(Type::I32, 25, 32), 0);
        assert_eq!(fold_and(Type::I64, 3, 2), 2);
        assert_eq!(fold_and(Type::I64, 3, 4294967295), 3);
        assert_eq!(fold_and(Type::I64, 75, 38), 2);
        assert_eq!(fold_and(Type::I64, 25, 32), 0);
    }

    #[test]
    fn or() {
        assert_eq!(fold_or(Type::I32, 3, 2), 3);
        assert_eq!(fold_or(Type::I32, 3, 4294967295), 4294967295);
        assert_eq!(fold_or(Type::I32, 75, 38), 111);
        assert_eq!(fold_or(Type::I32, 25, 32), 57);
        assert_eq!(fold_or(Type::I64, 3, 2), 3);
        assert_eq!(fold_or(Type::I64, 3, 4294967295), 4294967295);
        assert_eq!(fold_or(Type::I64, 75, 38), 111);
        assert_eq!(fold_or(Type::I64, 25, 32), 57);
    }

    #[test]
    fn xor() {
        assert_eq!(fold_xor(Type::I32, 3, 2), 1);
        assert_eq!(fold_xor(Type::I32, 3, 4294967295), 4294967292);
        assert_eq!(fold_xor(Type::I32, 75, 38), 109);
        assert_eq!(fold_xor(Type::I32, 25, 32), 57);
        assert_eq!(fold_xor(Type::I64, 3, 2), 1);
        assert_eq!(fold_xor(Type::I64, 3, 4294967295), 4294967292);
        assert_eq!(fold_xor(Type::I64, 75, 38), 109);
        assert_eq!(fold_xor(Type::I64, 25, 32), 57);
    }

    #[test]
    fn shl() {
        assert_eq!(fold_shl(Type::I32, 5, 1), 10);
        assert_eq!(fold_shl(Type::I32, 5, 2), 20);
        assert_eq!(fold_shl(Type::I32, 5, 3), 40);
        assert_eq!(fold_shl(Type::I32, 7, 20), 7340032);
        assert_eq!(fold_shl(Type::I32, 1, 31), 2147483648);

        // The output here is unspecified, but whatever constant we choose must fit in 32 bits to
        // avoid generating invalid IR during folding.
        assert!(is_u32(fold_shl(Type::I32, 1, 32)));
        assert!(is_u32(fold_shl(Type::I32, 7, 35)));
        assert!(is_u32(fold_shl(Type::I32, 7, 67)));

        assert_eq!(fold_shl(Type::I64, 5, 1), 10);
        assert_eq!(fold_shl(Type::I64, 5, 2), 20);
        assert_eq!(fold_shl(Type::I64, 5, 3), 40);
        assert_eq!(fold_shl(Type::I64, 7, 20), 7340032);
        assert_eq!(fold_shl(Type::I64, 1, 31), 2147483648);
        assert_eq!(fold_shl(Type::I64, 1, 32), 4294967296);
        assert_eq!(fold_shl(Type::I64, 7, 35), 240518168576);
        assert_eq!(fold_shl(Type::I64, 1, 63), 9223372036854775808);
    }

    #[test]
    fn lshr() {
        assert_eq!(fold_lshr(Type::I32, 5, 1), 2);
        assert_eq!(fold_lshr(Type::I32, 5, 2), 1);
        assert_eq!(fold_lshr(Type::I32, 5, 3), 0);
        assert_eq!(fold_lshr(Type::I32, 7, 20), 0);
        assert_eq!(fold_lshr(Type::I32, 2147483648, 31), 1);
        assert_eq!(fold_lshr(Type::I32, 4294967254, 5), 134217726);

        // The output here is unspecified, but whatever constant we choose must fit in 32 bits to
        // avoid generating invalid IR during folding.
        assert!(is_u32(fold_lshr(Type::I32, 1, 32)));
        assert!(is_u32(fold_lshr(Type::I32, 7, 35)));
        assert!(is_u32(fold_lshr(Type::I32, 7, 67)));

        assert_eq!(fold_lshr(Type::I64, 5, 1), 2);
        assert_eq!(fold_lshr(Type::I64, 5, 2), 1);
        assert_eq!(fold_lshr(Type::I64, 5, 3), 0);
        assert_eq!(fold_lshr(Type::I64, 7, 20), 0);
        assert_eq!(fold_lshr(Type::I64, 2147483648, 31), 1);
        assert_eq!(fold_lshr(Type::I64, 4294967254, 5), 134217726);
        assert_eq!(fold_lshr(Type::I64, 9223372036854775808, 63), 1);
        assert_eq!(
            fold_lshr(Type::I64, 18446744073709551574, 5),
            576460752303423486
        );
    }

    #[test]
    fn ashr() {
        assert_eq!(fold_ashr(Type::I32, 5, 1), 2);
        assert_eq!(fold_ashr(Type::I32, 5, 2), 1);
        assert_eq!(fold_ashr(Type::I32, 5, 3), 0);
        assert_eq!(fold_ashr(Type::I32, 7, 20), 0);

        // Arithmetic shifts always round toward negative infinity, rather than toward 0.
        assert_eq!(fold_ashr(Type::I32, 2147483648, 31), 4294967295);
        assert_eq!(fold_ashr(Type::I32, 4294967254, 5), 4294967294);

        // The output here is unspecified, but whatever constant we choose must fit in 32 bits to
        // avoid generating invalid IR during folding.
        assert!(is_u32(fold_ashr(Type::I32, 1, 32)));
        assert!(is_u32(fold_ashr(Type::I32, 7, 35)));
        assert!(is_u32(fold_ashr(Type::I32, 7, 67)));

        assert_eq!(fold_ashr(Type::I64, 5, 1), 2);
        assert_eq!(fold_ashr(Type::I64, 5, 2), 1);
        assert_eq!(fold_ashr(Type::I64, 5, 3), 0);
        assert_eq!(fold_ashr(Type::I64, 7, 20), 0);
        assert_eq!(fold_ashr(Type::I64, 2147483648, 31), 1);
        assert_eq!(fold_ashr(Type::I64, 4294967254, 5), 134217726);

        // Arithmetic shifts always round toward negative infinity, rather than toward 0.
        assert_eq!(
            fold_ashr(Type::I64, 9223372036854775808, 63),
            18446744073709551615
        );
        assert_eq!(
            fold_ashr(Type::I64, 18446744073709551574, 5),
            18446744073709551614
        );
    }

    #[test]
    fn imul() {
        assert_eq!(fold_imul(Type::I32, 1, 2), 2);
        assert_eq!(fold_imul(Type::I32, 5, 7), 35);
        assert_eq!(fold_imul(Type::I32, 4294967295, 2), 4294967294);
        assert_eq!(fold_imul(Type::I32, 4294967290, 4294967294), 12);

        assert_eq!(fold_imul(Type::I64, 1, 2), 2);
        assert_eq!(fold_imul(Type::I64, 5, 7), 35);
        assert_eq!(fold_imul(Type::I64, 4294967295, 2), 8589934590);
        assert_eq!(
            fold_imul(Type::I64, 4294967290, 4294967294),
            18446744039349813260
        );
        assert_eq!(
            fold_imul(Type::I64, 18446744073709551615, 2),
            18446744073709551614
        );
        assert_eq!(
            fold_imul(Type::I64, 18446744073709551610, 18446744073709551614),
            12
        );
    }

    #[test]
    fn sdiv() {
        assert_eq!(fold_sdiv(Type::I32, 10, 0), None);
        assert_eq!(fold_sdiv(Type::I32, 2147483648, 4294967295), None);

        // `sdiv` should always round toward 0.
        assert_eq!(fold_sdiv(Type::I32, 10, 5), Some(2));
        assert_eq!(fold_sdiv(Type::I32, 4294967286, 5), Some(4294967294));
        assert_eq!(fold_sdiv(Type::I32, 10, 4294967291), Some(4294967294));
        assert_eq!(fold_sdiv(Type::I32, 4294967286, 4294967291), Some(2));
        assert_eq!(fold_sdiv(Type::I32, 37, 4), Some(9));
        assert_eq!(fold_sdiv(Type::I32, 4294967259, 4), Some(4294967287));
        assert_eq!(fold_sdiv(Type::I32, 37, 4294967292), Some(4294967287));
        assert_eq!(fold_sdiv(Type::I32, 4294967259, 4294967292), Some(9));

        assert_eq!(fold_sdiv(Type::I64, 10, 0), None);
        assert_eq!(fold_sdiv(Type::I64, 2147483648, 4294967295), Some(0));
        assert_eq!(
            fold_sdiv(Type::I64, 9223372036854775808, 18446744073709551615),
            None
        );

        // `sdiv` should always round toward 0.
        assert_eq!(fold_sdiv(Type::I64, 10, 5), Some(2));
        assert_eq!(
            fold_sdiv(Type::I64, 18446744073709551606, 5),
            Some(18446744073709551614)
        );
        assert_eq!(
            fold_sdiv(Type::I64, 10, 18446744073709551611),
            Some(18446744073709551614)
        );
        assert_eq!(
            fold_sdiv(Type::I64, 18446744073709551606, 18446744073709551611),
            Some(2)
        );
        assert_eq!(fold_sdiv(Type::I64, 37, 4), Some(9));
        assert_eq!(
            fold_sdiv(Type::I64, 18446744073709551579, 4),
            Some(18446744073709551607)
        );
        assert_eq!(
            fold_sdiv(Type::I64, 37, 18446744073709551612),
            Some(18446744073709551607)
        );
        assert_eq!(
            fold_sdiv(Type::I64, 18446744073709551579, 18446744073709551612),
            Some(9)
        );

        assert_eq!(fold_sdiv(Type::I64, 4294967286, 5), Some(858993457));
        assert_eq!(fold_sdiv(Type::I64, 10, 4294967291), Some(0));
        assert_eq!(fold_sdiv(Type::I64, 4294967286, 4294967291), Some(0));
        assert_eq!(fold_sdiv(Type::I64, 4294967259, 4), Some(1073741814));
        assert_eq!(fold_sdiv(Type::I64, 37, 4294967292), Some(0));
        assert_eq!(fold_sdiv(Type::I64, 4294967259, 4294967292), Some(0));
    }

    #[test]
    fn udiv() {
        assert_eq!(fold_udiv(Type::I32, 10, 0), None);
        assert_eq!(fold_udiv(Type::I32, 2147483648, 4294967295), Some(0));

        assert_eq!(fold_udiv(Type::I32, 10, 5), Some(2));
        assert_eq!(fold_udiv(Type::I32, 4294967286, 5), Some(858993457));
        assert_eq!(fold_udiv(Type::I32, 10, 4294967291), Some(0));
        assert_eq!(fold_udiv(Type::I32, 4294967286, 4294967291), Some(0));
        assert_eq!(fold_udiv(Type::I32, 37, 4), Some(9));
        assert_eq!(fold_udiv(Type::I32, 4294967259, 4), Some(1073741814));
        assert_eq!(fold_udiv(Type::I32, 37, 4294967292), Some(0));
        assert_eq!(fold_udiv(Type::I32, 4294967259, 4294967292), Some(0));

        assert_eq!(fold_udiv(Type::I64, 10, 0), None);
        assert_eq!(fold_udiv(Type::I64, 2147483648, 4294967295), Some(0));
        assert_eq!(
            fold_udiv(Type::I64, 9223372036854775808, 18446744073709551615),
            Some(0)
        );

        assert_eq!(fold_udiv(Type::I64, 10, 5), Some(2));
        assert_eq!(
            fold_udiv(Type::I64, 18446744073709551606, 5),
            Some(3689348814741910321)
        );
        assert_eq!(fold_udiv(Type::I64, 10, 18446744073709551611), Some(0));
        assert_eq!(
            fold_udiv(Type::I64, 18446744073709551606, 18446744073709551611),
            Some(0)
        );
        assert_eq!(fold_udiv(Type::I64, 37, 4), Some(9));
        assert_eq!(
            fold_udiv(Type::I64, 18446744073709551579, 4),
            Some(4611686018427387894)
        );
        assert_eq!(fold_udiv(Type::I64, 37, 18446744073709551612), Some(0));
        assert_eq!(
            fold_udiv(Type::I64, 18446744073709551579, 18446744073709551612),
            Some(0)
        );

        assert_eq!(fold_udiv(Type::I64, 4294967286, 5), Some(858993457));
        assert_eq!(fold_udiv(Type::I64, 10, 4294967291), Some(0));
        assert_eq!(fold_udiv(Type::I64, 4294967286, 4294967291), Some(0));
        assert_eq!(fold_udiv(Type::I64, 4294967259, 4), Some(1073741814));
        assert_eq!(fold_udiv(Type::I64, 37, 4294967292), Some(0));
        assert_eq!(fold_udiv(Type::I64, 4294967259, 4294967292), Some(0));
    }

    #[test]
    fn srem() {
        assert_eq!(fold_srem(Type::I32, 10, 0), None);
        assert_eq!(fold_srem(Type::I32, 2147483648, 4294967295), None);

        // `srem` should always round toward 0 and adopt the sign of the dividend.
        assert_eq!(fold_srem(Type::I32, 10, 5), Some(0));
        assert_eq!(fold_srem(Type::I32, 4294967286, 5), Some(0));
        assert_eq!(fold_srem(Type::I32, 10, 4294967291), Some(0));
        assert_eq!(fold_srem(Type::I32, 4294967286, 4294967291), Some(0));
        assert_eq!(fold_srem(Type::I32, 37, 4), Some(1));
        assert_eq!(fold_srem(Type::I32, 4294967259, 4), Some(4294967295));
        assert_eq!(fold_srem(Type::I32, 37, 4294967292), Some(1));
        assert_eq!(
            fold_srem(Type::I32, 4294967259, 4294967292),
            Some(4294967295)
        );

        assert_eq!(fold_srem(Type::I64, 10, 0), None);
        assert_eq!(
            fold_srem(Type::I64, 2147483648, 4294967295),
            Some(2147483648)
        );
        assert_eq!(
            fold_srem(Type::I64, 9223372036854775808, 18446744073709551615),
            None
        );

        // `srem` should always round toward 0 and adopt the sign of the dividend.
        assert_eq!(fold_srem(Type::I64, 10, 5), Some(0));
        assert_eq!(fold_srem(Type::I64, 18446744073709551606, 5), Some(0));
        assert_eq!(fold_srem(Type::I64, 10, 18446744073709551611), Some(0));
        assert_eq!(
            fold_srem(Type::I64, 18446744073709551606, 18446744073709551611),
            Some(0)
        );
        assert_eq!(fold_srem(Type::I64, 37, 4), Some(1));
        assert_eq!(
            fold_srem(Type::I64, 18446744073709551579, 4),
            Some(18446744073709551615)
        );
        assert_eq!(fold_srem(Type::I64, 37, 18446744073709551612), Some(1));
        assert_eq!(
            fold_srem(Type::I64, 18446744073709551579, 18446744073709551612),
            Some(18446744073709551615)
        );

        assert_eq!(fold_srem(Type::I64, 4294967286, 5), Some(1));
        assert_eq!(fold_srem(Type::I64, 10, 4294967291), Some(10));
        assert_eq!(
            fold_srem(Type::I64, 4294967286, 4294967291),
            Some(4294967286)
        );
        assert_eq!(fold_srem(Type::I64, 4294967259, 4), Some(3));
        assert_eq!(fold_srem(Type::I64, 37, 4294967292), Some(37));
        assert_eq!(
            fold_srem(Type::I64, 4294967259, 4294967292),
            Some(4294967259)
        );
    }

    #[test]
    fn urem() {
        assert_eq!(fold_urem(Type::I32, 10, 0), None);
        assert_eq!(
            fold_urem(Type::I32, 2147483648, 4294967295),
            Some(2147483648)
        );

        assert_eq!(fold_urem(Type::I32, 10, 5), Some(0));
        assert_eq!(fold_urem(Type::I32, 4294967286, 5), Some(1));
        assert_eq!(fold_urem(Type::I32, 10, 4294967291), Some(10));
        assert_eq!(
            fold_urem(Type::I32, 4294967286, 4294967291),
            Some(4294967286)
        );
        assert_eq!(fold_urem(Type::I32, 37, 4), Some(1));
        assert_eq!(fold_urem(Type::I32, 4294967259, 4), Some(3));
        assert_eq!(fold_urem(Type::I32, 37, 4294967292), Some(37));
        assert_eq!(
            fold_urem(Type::I32, 4294967259, 4294967292),
            Some(4294967259)
        );

        assert_eq!(fold_urem(Type::I64, 10, 0), None);
        assert_eq!(
            fold_urem(Type::I64, 2147483648, 4294967295),
            Some(2147483648)
        );
        assert_eq!(
            fold_urem(Type::I64, 9223372036854775808, 18446744073709551615),
            Some(9223372036854775808)
        );

        assert_eq!(fold_urem(Type::I64, 10, 5), Some(0));
        assert_eq!(fold_urem(Type::I64, 18446744073709551606, 5), Some(1));
        assert_eq!(fold_urem(Type::I64, 10, 18446744073709551611), Some(10));
        assert_eq!(
            fold_urem(Type::I64, 18446744073709551606, 18446744073709551611),
            Some(18446744073709551606)
        );
        assert_eq!(fold_urem(Type::I64, 37, 4), Some(1));
        assert_eq!(fold_urem(Type::I64, 18446744073709551579, 4), Some(3));
        assert_eq!(fold_urem(Type::I64, 37, 18446744073709551612), Some(37));
        assert_eq!(
            fold_urem(Type::I64, 18446744073709551579, 18446744073709551612),
            Some(18446744073709551579)
        );

        assert_eq!(fold_urem(Type::I64, 4294967286, 5), Some(1));
        assert_eq!(fold_urem(Type::I64, 10, 4294967291), Some(10));
        assert_eq!(
            fold_urem(Type::I64, 4294967286, 4294967291),
            Some(4294967286)
        );
        assert_eq!(fold_urem(Type::I64, 4294967259, 4), Some(3));
        assert_eq!(fold_urem(Type::I64, 37, 4294967292), Some(37));
        assert_eq!(
            fold_urem(Type::I64, 4294967259, 4294967292),
            Some(4294967259)
        );
    }

    #[test]
    fn sfill() {
        assert_eq!(fold_sfill(Type::I32, 0, 1), 0);
        assert_eq!(fold_sfill(Type::I32, 1, 1), 4294967295);
        assert_eq!(fold_sfill(Type::I32, 2, 1), 0);
        assert_eq!(fold_sfill(Type::I32, 1, 3), 1);
        assert_eq!(fold_sfill(Type::I32, 4, 3), 4294967292);
        assert_eq!(fold_sfill(Type::I32, 4, 8), 4);
        assert_eq!(fold_sfill(Type::I32, 252, 8), 4294967292);
        assert_eq!(fold_sfill(Type::I32, 239, 8), 4294967279);
        assert_eq!(fold_sfill(Type::I32, 255, 8), 4294967295);
        assert_eq!(fold_sfill(Type::I32, 256, 8), 0);
        assert_eq!(fold_sfill(Type::I32, 4, 16), 4);
        assert_eq!(fold_sfill(Type::I32, 65532, 16), 4294967292);
        assert_eq!(fold_sfill(Type::I32, 65519, 16), 4294967279);
        assert_eq!(fold_sfill(Type::I32, 65535, 16), 4294967295);
        assert_eq!(fold_sfill(Type::I32, 65536, 16), 0);
        assert_eq!(fold_sfill(Type::I64, 0, 1), 0);
        assert_eq!(fold_sfill(Type::I64, 1, 1), 18446744073709551615);
        assert_eq!(fold_sfill(Type::I64, 2, 1), 0);
        assert_eq!(fold_sfill(Type::I64, 1, 3), 1);
        assert_eq!(fold_sfill(Type::I64, 4, 3), 18446744073709551612);
        assert_eq!(fold_sfill(Type::I64, 4, 8), 4);
        assert_eq!(fold_sfill(Type::I64, 252, 8), 18446744073709551612);
        assert_eq!(fold_sfill(Type::I64, 239, 8), 18446744073709551599);
        assert_eq!(fold_sfill(Type::I64, 255, 8), 18446744073709551615);
        assert_eq!(fold_sfill(Type::I64, 256, 8), 0);
        assert_eq!(fold_sfill(Type::I64, 4, 16), 4);
        assert_eq!(fold_sfill(Type::I64, 65532, 16), 18446744073709551612);
        assert_eq!(fold_sfill(Type::I64, 65519, 16), 18446744073709551599);
        assert_eq!(fold_sfill(Type::I64, 65535, 16), 18446744073709551615);
        assert_eq!(fold_sfill(Type::I64, 65536, 16), 0);
    }

    #[test]
    fn icmp_eq() {
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Eq, 5, 1), 0);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Eq, 5, 5), 1);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Ne, 5, 1), 1);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Ne, 5, 5), 0);

        assert_eq!(fold_icmp(Type::I64, IcmpKind::Eq, 5, 1), 0);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Eq, 5, 5), 1);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Ne, 5, 1), 1);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Ne, 5, 5), 0);

        assert_eq!(fold_icmp(Type::I64, IcmpKind::Eq, 4294967296, 0), 0);
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Eq, 4294967296, 4294967296),
            1
        );
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Ne, 4294967296, 0), 1);
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Ne, 4294967296, 4294967296),
            0
        );
    }

    #[test]
    fn icmp_slt() {
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Slt, 3, 5), 1);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Slt, 5, 3), 0);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Slt, 5, 5), 0);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Slt, 4294967291, 3), 1);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Slt, 4294967291, 0), 1);
        assert_eq!(
            fold_icmp(Type::I32, IcmpKind::Slt, 4294967291, 4294967293),
            1
        );
        assert_eq!(
            fold_icmp(Type::I32, IcmpKind::Slt, 4294967293, 4294967291),
            0
        );
        assert_eq!(
            fold_icmp(Type::I32, IcmpKind::Slt, 4294967291, 4294967291),
            0
        );

        assert_eq!(fold_icmp(Type::I32, IcmpKind::Sle, 3, 5), 1);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Sle, 5, 3), 0);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Sle, 5, 5), 1);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Sle, 4294967291, 3), 1);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Sle, 4294967291, 0), 1);
        assert_eq!(
            fold_icmp(Type::I32, IcmpKind::Sle, 4294967291, 4294967293),
            1
        );
        assert_eq!(
            fold_icmp(Type::I32, IcmpKind::Sle, 4294967293, 4294967291),
            0
        );
        assert_eq!(
            fold_icmp(Type::I32, IcmpKind::Sle, 4294967291, 4294967291),
            1
        );

        assert_eq!(fold_icmp(Type::I64, IcmpKind::Slt, 3, 5), 1);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Slt, 5, 3), 0);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Slt, 5, 5), 0);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Slt, 4294967291, 3), 0);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Slt, 4294967291, 0), 0);
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Slt, 4294967291, 4294967293),
            1
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Slt, 4294967293, 4294967291),
            0
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Slt, 4294967291, 4294967291),
            0
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Slt, 18446744073709551611, 3),
            1
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Slt, 18446744073709551611, 0),
            1
        );
        assert_eq!(
            fold_icmp(
                Type::I64,
                IcmpKind::Slt,
                18446744073709551611,
                18446744073709551613
            ),
            1
        );
        assert_eq!(
            fold_icmp(
                Type::I64,
                IcmpKind::Slt,
                18446744073709551613,
                18446744073709551611
            ),
            0
        );
        assert_eq!(
            fold_icmp(
                Type::I64,
                IcmpKind::Slt,
                18446744073709551611,
                18446744073709551611
            ),
            0
        );

        assert_eq!(fold_icmp(Type::I64, IcmpKind::Sle, 3, 5), 1);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Sle, 5, 3), 0);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Sle, 5, 5), 1);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Sle, 4294967291, 3), 0);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Sle, 4294967291, 0), 0);
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Sle, 4294967291, 4294967293),
            1
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Sle, 4294967293, 4294967291),
            0
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Sle, 4294967291, 4294967291),
            1
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Sle, 18446744073709551611, 3),
            1
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Sle, 18446744073709551611, 0),
            1
        );
        assert_eq!(
            fold_icmp(
                Type::I64,
                IcmpKind::Sle,
                18446744073709551611,
                18446744073709551613
            ),
            1
        );
        assert_eq!(
            fold_icmp(
                Type::I64,
                IcmpKind::Sle,
                18446744073709551613,
                18446744073709551611
            ),
            0
        );
        assert_eq!(
            fold_icmp(
                Type::I64,
                IcmpKind::Sle,
                18446744073709551611,
                18446744073709551611
            ),
            1
        );
    }

    #[test]
    fn icmp_ult() {
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Ult, 3, 5), 1);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Ult, 5, 3), 0);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Ult, 5, 5), 0);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Ult, 4294967291, 3), 0);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Ult, 4294967291, 0), 0);
        assert_eq!(
            fold_icmp(Type::I32, IcmpKind::Ult, 4294967291, 4294967293),
            1
        );
        assert_eq!(
            fold_icmp(Type::I32, IcmpKind::Ult, 4294967293, 4294967291),
            0
        );
        assert_eq!(
            fold_icmp(Type::I32, IcmpKind::Ult, 4294967291, 4294967291),
            0
        );

        assert_eq!(fold_icmp(Type::I32, IcmpKind::Ule, 3, 5), 1);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Ule, 5, 3), 0);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Ule, 5, 5), 1);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Ule, 4294967291, 3), 0);
        assert_eq!(fold_icmp(Type::I32, IcmpKind::Ule, 4294967291, 0), 0);
        assert_eq!(
            fold_icmp(Type::I32, IcmpKind::Ule, 4294967291, 4294967293),
            1
        );
        assert_eq!(
            fold_icmp(Type::I32, IcmpKind::Ule, 4294967293, 4294967291),
            0
        );
        assert_eq!(
            fold_icmp(Type::I32, IcmpKind::Ule, 4294967291, 4294967291),
            1
        );

        assert_eq!(fold_icmp(Type::I64, IcmpKind::Ult, 3, 5), 1);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Ult, 5, 3), 0);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Ult, 5, 5), 0);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Ult, 4294967291, 3), 0);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Ult, 4294967291, 0), 0);
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Ult, 4294967291, 4294967293),
            1
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Ult, 4294967293, 4294967291),
            0
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Ult, 4294967291, 4294967291),
            0
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Ult, 18446744073709551611, 3),
            0
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Ult, 18446744073709551611, 0),
            0
        );
        assert_eq!(
            fold_icmp(
                Type::I64,
                IcmpKind::Ult,
                18446744073709551611,
                18446744073709551613
            ),
            1
        );
        assert_eq!(
            fold_icmp(
                Type::I64,
                IcmpKind::Ult,
                18446744073709551613,
                18446744073709551611
            ),
            0
        );
        assert_eq!(
            fold_icmp(
                Type::I64,
                IcmpKind::Ult,
                18446744073709551611,
                18446744073709551611
            ),
            0
        );

        assert_eq!(fold_icmp(Type::I64, IcmpKind::Ule, 3, 5), 1);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Ule, 5, 3), 0);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Ule, 5, 5), 1);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Ule, 4294967291, 3), 0);
        assert_eq!(fold_icmp(Type::I64, IcmpKind::Ule, 4294967291, 0), 0);
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Ule, 4294967291, 4294967293),
            1
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Ule, 4294967293, 4294967291),
            0
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Ule, 4294967291, 4294967291),
            1
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Ule, 18446744073709551611, 3),
            0
        );
        assert_eq!(
            fold_icmp(Type::I64, IcmpKind::Ule, 18446744073709551611, 0),
            0
        );
        assert_eq!(
            fold_icmp(
                Type::I64,
                IcmpKind::Ule,
                18446744073709551611,
                18446744073709551613
            ),
            1
        );
        assert_eq!(
            fold_icmp(
                Type::I64,
                IcmpKind::Ule,
                18446744073709551613,
                18446744073709551611
            ),
            0
        );
        assert_eq!(
            fold_icmp(
                Type::I64,
                IcmpKind::Ule,
                18446744073709551611,
                18446744073709551611
            ),
            1
        );
    }
}

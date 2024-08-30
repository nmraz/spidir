pub fn align_down(val: u32, align: u32) -> u32 {
    debug_assert!(align.is_power_of_two());
    val & 0u32.wrapping_sub(align)
}

pub fn align_up(val: u32, align: u32) -> u32 {
    align_down(val + align - 1, align)
}

pub fn is_uint<const N: u32>(val: u64) -> bool {
    debug_assert!(N > 0);
    debug_assert!(N <= 64);

    // Shift by `N`, folding to zero when the high bit is shifted out completely.
    let bound = (1u64 << (N - 1)) << 1;
    val & bound.wrapping_sub(1) == val
}

pub fn is_sint<const N: u32>(val: u64) -> bool {
    debug_assert!(N > 0);
    debug_assert!(N <= 64);

    // All bits from the sign bit onward should be identical.
    matches!(((val as i64) >> (N - 1)) as u64, 0 | u64::MAX)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn align_down_works() {
        assert_eq!(align_down(2, 4), 0);
        assert_eq!(align_down(0, 4), 0);
        assert_eq!(align_down(3, 16), 0);
        assert_eq!(align_down(32, 16), 32);
    }

    #[test]
    fn align_up_works() {
        assert_eq!(align_up(2, 4), 4);
        assert_eq!(align_up(0, 4), 0);
        assert_eq!(align_up(3, 16), 16);
        assert_eq!(align_up(32, 16), 32);
    }

    #[test]
    fn is_uint_works() {
        assert!(is_uint::<1>(0));
        assert!(is_uint::<1>(1));
        assert!(!is_uint::<1>(3));
        assert!(is_uint::<8>(u8::MAX as u64));
        assert!(!is_uint::<8>(-5i64 as u64));
        assert!(is_uint::<32>(u32::MAX as u64));
        assert!(!is_uint::<32>(u32::MAX as u64 + 1));
        assert!(is_uint::<64>(u64::MAX));
    }

    #[test]
    fn is_sint_works() {
        assert!(is_sint::<1>(0));
        assert!(!is_sint::<1>(1));
        assert!(is_sint::<1>(-1i64 as u64));
        assert!(!is_sint::<1>(3));
        assert!(is_sint::<2>(-2i64 as u64));
        assert!(!is_sint::<2>(-3i64 as u64));
        assert!(!is_sint::<8>(u8::MAX as u64));
        assert!(is_sint::<8>(0u8.wrapping_sub(u8::MAX) as u64));
        assert!(is_sint::<8>(-5i64 as u64));
        assert!(is_sint::<32>(i32::MIN as u64));
        assert!(!is_sint::<32>(u32::MAX as u64));
        assert!(!is_sint::<32>(u32::MAX as u64 + 1));
        assert!(is_sint::<64>(u64::MAX));
    }
}

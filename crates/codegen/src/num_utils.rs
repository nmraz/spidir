pub fn align_down(val: u32, align: u32) -> u32 {
    debug_assert!(align.is_power_of_two());
    val & 0u32.wrapping_sub(align)
}

pub fn align_up(val: u32, align: u32) -> u32 {
    align_down(val + align - 1, align)
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
}

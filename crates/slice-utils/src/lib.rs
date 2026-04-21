#![cfg_attr(not(test), no_std)]

pub fn coalesce<T>(slice: &mut [T], mut coalesce: impl FnMut(&T, &T) -> Option<T>) -> usize {
    if slice.is_empty() {
        return 0;
    }

    let mut base = 0;

    for cur in 1..slice.len() {
        let base_val = &slice[base];
        let cur_val = &slice[cur];

        if let Some(new_val) = coalesce(base_val, cur_val) {
            // Values can be merged, update our base value in place and try to merge it with the
            // next value.
            slice[base] = new_val;
        } else {
            // Values cannot be merged, move our base up and try again.
            base += 1;
            slice.swap(base, cur);
        }
    }

    base + 1
}

pub fn dedup<T: Copy + PartialEq>(slice: &mut [T]) -> usize {
    coalesce(slice, |a, b| if a == b { Some(*a) } else { None })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_coalesce_histogram(slice: &mut [(i32, i32)], expected: &[(i32, i32)]) {
        let new_len = coalesce(slice, |&(prev_id, prev_count), &(cur_id, cur_count)| {
            if prev_id == cur_id {
                Some((prev_id, prev_count + cur_count))
            } else {
                None
            }
        });

        assert_eq!(&slice[..new_len], expected);
    }

    #[test]
    fn coalesce_empty() {
        check_coalesce_histogram(&mut [], &[])
    }

    #[test]
    fn coalesce_singleton() {
        check_coalesce_histogram(&mut [(1, 2)], &[(1, 2)]);
    }

    #[test]
    fn coalesce_interesting() {
        check_coalesce_histogram(
            &mut [
                (1, 13),
                (1, 72),
                (2, 53),
                (2, 30),
                (2, 37),
                (7, 80),
                (9, 36),
                (23, 70),
                (5, 70),
                (5, 35),
                (5, 38),
                (5, 40),
            ],
            &[(1, 85), (2, 120), (7, 80), (9, 36), (23, 70), (5, 183)],
        );
    }

    #[test]
    fn dedup_simple() {
        let slice = &mut [1, 1, 2, 3, 8, 9, 17, 17, 17, 20];
        let dedup_len = dedup(slice);
        let slice = &slice[..dedup_len];
        assert_eq!(slice, &[1, 2, 3, 8, 9, 17, 20])
    }
}

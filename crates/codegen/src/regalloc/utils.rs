use crate::{
    cfg::CfgContext,
    lir::{Instr, Lir},
    machine::MachineCore,
};

pub fn get_instr_weight<M: MachineCore>(lir: &Lir<M>, cfg_ctx: &CfgContext, instr: Instr) -> f32 {
    let block = lir.instr_block(instr);
    let loop_depth = cfg_ctx
        .depth_map
        .loop_depth(cfg_ctx.domtree.get_tree_node(block).unwrap());
    1000f32 * ((loop_depth + 1) as f32)
}

pub fn coalesce_slice<T: Copy>(
    slice: &mut [T],
    mut coalesce: impl FnMut(&T, &T) -> Option<T>,
) -> usize {
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
            slice[base] = *cur_val;
        }
    }

    base + 1
}

#[cfg(test)]
mod tests {
    use super::coalesce_slice;

    fn check_coalesce_histogram_slice(slice: &mut [(i32, i32)], expected: &[(i32, i32)]) {
        let new_len = coalesce_slice(slice, |&(prev_id, prev_count), &(cur_id, cur_count)| {
            if prev_id == cur_id {
                Some((prev_id, prev_count + cur_count))
            } else {
                None
            }
        });

        assert_eq!(&slice[..new_len], expected);
    }

    #[test]
    fn coalesce_empty_slice() {
        check_coalesce_histogram_slice(&mut [], &[])
    }

    #[test]
    fn coalesce_singleton_slice() {
        check_coalesce_histogram_slice(&mut [(1, 2)], &[(1, 2)]);
    }

    #[test]
    fn coalesce_interesting_slice() {
        check_coalesce_histogram_slice(
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
}

use crate::{
    cfg::{Block, CfgContext},
    lir::{Instr, Lir},
    machine::MachineRegalloc,
};

use super::types::PhysRegHint;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChangeStatus {
    Unchanged,
    Changed,
}

impl ChangeStatus {
    pub fn is_changed(&self) -> bool {
        matches!(self, Self::Changed)
    }
}

pub fn get_instr_weight<M: MachineRegalloc>(
    lir: &Lir<M>,
    cfg_ctx: &CfgContext,
    instr: Instr,
) -> f32 {
    let block = cfg_ctx.block_order[lir.instr_block_index(instr)];
    get_block_weight(cfg_ctx, block)
}

pub fn get_block_weight(cfg_ctx: &CfgContext, block: Block) -> f32 {
    let loop_depth = cfg_ctx
        .depth_map
        .loop_depth(cfg_ctx.domtree.get_tree_node(block).unwrap());
    1000f32 * ((loop_depth + 1) as f32)
}

pub fn sort_reg_hints(hints: &mut [PhysRegHint]) -> usize {
    // First: group the hints by physical register.
    hints.sort_unstable_by_key(|hint| hint.preg.as_u8());

    // Coalesce adjacent hints for the same register, recording total weight for each.
    let new_len = coalesce_slice(hints, |prev_hint, cur_hint| {
        if prev_hint.preg == cur_hint.preg {
            Some(PhysRegHint {
                preg: prev_hint.preg,
                weight: prev_hint.weight + cur_hint.weight,
            })
        } else {
            None
        }
    });

    // Now, sort the hints in order of decreasing weight.
    hints[..new_len]
        .sort_unstable_by(|lhs, rhs| lhs.weight.partial_cmp(&rhs.weight).unwrap().reverse());

    new_len
}

fn coalesce_slice<T: Copy>(
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

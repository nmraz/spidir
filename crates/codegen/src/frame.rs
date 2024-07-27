use core::cmp;

use alloc::vec::Vec;

use cranelift_entity::SecondaryMap;

use crate::{
    lir::{Lir, MemLayout, StackSlot},
    machine::MachineCore,
    regalloc::{Assignment, SpillSlot},
};

pub struct FrameLayout {
    pub full_layout: MemLayout,
    pub stack_slot_offsets: SecondaryMap<StackSlot, u32>,
    pub spill_slot_offsets: SecondaryMap<SpillSlot, u32>,
}

impl FrameLayout {
    pub fn compute<M: MachineCore>(lir: &Lir<M>, regalloc_assignment: &Assignment) -> Self {
        let mut frame_objects = Vec::new();
        for slot in lir.stack_slots() {
            frame_objects.push(FrameObject {
                index: AllocOrSpill::Alloc(slot),
                layout: lir.stack_slot_layout(slot),
                offset: 0,
            });
        }

        for spill in regalloc_assignment.spill_slots() {
            frame_objects.push(FrameObject {
                index: AllocOrSpill::Spill(spill),
                layout: regalloc_assignment.spill_slot_layout(spill),
                offset: 0,
            })
        }

        // Sort our objects by _descending_ key, so we can place more-aligned objects earlier.
        frame_objects.sort_unstable_by_key(|b| cmp::Reverse(frame_object_sort_key(b)));

        let mut frame_size = 0;
        let mut frame_align = 0;
        for obj in &mut frame_objects {
            frame_align = frame_align.max(obj.layout.align);
            let offset = align_up(frame_size, obj.layout.align);
            obj.offset = offset;
            frame_size = offset + obj.layout.size;
        }

        let mut layout = Self {
            full_layout: MemLayout {
                size: frame_size,
                align: frame_align,
            },
            stack_slot_offsets: SecondaryMap::with_capacity(lir.stack_slots().len()),
            spill_slot_offsets: SecondaryMap::with_capacity(
                regalloc_assignment.spill_slots().len(),
            ),
        };

        for obj in &frame_objects {
            match obj.index {
                AllocOrSpill::Alloc(slot) => {
                    layout.stack_slot_offsets[slot] = obj.offset;
                }
                AllocOrSpill::Spill(spill) => {
                    layout.spill_slot_offsets[spill] = obj.offset;
                }
            }
        }

        layout
    }
}

fn align_up(val: u32, align: u32) -> u32 {
    (val + align - 1) & 0u32.wrapping_sub(align)
}

enum AllocOrSpill {
    Alloc(StackSlot),
    Spill(SpillSlot),
}

struct FrameObject {
    index: AllocOrSpill,
    layout: MemLayout,
    offset: u32,
}

fn frame_object_sort_key(obj: &FrameObject) -> u64 {
    // Sort by alignment, break ties with size.
    (obj.layout.align as u64) << 32 | obj.layout.size as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn align_up_works() {
        assert_eq!(align_up(2, 4), 4);
        assert_eq!(align_up(0, 4), 0);
        assert_eq!(align_up(3, 16), 16);
        assert_eq!(align_up(32, 16), 32);
    }
}

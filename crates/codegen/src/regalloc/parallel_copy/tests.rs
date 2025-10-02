use std::fmt::Write;

use cranelift_entity::EntityRef;
use expect_test::{Expect, expect};
use fx_utils::FxHashMap;

use crate::{
    lir::{Instr, PhysReg, RegBank, RegClass},
    regalloc::{
        SpillSlot,
        test_utils::{
            copy_source_to_string, operand_to_string, parse_copy_dest, parse_copy_source,
        },
        types::ParallelCopyPhase,
    },
};

use super::*;

struct DummyRegScavenger {
    reg_count: u8,
    next_spill: u32,
    used_reg_width: RegWidth,
    tmp_spill_widths: FxHashMap<SpillSlot, RegWidth>,
}

impl RegScavenger for DummyRegScavenger {
    fn scavenge_regs(&self, _bank: RegBank) -> impl Iterator<Item = (PhysReg, ScavengedRegState)> {
        let count = self.reg_count.max(1);
        (0..count).map(|i| {
            let state = if i >= self.reg_count {
                ScavengedRegState::InUse(self.used_reg_width)
            } else {
                ScavengedRegState::Available
            };
            (PhysReg::new(i), state)
        })
    }

    fn alloc_tmp_spill(&mut self, class: RegClass) -> SpillSlot {
        let spill = SpillSlot::from_u32(self.next_spill);
        self.next_spill += 1;
        self.tmp_spill_widths.insert(spill, class.width());
        spill
    }

    fn expand_tmp_spill(&mut self, spill: SpillSlot, class: RegClass) {
        let width = self.tmp_spill_widths.get_mut(&spill).unwrap();
        *width = (*width).max(class.width());
    }
}

fn check_resolution_with_reg_count_and_used_reg_width(
    reg_count: u8,
    used_reg_width: RegWidth,
    input: &str,
    expected: Expect,
) {
    let mut parallel_copies = Vec::new();

    for line in input.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let mut parts = line.split('=');
        let (to, width) = parse_copy_dest(parts.next().unwrap().trim());
        let from = parse_copy_source(parts.next().unwrap().trim());

        parallel_copies.push(ParallelCopy {
            instr: Instr::new(0),
            class: RegClass::new(RegBank::new(0), width),
            phase: ParallelCopyPhase::InterInstr,
            from,
            to,
        });
    }

    let mut scavenger = DummyRegScavenger {
        reg_count,
        next_spill: 1234,
        used_reg_width,
        tmp_spill_widths: Default::default(),
    };

    let mut resolved = Vec::new();
    let mut state = ResolverState::new();
    resolve(&parallel_copies, &mut state, &mut scavenger, |copy| {
        resolved.push(*copy)
    });

    let mut output = String::new();

    if !scavenger.tmp_spill_widths.is_empty() {
        for (&spill, &width) in &scavenger.tmp_spill_widths {
            writeln!(output, "s{}: w{}", spill.as_u32(), width.as_u8()).unwrap();
        }
        writeln!(output).unwrap();
    }

    for copy in &resolved {
        let width = copy.class.width().as_u8();
        writeln!(
            output,
            "{}:w{width} = {}",
            operand_to_string(copy.to),
            copy_source_to_string(copy.from),
        )
        .unwrap();
    }

    expected.assert_eq(&output);
}

fn check_resolution_with_reg_count(reg_count: u8, input: &str, expected: Expect) {
    check_resolution_with_reg_count_and_used_reg_width(
        reg_count,
        RegWidth::new(0),
        input,
        expected,
    );
}

fn check_resolution(input: &str, expected: Expect) {
    check_resolution_with_reg_count(32, input, expected);
}

#[test]
fn single_copy() {
    check_resolution(
        "
            r0 = r1
            ",
        expect![[r#"
                r0:w0 = r1
            "#]],
    )
}

#[test]
fn simple_remat() {
    check_resolution(
        "
            r0 = i7
            ",
        expect![[r#"
                r0:w0 = i7
            "#]],
    );
}

#[test]
fn disjoint_copies() {
    check_resolution(
        "
            r0 = r1
            r2 = r3
            r4 = r5
            r6 = r7
            ",
        expect![[r#"
                r6:w0 = r7
                r4:w0 = r5
                r2:w0 = r3
                r0:w0 = r1
            "#]],
    )
}

#[test]
fn disjoint_copies_with_remat() {
    check_resolution(
        "
            r0 = r1
            r2 = i9
            r4 = r5
            r6 = r7
            ",
        expect![[r#"
                r6:w0 = r7
                r4:w0 = r5
                r2:w0 = i9
                r0:w0 = r1
            "#]],
    )
}

#[test]
fn overlapping_copy_chain() {
    check_resolution(
        "
            r0 = r1
            r1 = r2
            r2 = r3
            r3 = r4
            ",
        expect![[r#"
                r0:w0 = r1
                r1:w0 = r2
                r2:w0 = r3
                r3:w0 = r4
            "#]],
    )
}

#[test]
fn overlapping_copy_chain_rev() {
    check_resolution(
        "
            r3 = r4
            r2 = r3
            r1 = r2
            r0 = r1
            ",
        expect![[r#"
                r0:w0 = r1
                r1:w0 = r2
                r2:w0 = r3
                r3:w0 = r4
            "#]],
    )
}

#[test]
fn overlapping_copy_chain_with_remat() {
    check_resolution(
        "
            r0 = r1
            r1 = r2
            r2 = r3
            r3 = i0
            ",
        expect![[r#"
                r0:w0 = r1
                r1:w0 = r2
                r2:w0 = r3
                r3:w0 = i0
            "#]],
    )
}

#[test]
fn overlapping_copy_chain_with_remat_rev() {
    check_resolution(
        "
            r3 = i0
            r2 = r3
            r1 = r2
            r0 = r1
            ",
        expect![[r#"
                r0:w0 = r1
                r1:w0 = r2
                r2:w0 = r3
                r3:w0 = i0
            "#]],
    )
}

#[test]
fn disjoint_overlapping_copy_chains() {
    check_resolution(
        "
            r0 = r1
            r6 = r7
            r2 = r3
            r4 = r5
            r1 = r2
            r5 = r6
            ",
        expect![[r#"
                r4:w0 = r5
                r5:w0 = r6
                r6:w0 = r7
                r0:w0 = r1
                r1:w0 = r2
                r2:w0 = r3
            "#]],
    )
}

#[test]
fn copy_chain_merge() {
    check_resolution(
        "
            r0 = r1
            r1 = r2
            r6 = r8
            r4 = r5
            r2 = r3
            r3 = r6
            r7 = r6
            r5 = r3
            ",
        expect![[r#"
                r7:w0 = r6
                r4:w0 = r5
                r5:w0 = r3
                r0:w0 = r1
                r1:w0 = r2
                r2:w0 = r3
                r3:w0 = r6
                r6:w0 = r8
            "#]],
    )
}

#[test]
fn copy_chain_merge_with_remat() {
    check_resolution(
        "
            r0 = r1
            r1 = r2
            r6 = i8
            r4 = r5
            r2 = r3
            r3 = r6
            r7 = r6
            r5 = r3
            ",
        expect![[r#"
                r7:w0 = r6
                r4:w0 = r5
                r5:w0 = r3
                r0:w0 = r1
                r1:w0 = r2
                r2:w0 = r3
                r3:w0 = r6
                r6:w0 = i8
            "#]],
    )
}

#[test]
fn swap_regs() {
    check_resolution(
        "
            r0 = r1
            r1 = r0
            ",
        expect![[r#"
                r2:w0 = r0
                r0:w0 = r1
                r1:w0 = r2
            "#]],
    )
}

#[test]
fn swap_regs_with_chain_merge() {
    check_resolution(
        "
            r4 = r2
            r0 = r1
            r1 = r0
            r2 = r0
            r3 = r1
            ",
        expect![[r#"
                r3:w0 = r1
                r4:w0 = r2
                r2:w0 = r0
                r5:w0 = r0
                r0:w0 = r1
                r1:w0 = r5
            "#]],
    )
}

#[test]
fn disjoint_swaps() {
    check_resolution(
        "
            r0 = r1
            r2 = r3
            r1 = r0
            r3 = r2
            ",
        expect![[r#"
                r4:w0 = r2
                r2:w0 = r3
                r3:w0 = r4
                r4:w0 = r0
                r0:w0 = r1
                r1:w0 = r4
            "#]],
    )
}

#[test]
fn large_copy_cycle() {
    check_resolution(
        "
            r0 = r1
            r1 = r2
            r2 = r3
            r3 = r4
            r4 = r5
            r5 = r0
            ",
        expect![[r#"
                r6:w0 = r0
                r0:w0 = r1
                r1:w0 = r2
                r2:w0 = r3
                r3:w0 = r4
                r4:w0 = r5
                r5:w0 = r6
            "#]],
    )
}

#[test]
fn disjoint_large_copy_cycles_with_chain_merge() {
    check_resolution(
        "
            r1 = r2
            r2 = r3
            r10 = r11
            r12 = r8
            r3 = r0
            r6 = r4
            r0 = r1
            r5 = r4
            r4 = r1
            r8 = r9
            r9 = r10
            r11 = r8
            ",
        expect![[r#"
                r5:w0 = r4
                r6:w0 = r4
                r4:w0 = r1
                r12:w0 = r8
                r7:w0 = r10
                r10:w0 = r11
                r11:w0 = r8
                r8:w0 = r9
                r9:w0 = r7
                r7:w0 = r1
                r1:w0 = r2
                r2:w0 = r3
                r3:w0 = r0
                r0:w0 = r7
            "#]],
    )
}

#[test]
fn copy_stack_to_reg() {
    check_resolution(
        "
            r0 = s0
            ",
        expect![[r#"
                r0:w0 = s0
            "#]],
    )
}

#[test]
fn copy_reg_to_stack() {
    check_resolution(
        "
            s0 = r0
            ",
        expect![[r#"
                s0:w0 = r0
            "#]],
    )
}

#[test]
fn copy_stack_to_stack() {
    check_resolution(
        "
            s1 = s0
            ",
        expect![[r#"
                r0:w0 = s0
                s1:w0 = r0
            "#]],
    )
}

#[test]
fn remat_to_stack() {
    check_resolution(
        "
            s1 = i0
            ",
        expect![[r#"
                r0:w0 = i0
                s1:w0 = r0
            "#]],
    )
}

#[test]
fn disjoint_stack_copies() {
    check_resolution(
        "
            s1 = s0
            s3 = s2
            ",
        expect![[r#"
                r0:w0 = s2
                s3:w0 = r0
                r0:w0 = s0
                s1:w0 = r0
            "#]],
    )
}

#[test]
fn overlapping_stack_copy_chain() {
    check_resolution(
        "
            s1 = s0
            s3 = s2
            s2 = s1
            ",
        expect![[r#"
                r0:w0 = s2
                s3:w0 = r0
                r0:w0 = s1
                s2:w0 = r0
                r0:w0 = s0
                s1:w0 = r0
            "#]],
    )
}

#[test]
fn large_stack_copy_cycle() {
    check_resolution(
        "
            s0 = s1
            s1 = s2
            s2 = s3
            s3 = s4
            s4 = s5
            s5 = s0
            ",
        expect![[r#"
                r0:w0 = s0
                r1:w0 = s1
                s0:w0 = r1
                r1:w0 = s2
                s1:w0 = r1
                r1:w0 = s3
                s2:w0 = r1
                r1:w0 = s4
                s3:w0 = r1
                r1:w0 = s5
                s4:w0 = r1
                s5:w0 = r0
            "#]],
    )
}

#[test]
fn copy_stack_to_stack_no_regs() {
    check_resolution_with_reg_count(
        0,
        "
            s0 = s1
            ",
        expect![[r#"
                s1234: w0

                s1234:w0 = r0
                r0:w0 = s1
                s0:w0 = r0
                r0:w0 = s1234
            "#]],
    )
}

#[test]
fn copy_multiple_stack_to_stack_no_regs() {
    check_resolution_with_reg_count(
        0,
        "
            s1 = s2
            s0 = s1
            ",
        expect![[r#"
                s1234: w0

                s1234:w0 = r0
                r0:w0 = s1
                s0:w0 = r0
                r0:w0 = s1234
                s1234:w0 = r0
                r0:w0 = s2
                s1:w0 = r0
                r0:w0 = s1234
            "#]],
    )
}

#[test]
fn disjoint_swaps_no_regs() {
    check_resolution_with_reg_count(
        4,
        "
            r0 = r1
            r2 = r3
            r1 = r0
            r3 = r2
            ",
        expect![[r#"
                s1234: w0

                s1234:w0 = r2
                r2:w0 = r3
                r3:w0 = s1234
                s1234:w0 = r0
                r0:w0 = r1
                r1:w0 = s1234
            "#]],
    )
}

#[test]
fn large_copy_cycle_no_regs() {
    check_resolution_with_reg_count(
        6,
        "
            r0 = r1
            r1 = r2
            r2 = r3
            r3 = r4
            r4 = r5
            r5 = r0
            ",
        expect![[r#"
                s1234: w0

                s1234:w0 = r0
                r0:w0 = r1
                r1:w0 = r2
                r2:w0 = r3
                r3:w0 = r4
                r4:w0 = r5
                r5:w0 = s1234
            "#]],
    )
}

#[test]
fn large_stack_copy_cycle_single_reg() {
    check_resolution_with_reg_count(
        1,
        "
            s0 = s1
            s1 = s2
            s2 = s3
            s3 = s4
            s4 = s5
            s5 = s0
            ",
        expect![[r#"
                s1234: w0

                r0:w0 = s0
                s1234:w0 = r0
                r0:w0 = s1
                s0:w0 = r0
                r0:w0 = s1234
                s1234:w0 = r0
                r0:w0 = s2
                s1:w0 = r0
                r0:w0 = s1234
                s1234:w0 = r0
                r0:w0 = s3
                s2:w0 = r0
                r0:w0 = s1234
                s1234:w0 = r0
                r0:w0 = s4
                s3:w0 = r0
                r0:w0 = s1234
                s1234:w0 = r0
                r0:w0 = s5
                s4:w0 = r0
                r0:w0 = s1234
                s5:w0 = r0
            "#]],
    )
}

#[test]
fn large_stack_copy_cycle_no_regs() {
    check_resolution_with_reg_count(
        0,
        "
            s0 = s1
            s1 = s2
            s2 = s3
            s3 = s4
            s4 = s5
            s5 = s0
            ",
        expect![[r#"
                s1235: w0
                s1234: w0

                s1235:w0 = r0
                r0:w0 = s0
                s1234:w0 = r0
                r0:w0 = s1235
                s1235:w0 = r0
                r0:w0 = s1
                s0:w0 = r0
                r0:w0 = s1235
                s1235:w0 = r0
                r0:w0 = s2
                s1:w0 = r0
                r0:w0 = s1235
                s1235:w0 = r0
                r0:w0 = s3
                s2:w0 = r0
                r0:w0 = s1235
                s1235:w0 = r0
                r0:w0 = s4
                s3:w0 = r0
                r0:w0 = s1235
                s1235:w0 = r0
                r0:w0 = s5
                s4:w0 = r0
                r0:w0 = s1235
                s1235:w0 = r0
                r0:w0 = s1234
                s5:w0 = r0
                r0:w0 = s1235
            "#]],
    )
}

#[test]
fn disjoint_copies_mixed_widths() {
    check_resolution(
        "
            r0:w1 = r1
            r2:w0 = r3
            r4:w2 = r5
            r6:w1 = r7
            ",
        expect![[r#"
                r6:w1 = r7
                r4:w2 = r5
                r2:w0 = r3
                r0:w1 = r1
            "#]],
    )
}

#[test]
fn overlapping_copy_chain_mixed_widths() {
    check_resolution(
        "
            r0:w1 = r1
            r1:w2 = r2
            r2:w3 = r3
            r3:w4 = r4
            ",
        expect![[r#"
                r0:w1 = r1
                r1:w2 = r2
                r2:w3 = r3
                r3:w4 = r4
            "#]],
    )
}

#[test]
fn swap_regs_mixed_widths() {
    check_resolution(
        "
            r0:w1 = r1
            r1:w2 = r0
            ",
        expect![[r#"
                r2:w2 = r0
                r0:w1 = r1
                r1:w2 = r2
            "#]],
    )
}

#[test]
fn large_copy_cycle_mixed_widths() {
    check_resolution(
        "
            r0:w1 = r1
            r1:w2 = r2
            r2:w3 = r3
            r3:w4 = r4
            r4:w5 = r5
            r5:w6 = r0
            ",
        expect![[r#"
                r6:w6 = r0
                r0:w1 = r1
                r1:w2 = r2
                r2:w3 = r3
                r3:w4 = r4
                r4:w5 = r5
                r5:w6 = r6
            "#]],
    )
}

#[test]
fn disjoint_stack_copies_mixed_widths() {
    check_resolution(
        "
            s1:w1 = s0
            s3:w2 = s2
            ",
        expect![[r#"
                r0:w2 = s2
                s3:w2 = r0
                r0:w1 = s0
                s1:w1 = r0
            "#]],
    )
}

#[test]
fn copy_stack_to_stack_different_width() {
    check_resolution(
        "
            s1:w3 = s0
            ",
        expect![[r#"
                r0:w3 = s0
                s1:w3 = r0
            "#]],
    )
}

#[test]
fn large_stack_copy_cycle_mixed_widths() {
    check_resolution(
        "
            s0:w1 = s1
            s1:w2 = s2
            s2:w3 = s3
            s3:w4 = s4
            s4:w5 = s5
            s5:w6 = s0
            ",
        expect![[r#"
                r0:w6 = s0
                r1:w1 = s1
                s0:w1 = r1
                r1:w2 = s2
                s1:w2 = r1
                r1:w3 = s3
                s2:w3 = r1
                r1:w4 = s4
                s3:w4 = r1
                r1:w5 = s5
                s4:w5 = r1
                s5:w6 = r0
            "#]],
    )
}

#[test]
fn copy_stack_to_stack_no_regs_different_width() {
    check_resolution_with_reg_count(
        0,
        "
            s0:w3 = s1
            ",
        expect![[r#"
                s1234: w0

                s1234:w0 = r0
                r0:w3 = s1
                s0:w3 = r0
                r0:w0 = s1234
            "#]],
    )
}

#[test]
fn copy_stack_to_stack_no_regs_different_width_used_reg_width() {
    check_resolution_with_reg_count_and_used_reg_width(
        0,
        RegWidth::new(2),
        "
            s0:w3 = s1
            ",
        expect![[r#"
                s1234: w2

                s1234:w2 = r0
                r0:w3 = s1
                s0:w3 = r0
                r0:w2 = s1234
            "#]],
    )
}

#[test]
fn disjoint_swaps_no_regs_mixed_widths() {
    check_resolution_with_reg_count(
        4,
        "
            r0:w3 = r1
            r2:w4 = r3
            r1:w5 = r0
            r3:w6 = r2
            ",
        expect![[r#"
                s1234: w6

                s1234:w6 = r2
                r2:w4 = r3
                r3:w6 = s1234
                s1234:w5 = r0
                r0:w3 = r1
                r1:w5 = s1234
            "#]],
    )
}

#[test]
fn large_copy_cycle_no_regs_mixed_widths() {
    check_resolution_with_reg_count(
        6,
        "
            r0:w1 = r1
            r1:w2 = r2
            r2:w3 = r3
            r3:w4 = r4
            r4:w5 = r5
            r5:w6 = r0
            ",
        expect![[r#"
                s1234: w6

                s1234:w6 = r0
                r0:w1 = r1
                r1:w2 = r2
                r2:w3 = r3
                r3:w4 = r4
                r4:w5 = r5
                r5:w6 = s1234
            "#]],
    )
}

#[test]
fn large_stack_copy_cycle_single_reg_mixed_widths() {
    check_resolution_with_reg_count(
        1,
        "
            s0:w2 = s1
            s1:w3 = s2
            s2:w4 = s3
            s3:w5 = s4
            s4:w6 = s5
            s5:w7 = s0
            ",
        expect![[r#"
                s1234: w7

                r0:w7 = s0
                s1234:w7 = r0
                r0:w2 = s1
                s0:w2 = r0
                r0:w7 = s1234
                s1234:w7 = r0
                r0:w3 = s2
                s1:w3 = r0
                r0:w7 = s1234
                s1234:w7 = r0
                r0:w4 = s3
                s2:w4 = r0
                r0:w7 = s1234
                s1234:w7 = r0
                r0:w5 = s4
                s3:w5 = r0
                r0:w7 = s1234
                s1234:w7 = r0
                r0:w6 = s5
                s4:w6 = r0
                r0:w7 = s1234
                s5:w7 = r0
            "#]],
    )
}

#[test]
fn large_stack_copy_cycle_no_regs_mixed_widths() {
    check_resolution_with_reg_count(
        0,
        "
            s0:w2 = s1
            s1:w3 = s2
            s2:w4 = s3
            s3:w5 = s4
            s4:w6 = s5
            s5:w7 = s0
            ",
        expect![[r#"
                s1235: w0
                s1234: w7

                s1235:w0 = r0
                r0:w7 = s0
                s1234:w7 = r0
                r0:w0 = s1235
                s1235:w0 = r0
                r0:w2 = s1
                s0:w2 = r0
                r0:w0 = s1235
                s1235:w0 = r0
                r0:w3 = s2
                s1:w3 = r0
                r0:w0 = s1235
                s1235:w0 = r0
                r0:w4 = s3
                s2:w4 = r0
                r0:w0 = s1235
                s1235:w0 = r0
                r0:w5 = s4
                s3:w5 = r0
                r0:w0 = s1235
                s1235:w0 = r0
                r0:w6 = s5
                s4:w6 = r0
                r0:w0 = s1235
                s1235:w0 = r0
                r0:w7 = s1234
                s5:w7 = r0
                r0:w0 = s1235
            "#]],
    )
}

#[test]
fn large_stack_copy_cycle_no_regs_mixed_widths_used_reg_width() {
    check_resolution_with_reg_count_and_used_reg_width(
        0,
        RegWidth::new(1),
        "
            s0:w2 = s1
            s1:w3 = s2
            s2:w4 = s3
            s3:w5 = s4
            s4:w6 = s5
            s5:w7 = s0
            ",
        expect![[r#"
                s1235: w1
                s1234: w7

                s1235:w1 = r0
                r0:w7 = s0
                s1234:w7 = r0
                r0:w1 = s1235
                s1235:w1 = r0
                r0:w2 = s1
                s0:w2 = r0
                r0:w1 = s1235
                s1235:w1 = r0
                r0:w3 = s2
                s1:w3 = r0
                r0:w1 = s1235
                s1235:w1 = r0
                r0:w4 = s3
                s2:w4 = r0
                r0:w1 = s1235
                s1235:w1 = r0
                r0:w5 = s4
                s3:w5 = r0
                r0:w1 = s1235
                s1235:w1 = r0
                r0:w6 = s5
                s4:w6 = r0
                r0:w1 = s1235
                s1235:w1 = r0
                r0:w7 = s1234
                s5:w7 = r0
                r0:w1 = s1235
            "#]],
    )
}

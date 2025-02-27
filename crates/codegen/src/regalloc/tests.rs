use expect_test::{Expect, expect};

use crate::{
    cfg::{BlockCfg, CfgContext},
    lir::{
        Builder as LirBuilder, MemLayout, OperandPos, PhysReg, RegClass, UseOperandConstraint,
        test_utils::{DummyInstr, DummyMachine, RC_GPR, REG_R0, REG_R1, REG_R2, push_instr},
    },
    machine::MachineRegalloc,
};

impl MachineRegalloc for DummyMachine {
    fn phys_reg_count() -> u32 {
        3
    }

    fn usable_regs(&self, class: RegClass) -> &[PhysReg] {
        match class {
            RC_GPR => &[REG_R0, REG_R1, REG_R2],
            _ => unreachable!(),
        }
    }

    fn reg_class_spill_layout(&self, class: RegClass) -> MemLayout {
        match class {
            RC_GPR => MemLayout { size: 4, align: 4 },
            _ => unreachable!(),
        }
    }
}

fn check_regalloc_block(build: impl FnOnce(&mut LirBuilder<'_, DummyMachine>), expected: Expect) {
    let mut cfg = BlockCfg::new();
    let entry = cfg.create_block();

    let cfg_ctx = CfgContext::compute(cfg, entry);

    let mut builder = LirBuilder::new(&cfg_ctx.block_order);
    builder.advance_block();
    build(&mut builder);
    builder.advance_block();
    let lir = builder.finish();

    let assignment = super::run(&lir, &cfg_ctx, &DummyMachine).expect("regalloc failed");
    if let Err(err) = super::verify(&lir, &cfg_ctx, &assignment) {
        panic!("invalid allocation: {}", err.display(&lir, &assignment));
    }
    expected.assert_eq(&assignment.display(&cfg_ctx.block_order, &lir).to_string());
}

// The full x64 isel/regalloc pipeline is too finnicky to reliably create copy cycles that need
// to be broken with a spill, so manually test for that case here.

#[test]
fn copy_cycle_all_regs() {
    check_regalloc_block(
        |builder| {
            // Call something with all available registers permuted.
            let [a2, a3, a1] = push_instr(
                builder,
                DummyInstr::Call,
                [],
                [
                    (UseOperandConstraint::Fixed(REG_R0), OperandPos::Early),
                    (UseOperandConstraint::Fixed(REG_R1), OperandPos::Early),
                    (UseOperandConstraint::Fixed(REG_R2), OperandPos::Early),
                ],
            );

            builder.set_incoming_block_params([a1, a2, a3]);
            builder.set_live_in_regs(vec![REG_R0, REG_R1, REG_R2]);
        },
        expect![[r#"
                  block0:
                      $spill0 = $r2
                      $r2 = $r0
                      $r0 = $r1
                      $r1 = $spill0
            0000:     Call $r0, $r1, $r2
        "#]],
    );
}

#[test]
fn copy_cycle_all_regs_twice() {
    check_regalloc_block(
        |builder| {
            // Use our parameters in the same registers we get on entry.
            let [a1, a2, a3] = push_instr(
                builder,
                DummyInstr::Call,
                [],
                [
                    (UseOperandConstraint::Fixed(REG_R0), OperandPos::Early),
                    (UseOperandConstraint::Fixed(REG_R1), OperandPos::Early),
                    (UseOperandConstraint::Fixed(REG_R2), OperandPos::Early),
                ],
            );

            // Use our parameters completely permuted.
            let [a2p, a3p, a1p] = push_instr(
                builder,
                DummyInstr::Call,
                [],
                [
                    (UseOperandConstraint::Fixed(REG_R0), OperandPos::Early),
                    (UseOperandConstraint::Fixed(REG_R1), OperandPos::Early),
                    (UseOperandConstraint::Fixed(REG_R2), OperandPos::Early),
                ],
            );

            builder.copy_vreg(a1p, a1);
            builder.copy_vreg(a2p, a2);
            builder.copy_vreg(a3p, a3);

            builder.set_incoming_block_params([a1, a2, a3]);
            builder.set_live_in_regs(vec![REG_R0, REG_R1, REG_R2]);
        },
        expect![[r#"
                  block0:
                      $spill0 = $r2
                      $r2 = $r0
                      $r0 = $r1
                      $r1 = $spill0
            0000:     Call $r0, $r1, $r2
                      $spill0 = $r0
                      $r0 = $r2
                      $r2 = $r1
                      $r1 = $spill0
            0001:     Call $r0, $r1, $r2
        "#]],
    );
}

#[test]
fn copy_cycle_no_free_reg() {
    check_regalloc_block(
        |builder| {
            // Keep r2 live across our call.
            let [a3] = push_instr(
                builder,
                DummyInstr::Ret,
                [],
                [(UseOperandConstraint::Fixed(REG_R2), OperandPos::Early)],
            );

            // Call something with all available registers permuted.
            let [a2, a1] = push_instr(
                builder,
                DummyInstr::Call,
                [],
                [
                    (UseOperandConstraint::Fixed(REG_R0), OperandPos::Early),
                    (UseOperandConstraint::Fixed(REG_R1), OperandPos::Early),
                ],
            );

            builder.set_incoming_block_params([a1, a2, a3]);
            builder.set_live_in_regs(vec![REG_R0, REG_R1, REG_R2]);
        },
        expect![[r#"
                  block0:
                      $spill0 = $r1
                      $r1 = $r0
                      $r0 = $spill0
            0000:     Call $r0, $r1
            0001:     Ret $r2
        "#]],
    );
}

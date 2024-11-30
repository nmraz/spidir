use expect_test::{expect, Expect};

use crate::{
    cfg::{Block, BlockCfg, CfgContext},
    lir::{
        test_utils::{push_instr, DummyInstr, DummyMachine, RC_GPR, REG_R0, REG_R1, REG_R2},
        Builder as LirBuilder, Lir, MemLayout, OperandPos, PhysReg, RegClass, UseOperandConstraint,
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

fn check_regalloc(lir: &Lir<DummyMachine>, cfg: BlockCfg, block_order: &[Block], expected: Expect) {
    let cfg_ctx = CfgContext::compute(cfg, block_order[0]);
    let assignment = super::run(lir, &cfg_ctx, &DummyMachine).expect("regalloc failed");
    if let Err(err) = super::verify(lir, &cfg_ctx, &assignment) {
        panic!("invalid allocation: {}", err.display(lir, &assignment));
    }
    expected.assert_eq(&assignment.display(block_order, lir).to_string());
}

// The full x64 isel/regalloc pipeline is too finnicky to reliably create copy cycles that need
// to be broken with a spill, so manually test for that case here.

#[test]
fn copy_cycle_all_regs() {
    let mut cfg = BlockCfg::new();
    let block = cfg.create_block();
    let block_order = [block];

    let mut builder = LirBuilder::new(&block_order);
    builder.advance_block();

    // Call something with all available registers permuted.
    let [a2, a3, a1] = push_instr(
        &mut builder,
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

    builder.advance_block();

    let lir = builder.finish();
    check_regalloc(
        &lir,
        cfg,
        &block_order,
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
    let mut cfg = BlockCfg::new();
    let block = cfg.create_block();
    let block_order = [block];

    let mut builder = LirBuilder::new(&block_order);
    builder.advance_block();

    // Use our parameters in the same registers we get on entry.
    let [a1, a2, a3] = push_instr(
        &mut builder,
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
        &mut builder,
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

    builder.advance_block();

    let lir = builder.finish();
    check_regalloc(
        &lir,
        cfg,
        &block_order,
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
    let mut cfg = BlockCfg::new();
    let block = cfg.create_block();
    let block_order = [block];

    let mut builder = LirBuilder::new(&block_order);
    builder.advance_block();

    // Keep r2 live across our call.
    let [a3] = push_instr(
        &mut builder,
        DummyInstr::Ret,
        [],
        [(UseOperandConstraint::Fixed(REG_R2), OperandPos::Early)],
    );

    // Call something with all available registers permuted.
    let [a2, a1] = push_instr(
        &mut builder,
        DummyInstr::Call,
        [],
        [
            (UseOperandConstraint::Fixed(REG_R0), OperandPos::Early),
            (UseOperandConstraint::Fixed(REG_R1), OperandPos::Early),
        ],
    );

    builder.set_incoming_block_params([a1, a2, a3]);
    builder.set_live_in_regs(vec![REG_R0, REG_R1, REG_R2]);

    builder.advance_block();

    let lir = builder.finish();
    check_regalloc(
        &lir,
        cfg,
        &block_order,
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

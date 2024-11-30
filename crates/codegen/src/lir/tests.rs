use expect_test::{expect, Expect};

use crate::cfg::{Block, BlockCfg};

use super::{
    test_utils::{
        push_instr, push_instr_with_clobbers, DummyInstr, DummyMachine, RC_GPR, REG_R0, REG_R1,
        REG_R2,
    },
    Builder, DefOperand, Lir, MemLayout, OperandPos, PhysRegSet, UseOperandConstraint,
};

fn check_lir(lir: Lir<DummyMachine>, cfg: &BlockCfg, block_order: &[Block], expected: Expect) {
    for &block in block_order {
        for instr in lir.block_instrs(block) {
            let instr_block_index = lir.instr_block_index(instr);
            let instr_block = block_order[instr_block_index];
            assert_eq!(instr_block, block);
        }
    }
    expected.assert_eq(&lir.display(cfg, block_order).to_string());
}

#[test]
#[should_panic(expected = "attempted to create empty LIR block: block1")]
fn build_empty_block() {
    let mut cfg = BlockCfg::new();
    let block0 = cfg.create_block();
    let block1 = cfg.create_block();
    let block_order = [block0, block1];

    let mut builder = Builder::<DummyMachine>::new(&block_order);
    builder.advance_block();
    builder.advance_block();
}

#[test]
fn build_simple_tied() {
    let mut cfg = BlockCfg::new();
    let block = cfg.create_block();
    let block_order = [block];

    let mut builder = Builder::new(&block_order);
    builder.advance_block();

    let [retval] = push_instr(
        &mut builder,
        DummyInstr::Ret,
        [],
        [(UseOperandConstraint::Fixed(REG_R0), OperandPos::Early)],
    );
    let [a, b] = push_instr(
        &mut builder,
        DummyInstr::Add,
        [DefOperand::any(retval)],
        [
            (UseOperandConstraint::TiedToDef(0), OperandPos::Early),
            (UseOperandConstraint::AnyReg, OperandPos::Early),
        ],
    );
    builder.set_incoming_block_params([a, b]);
    builder.advance_block();
    builder.set_live_in_regs(vec![REG_R0, REG_R1]);
    let lir = builder.finish();

    check_lir(
        lir,
        &cfg,
        &block_order,
        expect![[r#"
                  block0[%1:gpr($r0), %2:gpr($r1)]:
            0000:      %0:gpr(any)[late] = Add %1(tied:0)[early], %2(reg)[early]
            0001:      Ret %0($r0)[early]
        "#]],
    );
}

#[test]
fn build_simple_3addr() {
    let mut cfg = BlockCfg::new();
    let block = cfg.create_block();
    let block_order = [block];

    let mut builder = Builder::new(&block_order);
    builder.advance_block();

    let [retval] = push_instr(
        &mut builder,
        DummyInstr::Ret,
        [],
        [(UseOperandConstraint::Fixed(REG_R0), OperandPos::Early)],
    );
    let [a, b] = push_instr(
        &mut builder,
        DummyInstr::Lea,
        [DefOperand::any_reg(retval)],
        [
            (UseOperandConstraint::AnyReg, OperandPos::Early),
            (UseOperandConstraint::AnyReg, OperandPos::Early),
        ],
    );
    builder.set_incoming_block_params([a, b]);
    builder.advance_block();
    builder.set_live_in_regs(vec![REG_R0, REG_R1]);
    let lir = builder.finish();

    check_lir(
        lir,
        &cfg,
        &block_order,
        expect![[r#"
                  block0[%1:gpr($r0), %2:gpr($r1)]:
            0000:      %0:gpr(reg)[late] = Lea %1(reg)[early], %2(reg)[early]
            0001:      Ret %0($r0)[early]
        "#]],
    );
}

#[test]
fn multi_block() {
    let mut cfg = BlockCfg::new();

    let entry = cfg.create_block();
    let left = cfg.create_block();
    let right = cfg.create_block();
    let exit = cfg.create_block();
    cfg.add_block_edge(entry, left);
    cfg.add_block_edge(entry, right);
    cfg.add_block_edge(left, exit);
    cfg.add_block_edge(right, exit);

    let block_order = [entry, left, right, exit];
    let mut builder = Builder::new(&block_order);
    builder.advance_block();

    // `exit`
    let [retval] = push_instr(
        &mut builder,
        DummyInstr::Ret,
        [],
        [(UseOperandConstraint::Fixed(REG_R0), OperandPos::Early)],
    );
    builder.set_incoming_block_params([retval]);
    builder.advance_block();

    // `right`
    let add5 = builder.create_vreg(RC_GPR);
    builder.set_outgoing_block_params([add5]);
    push_instr(&mut builder, DummyInstr::Jump(exit), [], []);
    let [right_param2, five] = push_instr(
        &mut builder,
        DummyInstr::Add,
        [DefOperand::any(add5)],
        [
            (UseOperandConstraint::TiedToDef(0), OperandPos::Early),
            (UseOperandConstraint::AnyReg, OperandPos::Early),
        ],
    );
    push_instr(
        &mut builder,
        DummyInstr::MovI(5),
        [DefOperand::any_reg(five)],
        [],
    );
    builder.advance_block();

    // `left`
    let add_params = builder.create_vreg(RC_GPR);
    builder.set_outgoing_block_params([add_params]);
    push_instr(&mut builder, DummyInstr::Jump(exit), [], []);
    let [left_param2, left_param3] = push_instr(
        &mut builder,
        DummyInstr::Add,
        [DefOperand::any(add_params)],
        [
            (UseOperandConstraint::TiedToDef(0), OperandPos::Early),
            (UseOperandConstraint::AnyReg, OperandPos::Early),
        ],
    );
    builder.advance_block();

    // `entry`
    push_instr(&mut builder, DummyInstr::JmpEq(left, right), [], []);
    let [param1, param2] = push_instr(
        &mut builder,
        DummyInstr::Cmp,
        [],
        [
            (UseOperandConstraint::Any, OperandPos::Early),
            (UseOperandConstraint::AnyReg, OperandPos::Early),
        ],
    );
    builder.copy_vreg(right_param2, param2);
    builder.copy_vreg(left_param2, param2);

    builder.set_incoming_block_params([param1, param2, left_param3]);
    builder.advance_block();
    builder.set_live_in_regs(vec![REG_R0, REG_R1, REG_R2]);
    let lir = builder.finish();

    check_lir(
        lir,
        &cfg,
        &block_order,
        expect![[r#"
                  block0[%7:gpr($r0), %8:gpr($r1), %6:gpr($r2)]:
            0000:      Cmp %7(any)[early], %8(reg)[early]
            0001:      JmpEq(block1, block2)
                  => block1, block2
                  block1:
            0002:      %4:gpr(any)[late] = Add %8(tied:0)[early], %6(reg)[early]
            0003:      Jump(block3)
                  => block3[%4:gpr]
                  block2:
            0004:      %3:gpr(reg)[late] = MovI(5)
            0005:      %1:gpr(any)[late] = Add %8(tied:0)[early], %3(reg)[early]
            0006:      Jump(block3)
                  => block3[%1:gpr]
                  block3[%0:gpr]:
            0007:      Ret %0($r0)[early]
        "#]],
    );
}

#[test]
fn block_param_vreg_copies() {
    let mut cfg = BlockCfg::new();

    let entry = cfg.create_block();
    let exit = cfg.create_block();
    cfg.add_block_edge(entry, exit);

    let block_order = [entry, exit];
    let mut builder = Builder::new(&block_order);
    builder.advance_block();

    let [retval] = push_instr(
        &mut builder,
        DummyInstr::Ret,
        [],
        [(UseOperandConstraint::Fixed(REG_R0), OperandPos::Early)],
    );
    builder.set_incoming_block_params([retval]);
    builder.advance_block();

    let outgoing_param = builder.create_vreg(RC_GPR);
    builder.set_outgoing_block_params([outgoing_param]);

    push_instr(&mut builder, DummyInstr::Jump(exit), [], []);

    let five = builder.create_vreg(RC_GPR);
    push_instr(
        &mut builder,
        DummyInstr::MovI(5),
        [DefOperand::any_reg(five)],
        [],
    );

    builder.copy_vreg(outgoing_param, five);
    builder.advance_block();

    let lir = builder.finish();

    check_lir(
        lir,
        &cfg,
        &block_order,
        expect![[r#"
                  block0[]:
            0000:      %2:gpr(reg)[late] = MovI(5)
            0001:      Jump(block1)
                  => block1[%2:gpr]
                  block1[%0:gpr]:
            0002:      Ret %0($r0)[early]
        "#]],
    );
}

#[test]
fn stack_slots() {
    let mut cfg = BlockCfg::new();
    let block = cfg.create_block();
    let block_order = [block];

    let mut builder = Builder::new(&block_order);

    builder.create_stack_slot(MemLayout { size: 4, align: 4 });
    builder.create_stack_slot(MemLayout { size: 16, align: 8 });

    builder.advance_block();

    let [retval] = push_instr(
        &mut builder,
        DummyInstr::Ret,
        [],
        [(UseOperandConstraint::Fixed(REG_R0), OperandPos::Early)],
    );
    builder.set_incoming_block_params([retval]);
    builder.advance_block();
    builder.set_live_in_regs(vec![REG_R0]);
    let lir = builder.finish();

    check_lir(
        lir,
        &cfg,
        &block_order,
        expect![[r#"
            !0 = stackslot 4:4
            !1 = stackslot 16:8
                  block0[%0:gpr($r0)]:
            0000:      Ret %0($r0)[early]
        "#]],
    );
}

#[test]
fn clobbers() {
    let mut cfg = BlockCfg::new();
    let block = cfg.create_block();
    let block_order = [block];

    let mut builder = Builder::new(&block_order);

    builder.advance_block();

    let [retval] = push_instr(
        &mut builder,
        DummyInstr::Ret,
        [],
        [(UseOperandConstraint::Fixed(REG_R0), OperandPos::Early)],
    );
    let [param] = push_instr_with_clobbers(
        &mut builder,
        DummyInstr::Call,
        [DefOperand::fixed(retval, REG_R0)],
        [(UseOperandConstraint::Fixed(REG_R0), OperandPos::Early)],
        PhysRegSet::from_iter([REG_R1, REG_R2]),
    );
    builder.set_incoming_block_params([param]);
    builder.advance_block();
    builder.set_live_in_regs(vec![REG_R0]);
    let lir = builder.finish();

    check_lir(
        lir,
        &cfg,
        &block_order,
        expect![[r#"
                  block0[%1:gpr($r0)]:
            0000:      %0:gpr($r0)[late] = Call %1($r0)[early] ^($r1, $r2)
            0001:      Ret %0($r0)[early]
        "#]],
    );
}

use expect_test::{expect, Expect};

use crate::cfg::{Block, BlockCfg};

use super::{
    Builder, DefOperand, DefOperandConstraint, Lir, OperandPos, PhysReg, RegClass, RegNames,
    UseOperand, UseOperandConstraint, VirtReg,
};

#[derive(Debug, Clone, Copy)]
enum DummyInstr {
    MovI(u64),
    Add,
    Lea,
    Cmp,
    Ret,
    Jump(Block),
    JmpEq(Block, Block),
}

const RC_GPR: RegClass = RegClass::new(0);

const REG_R0: PhysReg = PhysReg::new(0);
const REG_R1: PhysReg = PhysReg::new(1);
const REG_R2: PhysReg = PhysReg::new(2);

struct DummyRegNames;
impl RegNames for DummyRegNames {
    fn reg_class_name(&self, class: RegClass) -> &str {
        match class {
            RC_GPR => "gpr",
            _ => unreachable!(),
        }
    }

    fn reg_name(&self, reg: PhysReg) -> &str {
        match reg {
            REG_R0 => "r0",
            REG_R1 => "r1",
            REG_R2 => "r2",
            _ => unreachable!(),
        }
    }
}

fn push_instr<const U: usize>(
    builder: &mut Builder<DummyInstr>,
    instr: DummyInstr,
    defs: impl IntoIterator<Item = DefOperand>,
    uses: [(UseOperandConstraint, OperandPos); U],
) -> [VirtReg; U] {
    let mut use_regs = [VirtReg::new(0, RC_GPR); U];
    builder.build_instrs(|mut b| {
        for use_reg in &mut use_regs {
            *use_reg = b.create_vreg(RC_GPR);
        }
        b.push_instr(
            instr,
            defs,
            uses.iter()
                .enumerate()
                .map(|(i, &(constraint, pos))| UseOperand::new(use_regs[i], constraint, pos)),
        );
    });
    use_regs
}

fn check_lir(lir: Lir<DummyInstr>, cfg: &BlockCfg, block_order: &[Block], expected: Expect) {
    expected.assert_eq(&lir.display(cfg, block_order, &DummyRegNames).to_string());
}

#[test]
fn build_simple_tied() {
    let mut cfg = BlockCfg::new();
    let block = cfg.create_block();
    let block_order = [block];

    let mut builder = Builder::new(&block_order);
    let [retval] = push_instr(
        &mut builder,
        DummyInstr::Ret,
        [],
        [(UseOperandConstraint::Fixed(REG_R0), OperandPos::Early)],
    );
    let [a, b] = push_instr(
        &mut builder,
        DummyInstr::Add,
        [DefOperand::new(
            retval,
            DefOperandConstraint::Any,
            OperandPos::Late,
        )],
        [
            (UseOperandConstraint::TiedToDef(0), OperandPos::Early),
            (UseOperandConstraint::AnyReg, OperandPos::Early),
        ],
    );
    builder.set_incoming_block_params([a, b]);
    builder.finish_block();
    builder.set_live_in_regs(vec![REG_R0, REG_R1]);
    let lir = builder.finish();

    check_lir(
        lir,
        &cfg,
        &block_order,
        expect![[r#"
            block0[%1:gpr($r0), %2:gpr($r1)]:
                %0:gpr(any)[late] = Add %1:gpr(tied:0)[early], %2:gpr(reg)[early]
                Ret %0:gpr($r0)[early]
        "#]],
    );
}

#[test]
fn build_simple_3addr() {
    let mut cfg = BlockCfg::new();
    let block = cfg.create_block();
    let block_order = [block];

    let mut builder = Builder::new(&block_order);
    let [retval] = push_instr(
        &mut builder,
        DummyInstr::Ret,
        [],
        [(UseOperandConstraint::Fixed(REG_R0), OperandPos::Early)],
    );
    let [a, b] = push_instr(
        &mut builder,
        DummyInstr::Lea,
        [DefOperand::new(
            retval,
            DefOperandConstraint::AnyReg,
            OperandPos::Late,
        )],
        [
            (UseOperandConstraint::AnyReg, OperandPos::Early),
            (UseOperandConstraint::AnyReg, OperandPos::Early),
        ],
    );
    builder.set_incoming_block_params([a, b]);
    builder.finish_block();
    builder.set_live_in_regs(vec![REG_R0, REG_R1]);
    let lir = builder.finish();

    check_lir(
        lir,
        &cfg,
        &block_order,
        expect![[r#"
            block0[%1:gpr($r0), %2:gpr($r1)]:
                %0:gpr(reg)[late] = Lea %1:gpr(reg)[early], %2:gpr(reg)[early]
                Ret %0:gpr($r0)[early]
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

    // `exit`
    let [retval] = push_instr(
        &mut builder,
        DummyInstr::Ret,
        [],
        [(UseOperandConstraint::Fixed(REG_R0), OperandPos::Early)],
    );
    builder.set_incoming_block_params([retval]);
    builder.finish_block();

    // `right`
    let add5 = builder.create_vreg(RC_GPR);
    builder.add_succ_outgoing_block_params([add5]);
    push_instr(&mut builder, DummyInstr::Jump(exit), [], []);
    let [right_param2, five] = push_instr(
        &mut builder,
        DummyInstr::Add,
        [DefOperand::new(
            add5,
            DefOperandConstraint::Any,
            OperandPos::Late,
        )],
        [
            (UseOperandConstraint::TiedToDef(0), OperandPos::Early),
            (UseOperandConstraint::AnyReg, OperandPos::Early),
        ],
    );
    push_instr(
        &mut builder,
        DummyInstr::MovI(5),
        [DefOperand::new(
            five,
            DefOperandConstraint::AnyReg,
            OperandPos::Late,
        )],
        [],
    );
    builder.finish_block();

    // `left`
    let add_params = builder.create_vreg(RC_GPR);
    builder.add_succ_outgoing_block_params([add_params]);
    push_instr(&mut builder, DummyInstr::Jump(exit), [], []);
    let [left_param2, left_param3] = push_instr(
        &mut builder,
        DummyInstr::Add,
        [DefOperand::new(
            add_params,
            DefOperandConstraint::Any,
            OperandPos::Late,
        )],
        [
            (UseOperandConstraint::TiedToDef(0), OperandPos::Early),
            (UseOperandConstraint::AnyReg, OperandPos::Early),
        ],
    );
    builder.finish_block();

    // `entry`
    builder.add_succ_outgoing_block_params([]);
    builder.add_succ_outgoing_block_params([]);
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
    builder.finish_block();
    builder.set_live_in_regs(vec![REG_R0, REG_R1, REG_R2]);
    let lir = builder.finish();

    check_lir(
        lir,
        &cfg,
        &block_order,
        expect![[r#"
            block0[%7:gpr($r0), %8:gpr($r1), %6:gpr($r2)]:
                Cmp %7:gpr(any)[early], %8:gpr(reg)[early]
                JmpEq(block1, block2)
            => block1[], block2[]
            block1[]:
                %4:gpr(any)[late] = Add %8:gpr(tied:0)[early], %6:gpr(reg)[early]
                Jump(block3)
            => block3[%4:gpr]
            block2[]:
                %3:gpr(reg)[late] = MovI(5)
                %1:gpr(any)[late] = Add %8:gpr(tied:0)[early], %3:gpr(reg)[early]
                Jump(block3)
            => block3[%1:gpr]
            block3[%0:gpr]:
                Ret %0:gpr($r0)[early]
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

    let [retval] = push_instr(
        &mut builder,
        DummyInstr::Ret,
        [],
        [(UseOperandConstraint::Fixed(REG_R0), OperandPos::Early)],
    );
    builder.set_incoming_block_params([retval]);
    builder.finish_block();

    let outgoing_param = builder.create_vreg(RC_GPR);
    builder.add_succ_outgoing_block_params([outgoing_param]);

    push_instr(&mut builder, DummyInstr::Jump(exit), [], []);

    let five = builder.create_vreg(RC_GPR);
    push_instr(
        &mut builder,
        DummyInstr::MovI(5),
        [DefOperand::new(
            five,
            DefOperandConstraint::AnyReg,
            OperandPos::Late,
        )],
        [],
    );

    builder.copy_vreg(outgoing_param, five);
    builder.finish_block();

    let lir = builder.finish();

    check_lir(
        lir,
        &cfg,
        &block_order,
        expect![[r#"
            block0[]:
                %2:gpr(reg)[late] = MovI(5)
                Jump(block1)
            => block1[%2:gpr]
            block1[%0:gpr]:
                Ret %0:gpr($r0)[early]
        "#]],
    );
}
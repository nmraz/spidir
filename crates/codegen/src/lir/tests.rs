use expect_test::{expect, Expect};

use super::{
    Builder, DefOperand, DefOperandConstraint, Lir, OperandPos, PhysReg, RegClass, RegNames,
    UseOperand, UseOperandConstraint, VirtReg,
};

#[derive(Debug, Clone, Copy)]
enum DummyInstr {
    Add,
    Lea,
    Ret,
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

fn check_lir(lir: Lir<DummyInstr>, expected: Expect) {
    expected.assert_eq(&lir.display(&DummyRegNames).to_string());
}

#[test]
fn build_simple_tied() {
    let mut builder = Builder::new();
    let [retval] = push_instr(
        &mut builder,
        DummyInstr::Ret,
        [],
        [(UseOperandConstraint::Fixed(REG_R0), OperandPos::Early)],
    );
    push_instr(
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
    let lir = builder.finish();

    check_lir(
        lir,
        expect![[r#"
            %0:gpr(any)[late] = Add %1:gpr(tied:0)[early], %2:gpr(reg)[early]
            Ret %0:gpr($r0)[early]
        "#]],
    );
}

#[test]
fn build_simple_3addr() {
    let mut builder = Builder::new();
    let [retval] = push_instr(
        &mut builder,
        DummyInstr::Ret,
        [],
        [(UseOperandConstraint::Fixed(REG_R0), OperandPos::Early)],
    );
    push_instr(
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
    let lir = builder.finish();

    check_lir(
        lir,
        expect![[r#"
            %0:gpr(reg)[late] = Lea %1:gpr(reg)[early], %2:gpr(reg)[early]
            Ret %0:gpr($r0)[early]
        "#]],
    );
}

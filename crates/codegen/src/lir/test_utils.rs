use cranelift_entity::packed_option::ReservedValue;

use crate::{
    cfg::Block,
    lir::{RegBank, RegWidth},
    machine::MachineCore,
};

use super::{
    Builder, DefOperand, OperandPos, PhysReg, PhysRegSet, RegClass, UseOperand,
    UseOperandConstraint, VirtReg,
};

// The compiler (quite sensibly) considers fields used only by the `Debug` impl as "never read", so
// shut it up manually.
#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum DummyInstr {
    MovI(u64),
    Add,
    Lea,
    Cmp,
    Ret,
    Call,
    Jump(Block),
    JmpEq(Block, Block),
}

pub const RW_FULL: RegWidth = RegWidth::new(0);

pub const RB_GPR: RegBank = RegBank::new(0);
pub const RC_GPR: RegClass = RegClass::new(RB_GPR, RW_FULL);

pub const REG_R0: PhysReg = PhysReg::new(0);
pub const REG_R1: PhysReg = PhysReg::new(1);
pub const REG_R2: PhysReg = PhysReg::new(2);

pub struct DummyMachine;
impl MachineCore for DummyMachine {
    type Instr = DummyInstr;

    fn reg_class_name(class: RegClass) -> &'static str {
        match class {
            RC_GPR => "gpr",
            _ => unreachable!(),
        }
    }

    fn reg_name(reg: PhysReg) -> &'static str {
        match reg {
            REG_R0 => "r0",
            REG_R1 => "r1",
            REG_R2 => "r2",
            _ => unreachable!(),
        }
    }
}

pub fn push_instr_with_clobbers<M: MachineCore, const U: usize>(
    builder: &mut Builder<'_, M>,
    instr: M::Instr,
    defs: impl IntoIterator<Item = DefOperand>,
    uses: [(UseOperandConstraint, OperandPos); U],
    clobbers: PhysRegSet,
) -> [VirtReg; U] {
    let mut use_regs = [VirtReg::reserved_value(); U];
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
            clobbers,
        );
    });
    use_regs
}

pub fn push_instr<M: MachineCore, const U: usize>(
    builder: &mut Builder<M>,
    instr: M::Instr,
    defs: impl IntoIterator<Item = DefOperand>,
    uses: [(UseOperandConstraint, OperandPos); U],
) -> [VirtReg; U] {
    push_instr_with_clobbers(builder, instr, defs, uses, PhysRegSet::empty())
}

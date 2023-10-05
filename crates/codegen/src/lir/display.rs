use core::fmt;

use itertools::Itertools;

use super::{
    DefOperand, DefOperandConstraint, Instr, Lir, PhysReg, RegClass, UseOperand,
    UseOperandConstraint, VirtReg,
};

pub trait RegNames {
    fn reg_class_name(&self, class: RegClass) -> &str;
    fn reg_name(&self, reg: PhysReg) -> &str;
}

pub struct DisplayVirtReg<'a> {
    pub(super) reg: VirtReg,
    pub(super) reg_names: &'a dyn RegNames,
}

impl fmt::Display for DisplayVirtReg<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "%{}:{}",
            self.reg.reg_num(),
            self.reg_names.reg_class_name(self.reg.class())
        )
    }
}

pub struct DisplayUseOperand<'a> {
    pub(super) operand: &'a UseOperand,
    pub(super) reg_names: &'a dyn RegNames,
}

impl fmt::Display for DisplayUseOperand<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.operand.reg.display(self.reg_names))?;
        match self.operand.constraint {
            UseOperandConstraint::Any => f.write_str("(any)")?,
            UseOperandConstraint::AnyReg => f.write_str("(reg)")?,
            UseOperandConstraint::Fixed(reg) => write!(f, "(${})", self.reg_names.reg_name(reg))?,
            UseOperandConstraint::TiedToDef(def) => write!(f, "(tied:{def})")?,
        }
        write!(f, "[{}]", self.operand.pos)
    }
}

pub struct DisplayDefOperand<'a> {
    pub(super) operand: &'a DefOperand,
    pub(super) reg_names: &'a dyn RegNames,
}

impl fmt::Display for DisplayDefOperand<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.operand.reg.display(self.reg_names))?;
        match self.operand.constraint {
            DefOperandConstraint::Any => f.write_str("(any)")?,
            DefOperandConstraint::AnyReg => f.write_str("(reg)")?,
            DefOperandConstraint::Fixed(reg) => write!(f, "(${})", self.reg_names.reg_name(reg))?,
        }
        write!(f, "[{}]", self.operand.pos)
    }
}

pub struct DisplayInstr<'a, I> {
    pub(super) lir: &'a Lir<I>,
    pub(super) reg_names: &'a dyn RegNames,
    pub(super) instr: Instr,
}

impl<I: fmt::Debug> fmt::Display for DisplayInstr<'_, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let defs = self.lir.instr_defs(self.instr);
        if !defs.is_empty() {
            write!(
                f,
                "{}",
                defs.iter()
                    .format_with(", ", |def, f| { f(&def.display(self.reg_names)) })
            )?;
            f.write_str(" = ")?;
        }

        write!(f, "{:?}", self.lir.instr_data(self.instr))?;

        let uses = self.lir.instr_uses(self.instr);
        if !uses.is_empty() {
            f.write_str(" ")?;
            write!(
                f,
                "{}",
                uses.iter()
                    .format_with(", ", |use_op, f| { f(&use_op.display(self.reg_names)) })
            )?;
        }

        Ok(())
    }
}

pub struct Display<'a, I> {
    pub(super) lir: &'a Lir<I>,
    pub(super) reg_names: &'a dyn RegNames,
}

impl<I: fmt::Debug> fmt::Display for Display<'_, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: include block labels/params here.
        for i in 0..self.lir.instrs.len() {
            writeln!(
                f,
                "{}",
                self.lir.display_instr(self.reg_names, Instr::from_usize(i))
            )?;
        }

        Ok(())
    }
}

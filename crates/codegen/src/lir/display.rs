use core::fmt;

use itertools::Itertools;

use crate::cfg::{Block, BlockCfg};

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
    pub(super) cfg: &'a BlockCfg,
    pub(super) block_order: &'a [Block],
    pub(super) reg_names: &'a dyn RegNames,
}

impl<I: fmt::Debug> fmt::Display for Display<'_, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for &block in self.block_order {
            writeln!(
                f,
                "{block}[{}]:",
                self.lir
                    .block_params(block)
                    .iter()
                    .format_with(", ", |param, f| f(&param.display(self.reg_names)))
            )?;
            for instr in self.lir.block_instrs(block) {
                writeln!(f, "    {}", self.lir.display_instr(self.reg_names, instr))?;
            }
            let succs = self.cfg.block_succs(block);
            if !succs.is_empty() {
                write!(
                    f,
                    "=> {}",
                    succs.iter().enumerate().format_with(", ", |(i, succ), f| {
                        f(&format_args!(
                            "{succ}[{}]",
                            self.lir
                                .outgoing_block_params(block, i as u32)
                                .iter()
                                .format_with(", ", |param, f| f(&param.display(self.reg_names)))
                        ))
                    })
                )?;
            }
        }

        Ok(())
    }
}

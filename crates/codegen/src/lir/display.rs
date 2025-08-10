use core::{fmt, marker::PhantomData};

use itertools::Itertools;

use crate::{
    cfg::{Block, BlockCfg},
    machine::MachineCore,
};

use super::{
    DefOperand, DefOperandConstraint, Instr, Lir, PhysRegSet, RegBank, UseOperand,
    UseOperandConstraint, VirtReg,
};

pub struct DisplayVirtRegWithBank<M> {
    pub(super) reg: VirtReg,
    pub(super) bank: RegBank,
    pub(super) _marker: PhantomData<M>,
}

impl<M: MachineCore> fmt::Display for DisplayVirtRegWithBank<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.reg, M::reg_bank_name(self.bank))
    }
}

pub struct DisplayUseOperand<'a, M> {
    pub(super) operand: &'a UseOperand,
    pub(super) _marker: PhantomData<M>,
}

impl<M: MachineCore> fmt::Display for DisplayUseOperand<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.operand.reg())?;
        match self.operand.constraint {
            UseOperandConstraint::Any => f.write_str("(any)")?,
            UseOperandConstraint::AnyReg => f.write_str("(reg)")?,
            UseOperandConstraint::Fixed(reg) => write!(f, "(${})", M::reg_name(reg))?,
            UseOperandConstraint::TiedToDef(def) => write!(f, "(tied:{def})")?,
            UseOperandConstraint::SoftTiedToDef(def) => write!(f, "(soft-tied:{def})")?,
        }
        write!(f, "[{}]", self.operand.pos)
    }
}

pub struct DisplayDefOperand<'a, M: MachineCore> {
    pub(super) lir: &'a Lir<M>,
    pub(super) operand: &'a DefOperand,
    pub(super) _marker: PhantomData<M>,
}

impl<M: MachineCore> fmt::Display for DisplayDefOperand<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let vreg = self.operand.reg();
        write!(f, "{}", self.lir.display_vreg_with_bank(vreg))?;
        match self.operand.constraint {
            DefOperandConstraint::Any => f.write_str("(any)")?,
            DefOperandConstraint::AnyReg => f.write_str("(reg)")?,
            DefOperandConstraint::Fixed(reg) => write!(f, "(${})", M::reg_name(reg))?,
        }
        write!(f, "[{}]", self.operand.pos)
    }
}

pub struct DisplayInstrData<'a, M: MachineCore> {
    pub(super) lir: &'a Lir<M>,
    pub(super) instr: M::Instr,
    pub(super) defs: &'a [DefOperand],
    pub(super) uses: &'a [UseOperand],
    pub(super) clobbers: PhysRegSet,
}

impl<M: MachineCore> fmt::Display for DisplayInstrData<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.defs.is_empty() {
            write!(
                f,
                "{}",
                self.defs
                    .iter()
                    .format_with(", ", |def, f| { f(&def.display(self.lir)) })
            )?;
            f.write_str(" = ")?;
        }

        write!(f, "{:?}", self.instr)?;

        if !self.uses.is_empty() {
            f.write_str(" ")?;
            write!(
                f,
                "{}",
                self.uses
                    .iter()
                    .format_with(", ", |use_op, f| { f(&use_op.display::<M>()) })
            )?;
        }

        if !self.clobbers.is_empty() {
            write!(
                f,
                " ^({})",
                self.clobbers
                    .iter()
                    .format_with(", ", |clobber, f| f(&format_args!(
                        "${}",
                        M::reg_name(clobber)
                    )))
            )?;
        }

        Ok(())
    }
}

pub(super) fn display_block_params<'a, M: MachineCore>(
    lir: &'a Lir<M>,
    block_params: &'a [VirtReg],
) -> DisplayBlockParams<'a, M> {
    DisplayBlockParams {
        lir,
        block_params,
        _marker: PhantomData,
    }
}

pub struct DisplayBlockParams<'a, M: MachineCore> {
    lir: &'a Lir<M>,
    block_params: &'a [VirtReg],
    _marker: PhantomData<M>,
}

impl<M: MachineCore> fmt::Display for DisplayBlockParams<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.block_params.is_empty() {
            write!(
                f,
                "[{}]",
                self.block_params.iter().format_with(", ", |&param, f| {
                    f(&self.lir.display_vreg_with_bank(param))
                })
            )?;
        }
        Ok(())
    }
}

const INSTR_NUM_DIGITS: usize = 4;
const INSTR_GUTTER_WIDTH: usize = INSTR_NUM_DIGITS + 2;

pub fn display_instr_gutter(instr: Instr) -> impl fmt::Display {
    // Note: we use a dedicated structure rather than `format_args!` because that macro takes all
    // its parameters by reference.
    struct DisplayInstrGutter(Instr);
    impl fmt::Display for DisplayInstrGutter {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:01$}: ", self.0.as_u32(), INSTR_NUM_DIGITS)
        }
    }
    DisplayInstrGutter(instr)
}

pub fn display_instr_gutter_padding() -> impl fmt::Display {
    struct InstrGutterPadding;
    impl fmt::Display for InstrGutterPadding {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:1$}", "", INSTR_GUTTER_WIDTH)
        }
    }
    InstrGutterPadding
}

pub struct Display<'a, M: MachineCore> {
    pub(super) lir: &'a Lir<M>,
    pub(super) cfg: &'a BlockCfg,
    pub(super) block_order: &'a [Block],
}

impl<M: MachineCore> fmt::Display for Display<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (stack_slot, layout) in self.lir.stack_slots.iter() {
            writeln!(
                f,
                "{} = stackslot {}:{}",
                stack_slot, layout.size, layout.align
            )?;
        }

        let mut first_block = true;

        for &block in self.block_order {
            if first_block {
                writeln!(
                    f,
                    "{}{block}[{}]:",
                    display_instr_gutter_padding(),
                    self.lir
                        .block_params(block)
                        .iter()
                        .zip(&self.lir.live_in_regs)
                        .format_with(", ", |(&param, reg), f| {
                            f(&format_args!(
                                "{}(${})",
                                self.lir.display_vreg_with_bank(param),
                                M::reg_name(*reg)
                            ))
                        }),
                )?;
            } else {
                writeln!(
                    f,
                    "{}{block}{}:",
                    display_instr_gutter_padding(),
                    display_block_params(self.lir, self.lir.block_params(block)),
                )?;
            }
            first_block = false;

            for instr in self.lir.block_instrs(block) {
                writeln!(
                    f,
                    "{}     {}",
                    display_instr_gutter(instr),
                    self.lir.display_instr(instr)
                )?;
            }

            let succs = self.cfg.block_succs(block);
            if let &[succ] = succs {
                // If we have a single successor, there might be outgoing block params.
                writeln!(
                    f,
                    "{}=> {}{}",
                    display_instr_gutter_padding(),
                    succ,
                    display_block_params(self.lir, self.lir.outgoing_block_params(block))
                )?;
            } else if !succs.is_empty() {
                // If there are multiple successors, just list them all.
                writeln!(
                    f,
                    "{}=> {}",
                    display_instr_gutter_padding(),
                    succs.iter().format(", ")
                )?;
            }
        }

        Ok(())
    }
}

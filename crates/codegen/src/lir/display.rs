use core::{fmt, marker::PhantomData};

use itertools::Itertools;

use crate::{
    cfg::{Block, BlockCfg},
    machine::MachineCore,
};

use super::{
    DefOperand, DefOperandConstraint, Instr, Lir, PhysRegSet, UseOperand, UseOperandConstraint,
    VirtReg,
};

pub struct DisplayVirtReg<M> {
    pub(super) reg: VirtReg,
    pub(super) _marker: PhantomData<M>,
}

impl<M: MachineCore> fmt::Display for DisplayVirtReg<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}",
            self.reg.reg_num(),
            M::reg_class_name(self.reg.class())
        )
    }
}

pub struct DisplayUseOperand<'a, M> {
    pub(super) operand: &'a UseOperand,
    pub(super) _marker: PhantomData<M>,
}

impl<M: MachineCore> fmt::Display for DisplayUseOperand<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.operand.reg.reg_num())?;
        match self.operand.constraint {
            UseOperandConstraint::Any => f.write_str("(any)")?,
            UseOperandConstraint::AnyReg => f.write_str("(reg)")?,
            UseOperandConstraint::Fixed(reg) => write!(f, "(${})", M::reg_name(reg))?,
            UseOperandConstraint::TiedToDef(def) => write!(f, "(tied:{def})")?,
        }
        write!(f, "[{}]", self.operand.pos)
    }
}

pub struct DisplayDefOperand<'a, M> {
    pub(super) operand: &'a DefOperand,
    pub(super) _marker: PhantomData<M>,
}

impl<M: MachineCore> fmt::Display for DisplayDefOperand<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.operand.reg.display::<M>())?;
        match self.operand.constraint {
            DefOperandConstraint::Any => f.write_str("(any)")?,
            DefOperandConstraint::AnyReg => f.write_str("(reg)")?,
            DefOperandConstraint::Fixed(reg) => write!(f, "(${})", M::reg_name(reg))?,
        }
        write!(f, "[{}]", self.operand.pos)
    }
}

pub fn display_instr_data<'a, M: MachineCore>(
    instr: M::Instr,
    defs: &'a [DefOperand],
    uses: &'a [UseOperand],
    clobbers: PhysRegSet,
) -> DisplayInstrData<'a, M> {
    DisplayInstrData {
        instr,
        defs,
        uses,
        clobbers,
    }
}

pub struct DisplayInstrData<'a, M: MachineCore> {
    instr: M::Instr,
    defs: &'a [DefOperand],
    uses: &'a [UseOperand],
    clobbers: PhysRegSet,
}

impl<M: MachineCore> fmt::Display for DisplayInstrData<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.defs.is_empty() {
            write!(
                f,
                "{}",
                self.defs
                    .iter()
                    .format_with(", ", |def, f| { f(&def.display::<M>()) })
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

pub struct DisplayInstr<'a, M: MachineCore> {
    pub(super) lir: &'a Lir<M>,
    pub(super) instr: Instr,
}

impl<M: MachineCore> fmt::Display for DisplayInstr<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            display_instr_data::<M>(
                *self.lir.instr_data(self.instr),
                self.lir.instr_defs(self.instr),
                self.lir.instr_uses(self.instr),
                self.lir.instr_clobbers(self.instr),
            )
        )
    }
}

pub fn display_block_params<M: MachineCore>(block_params: &[VirtReg]) -> DisplayBlockParams<'_, M> {
    DisplayBlockParams {
        block_params,
        _marker: PhantomData,
    }
}

pub struct DisplayBlockParams<'a, M: MachineCore> {
    block_params: &'a [VirtReg],
    _marker: PhantomData<M>,
}

impl<M: MachineCore> fmt::Display for DisplayBlockParams<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.block_params.is_empty() {
            write!(
                f,
                "[{}]",
                self.block_params
                    .iter()
                    .format_with(", ", |param, f| f(&param.display::<M>()))
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
        for (stack_slot, data) in self.lir.stack_slots.iter() {
            writeln!(f, "{} = {:?}", stack_slot, data)?;
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
                        .format_with(", ", |(param, reg), f| f(&format_args!(
                            "{}(${})",
                            param.display::<M>(),
                            M::reg_name(*reg)
                        ))),
                )?;
            } else {
                writeln!(
                    f,
                    "{}{block}{}:",
                    display_instr_gutter_padding(),
                    display_block_params::<M>(self.lir.block_params(block)),
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
                    display_block_params::<M>(self.lir.outgoing_block_params(block))
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

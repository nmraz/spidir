use core::{
    fmt::{self, Display},
    iter,
};

use itertools::Itertools;
use log::info;

use crate::{
    cfg::Block,
    lir::{
        display::{display_instr_gutter, display_instr_gutter_padding},
        Lir,
    },
    machine::MachineCore,
};

use super::{Assignment, InstrOrCopy, OperandAssignment};

pub struct DisplayAssignment<'a, M: MachineCore> {
    pub(super) assignment: &'a Assignment,
    pub(super) lir: &'a Lir<M>,
    pub(super) block_order: &'a [Block],
}

impl<M: MachineCore> fmt::Display for DisplayAssignment<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for &block in self.block_order {
            info!("{block}");
            writeln!(f, "{}{block}:", display_instr_gutter_padding())?;

            for instr in self
                .assignment
                .instrs_and_copies(self.lir.block_instrs(block))
            {
                match instr {
                    InstrOrCopy::Instr(instr) => {
                        write!(f, "{}    ", display_instr_gutter(instr))?;
                        let defs = self.assignment.instr_def_assignments(instr);
                        if !defs.is_empty() {
                            write!(f, "{} = ", format_op_assignments::<M>(defs.iter().copied()))?;
                        }
                        write!(f, "{:?}", self.lir.instr_data(instr))?;
                        let uses = self.assignment.instr_use_assignments(instr);
                        if !uses.is_empty() {
                            write!(f, " {}", format_op_assignments::<M>(uses.iter().copied()))?;
                        }
                    }
                    InstrOrCopy::Copy(copy) => {
                        write!(f, "{}    ", display_instr_gutter_padding())?;
                        write_op::<M>(f, copy.to)?;
                        write!(f, " = ")?;
                        write_op::<M>(f, copy.from)?;
                    }
                }
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

fn format_op_assignments<M: MachineCore>(
    op_assignments: impl Iterator<Item = OperandAssignment>,
) -> impl Display {
    op_assignments.format_with(", ", |op, f| match op {
        OperandAssignment::Reg(reg) => f(&format_args!("${}", M::reg_name(reg))),
        OperandAssignment::Spill(slot) => f(&format_args!("${}", slot)),
    })
}

fn write_op<M: MachineCore>(f: &mut fmt::Formatter<'_>, op: OperandAssignment) -> fmt::Result {
    write!(f, "{}", format_op_assignments::<M>(iter::once(op)))
}

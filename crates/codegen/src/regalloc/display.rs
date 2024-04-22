use core::fmt::{self, Display};

use itertools::Itertools;

use crate::{
    cfg::Block,
    lir::{
        display::{display_instr_gutter, display_instr_gutter_padding},
        Lir,
    },
    machine::MachineCore,
};

use super::{Assignment, OperandAssignment};

pub struct DisplayAssignment<'a, M: MachineCore> {
    pub(super) assignment: &'a Assignment,
    pub(super) lir: &'a Lir<M>,
    pub(super) block_order: &'a [Block],
}

impl<M: MachineCore> fmt::Display for DisplayAssignment<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for &block in self.block_order {
            writeln!(f, "{}{block}:", display_instr_gutter_padding())?;
            for instr in self.lir.block_instrs(block) {
                write!(f, "{}    ", display_instr_gutter(instr))?;
                let defs = self.assignment.instr_def_assignments(instr);
                if !defs.is_empty() {
                    write!(f, "{} = ", format_op_assignments::<M>(defs))?;
                }
                write!(f, "{:?}", self.lir.instr_data(instr))?;
                let uses = self.assignment.instr_use_assignments(instr);
                if !uses.is_empty() {
                    write!(f, " {}", format_op_assignments::<M>(uses))?;
                }
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

fn format_op_assignments<M: MachineCore>(
    op_assignments: &[OperandAssignment],
) -> impl Display + '_ {
    op_assignments.iter().format_with(", ", |op, f| match op {
        &OperandAssignment::Reg(reg) => f(&format_args!("${}", M::reg_name(reg))),
        OperandAssignment::Spill(_slot) => {
            // TODO: use real stack slot value here
            f(&"$stack")
        }
    })
}

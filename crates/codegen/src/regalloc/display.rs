use core::{
    fmt::{self, Display},
    marker::PhantomData,
};

use itertools::Itertools;

use crate::{
    cfg::Block,
    lir::{
        display::{display_instr_gutter, display_instr_gutter_padding},
        Lir,
    },
    machine::MachineCore,
};

use super::{types::CopySourceAssignment, Assignment, InstrOrCopy, OperandAssignment};

pub struct DisplayOperandAssignment<M: MachineCore> {
    pub(super) assignment: OperandAssignment,
    pub(super) _marker: PhantomData<M>,
}

impl<M: MachineCore> fmt::Display for DisplayOperandAssignment<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.assignment {
            OperandAssignment::Reg(reg) => write!(f, "${}", M::reg_name(reg)),
            OperandAssignment::Spill(spill) => write!(f, "${spill}"),
        }
    }
}

pub struct DisplayAssignmentCopySource<'a, M: MachineCore> {
    pub(super) lir: &'a Lir<M>,
    pub(super) source: CopySourceAssignment,
}

impl<M: MachineCore> fmt::Display for DisplayAssignmentCopySource<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.source {
            CopySourceAssignment::Operand(assignment) => write!(f, "{}", assignment.display::<M>()),
            CopySourceAssignment::Remat(instr) => write!(f, "{:?}", self.lir.instr_data(instr)),
        }
    }
}

pub struct DisplayAssignment<'a, M: MachineCore> {
    pub(super) assignment: &'a Assignment,
    pub(super) lir: &'a Lir<M>,
    pub(super) block_order: &'a [Block],
}

impl<M: MachineCore> fmt::Display for DisplayAssignment<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for &block in self.block_order {
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
                        write!(
                            f,
                            "{} = {}",
                            copy.to.display::<M>(),
                            copy.from.display(self.lir)
                        )?;
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
    op_assignments.format_with(", ", |op, f| f(&op.display::<M>()))
}

use core::fmt::{self, Display};

use itertools::Itertools;

use crate::{
    cfg::Block,
    lir::{
        display::{display_instr_gutter, display_instr_gutter_padding},
        Instr, Lir,
    },
    machine::MachineCore,
};

use super::{types::AssignmentCopy, Assignment, OperandAssignment};

pub struct DisplayAssignment<'a, M: MachineCore> {
    pub(super) assignment: &'a Assignment,
    pub(super) lir: &'a Lir<M>,
    pub(super) block_order: &'a [Block],
}

impl<M: MachineCore> fmt::Display for DisplayAssignment<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut copies = &self.assignment.copies[..];

        for &block in self.block_order {
            writeln!(f, "{}{block}:", display_instr_gutter_padding())?;
            for instr in self.lir.block_instrs(block) {
                let instr_copies = consume_copies_for(&mut copies, instr);
                write_instr_copies::<M>(f, instr_copies)?;

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
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

fn write_instr_copies<M: MachineCore>(
    f: &mut fmt::Formatter<'_>,
    mut copies: &[AssignmentCopy],
) -> fmt::Result {
    loop {
        let Some(first) = copies.first() else {
            break;
        };

        let len = copies
            .iter()
            .position(|copy| copy.phase != first.phase)
            .unwrap_or(copies.len());
        let (cur_copies, next_copies) = copies.split_at(len);

        let cur_dests = cur_copies.iter().map(|copy| copy.to);
        let cur_srcs = cur_copies.iter().map(|copy| copy.from);

        writeln!(
            f,
            "{}    ({}) = ({})",
            display_instr_gutter_padding(),
            format_op_assignments::<M>(cur_dests),
            format_op_assignments::<M>(cur_srcs),
        )?;

        copies = next_copies;
    }

    Ok(())
}

fn consume_copies_for<'a>(copies: &mut &'a [AssignmentCopy], instr: Instr) -> &'a [AssignmentCopy] {
    let Some(len) = (*copies).iter().position(|copy| copy.instr != instr) else {
        return &[];
    };

    let (for_instr, after_instr) = copies.split_at(len);
    *copies = after_instr;
    for_instr
}

fn format_op_assignments<M: MachineCore>(
    op_assignments: impl Iterator<Item = OperandAssignment>,
) -> impl Display {
    op_assignments.format_with(", ", |op, f| match op {
        OperandAssignment::Reg(reg) => f(&format_args!("${}", M::reg_name(reg))),
        OperandAssignment::Spill(slot) => f(&format_args!("${}", slot)),
    })
}

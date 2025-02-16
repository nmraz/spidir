use cranelift_entity::{packed_option::ReservedValue, SecondaryMap};

use crate::{
    cfg::{Block, CfgContext},
    code_buffer::{CodeBlob, CodeBuffer, Label},
    lir::Lir,
    machine::MachineEmit,
    regalloc::{Assignment, CopySourceAssignment, InstrOrCopy, TaggedAssignmentCopy},
};

pub type BlockLabelMap = SecondaryMap<Block, Label>;

pub fn emit_code<M: MachineEmit>(
    lir: &Lir<M>,
    cfg_ctx: &CfgContext,
    assignment: &Assignment,
    machine: &M,
) -> CodeBlob {
    let mut state = machine.prepare_state(lir, assignment);

    let mut buffer = CodeBuffer::new();

    let mut block_labels = BlockLabelMap::with_default(Label::reserved_value());
    for &block in &cfg_ctx.block_order {
        let label = buffer.create_label();
        block_labels[block] = label;
    }

    machine.emit_prologue(&mut state, &mut buffer);

    for &block in &cfg_ctx.block_order {
        buffer.bind_label(block_labels[block]);

        for instr in assignment.instrs_and_copies(lir.block_instrs(block)) {
            match instr {
                InstrOrCopy::Instr(instr) => {
                    machine.emit_instr(
                        &mut state,
                        &mut buffer,
                        &block_labels,
                        lir.instr_data(instr),
                        assignment.instr_def_assignments(instr),
                        assignment.instr_use_assignments(instr),
                    );
                }
                InstrOrCopy::Copy(TaggedAssignmentCopy { copy, .. }) => match copy.from {
                    CopySourceAssignment::Operand(from) => {
                        machine.emit_copy(&mut state, &mut buffer, from, copy.to)
                    }
                    CopySourceAssignment::Remat(instr) => machine.emit_instr(
                        &mut state,
                        &mut buffer,
                        &block_labels,
                        lir.instr_data(instr),
                        &[copy.to],
                        &[],
                    ),
                },
            }
        }
    }

    buffer.finish()
}

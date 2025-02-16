use cranelift_entity::{packed_option::ReservedValue, SecondaryMap};

use crate::{
    cfg::{Block, CfgContext},
    code_buffer::{CodeBlob, CodeBuffer, Label},
    lir::Lir,
    machine::MachineEmit,
    regalloc::{Assignment, CopySourceAssignment, InstrOrCopy, TaggedAssignmentCopy},
};

pub type BlockLabelMap = SecondaryMap<Block, Label>;

pub struct EmitContext<'a, M: MachineEmit> {
    pub lir: &'a Lir<M>,
    pub cfg_ctx: &'a CfgContext,
    pub assignment: &'a Assignment,
    pub block_labels: &'a BlockLabelMap,
}

pub fn emit_code<M: MachineEmit>(
    lir: &Lir<M>,
    cfg_ctx: &CfgContext,
    assignment: &Assignment,
    machine: &M,
) -> CodeBlob {
    let mut buffer = CodeBuffer::new();

    let mut block_labels = BlockLabelMap::with_default(Label::reserved_value());
    for &block in &cfg_ctx.block_order {
        let label = buffer.create_label();
        block_labels[block] = label;
    }

    let ctx = EmitContext {
        lir,
        cfg_ctx,
        assignment,
        block_labels: &block_labels,
    };

    let mut state = machine.prepare_state(&ctx);
    machine.emit_prologue(&ctx, &mut state, &mut buffer);

    for &block in &cfg_ctx.block_order {
        buffer.bind_label(block_labels[block]);

        let ctx = EmitContext {
            lir,
            cfg_ctx,
            assignment,
            block_labels: &block_labels,
        };

        machine.prepare_block(&ctx, &mut state, block);

        for instr in assignment.instrs_and_copies(lir.block_instrs(block)) {
            match instr {
                InstrOrCopy::Instr(instr) => {
                    machine.emit_instr(
                        &ctx,
                        &mut state,
                        &mut buffer,
                        lir.instr_data(instr),
                        assignment.instr_def_assignments(instr),
                        assignment.instr_use_assignments(instr),
                    );
                }
                InstrOrCopy::Copy(TaggedAssignmentCopy { copy, .. }) => match copy.from {
                    CopySourceAssignment::Operand(from) => {
                        machine.emit_copy(&ctx, &mut state, &mut buffer, from, copy.to)
                    }
                    CopySourceAssignment::Remat(instr) => machine.emit_instr(
                        &ctx,
                        &mut state,
                        &mut buffer,
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

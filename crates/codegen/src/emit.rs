use cranelift_entity::{SecondaryMap, packed_option::ReservedValue};

use crate::{
    cfg::{Block, CfgContext},
    code_buffer::{CodeBlob, CodeBuffer, Label},
    lir::Lir,
    machine::MachineEmit,
    regalloc::{
        Assignment, CopySourceAssignment, InstrOrCopy, OperandAssignment, TaggedAssignmentCopy,
    },
};

pub type BlockLabelMap = SecondaryMap<Block, Label>;

pub struct EmitContext<'a, M: MachineEmit> {
    pub lir: &'a Lir<M>,
    pub cfg_ctx: &'a CfgContext,
    pub assignment: &'a Assignment,
    pub block_labels: &'a BlockLabelMap,
}

pub struct EmitInstrData<'a, M: MachineEmit> {
    pub instr: &'a M::Instr,
    pub defs: &'a [OperandAssignment],
    pub uses: &'a [OperandAssignment],
}

pub struct EmitCopyData {
    pub from: OperandAssignment,
    pub to: OperandAssignment,
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
                        instr,
                        &EmitInstrData {
                            instr: lir.instr_data(instr),
                            defs: assignment.instr_def_assignments(instr),
                            uses: assignment.instr_use_assignments(instr),
                        },
                        &mut state,
                        &mut buffer,
                    );
                }
                InstrOrCopy::Copy(TaggedAssignmentCopy { copy, instr }) => match copy.from {
                    CopySourceAssignment::Operand(from) => machine.emit_copy(
                        &ctx,
                        instr,
                        &EmitCopyData { from, to: copy.to },
                        &mut state,
                        &mut buffer,
                    ),
                    CopySourceAssignment::Remat(instr) => machine.emit_instr(
                        &ctx,
                        instr,
                        &EmitInstrData {
                            instr: lir.instr_data(instr),
                            defs: &[copy.to],
                            uses: &[],
                        },
                        &mut state,
                        &mut buffer,
                    ),
                },
            }
        }
    }

    buffer.finish()
}

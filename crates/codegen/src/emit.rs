use alloc::vec::Vec;
use cranelift_entity::{entity_impl, packed_option::ReservedValue, PrimaryMap, SecondaryMap};
use ir::node::FunctionRef;

use crate::{
    cfg::{Block, CfgContext},
    lir::Lir,
    machine::{FixupKind, MachineEmit},
    regalloc::{Assignment, InstrOrCopy},
};

// These types are intentionally completely machine-independent; we want the final `CodeBlob` to be
// type-erased to enable trait objects in the high-level API.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RelocKind(pub u8);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Reloc {
    pub kind: RelocKind,
    pub offset: u32,
    pub target: FunctionRef,
    pub addend: i64,
}

#[derive(Default, Clone)]
pub struct CodeBlob {
    pub code: Vec<u8>,
    pub relocs: Vec<Reloc>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Label(u32);
entity_impl!(Label, "l");

struct Fixup<F> {
    offset: u32,
    label: Label,
    kind: F,
}

pub struct CodeBuffer<M: MachineEmit> {
    bytes: Vec<u8>,
    labels: PrimaryMap<Label, Option<u32>>,
    fixups: Vec<Fixup<M::Fixup>>,
    relocs: Vec<Reloc>,
}

impl<M: MachineEmit> CodeBuffer<M> {
    pub fn new() -> Self {
        Self {
            bytes: Vec::new(),
            labels: PrimaryMap::new(),
            fixups: Vec::new(),
            relocs: Vec::new(),
        }
    }

    pub fn offset(&self) -> u32 {
        self.bytes.len().try_into().unwrap()
    }

    pub fn instr(&mut self, f: impl FnOnce(&mut InstrBuffer<'_>)) {
        f(&mut InstrBuffer {
            bytes: &mut self.bytes,
        });
    }

    pub fn instr_with_reloc(
        &mut self,
        instr_offset: u32,
        target: FunctionRef,
        addend: i64,
        kind: RelocKind,
        f: impl FnOnce(&mut InstrBuffer<'_>),
    ) {
        let offset = self.offset() + instr_offset;
        self.relocs.push(Reloc {
            kind,
            offset,
            target,
            addend,
        });
        f(&mut InstrBuffer {
            bytes: &mut self.bytes,
        });
        // We don't actually know when the reloc ends, but at the very least it shouldn't start
        // outside the emitted instruction.
        debug_assert!(offset <= self.offset());
    }

    pub fn instr_with_fixup(
        &mut self,
        instr_offset: u32,
        label: Label,
        kind: M::Fixup,
        f: impl FnOnce(&mut InstrBuffer<'_>),
    ) {
        let offset = self.offset() + instr_offset;
        self.fixups.push(Fixup {
            offset,
            label,
            kind,
        });
        let fixup_end_offset = offset + u32::try_from(kind.byte_size()).unwrap();
        f(&mut InstrBuffer {
            bytes: &mut self.bytes,
        });
        debug_assert!(fixup_end_offset <= self.offset());
    }

    pub fn create_label(&mut self) -> Label {
        self.labels.push(None)
    }

    pub fn bind_label(&mut self, label: Label) {
        assert!(self.labels[label].is_none(), "label already bound");
        self.labels[label] = Some(self.offset());
    }

    pub fn resolve_label(&self, label: Label) -> Option<u32> {
        self.labels[label]
    }

    pub fn finish(mut self) -> CodeBlob {
        // Resolve all outstanding fixups now that the final layout is known.
        for fixup in &self.fixups {
            let label_offset = self.resolve_label(fixup.label).expect("label not bound");

            let offset_usize = fixup.offset as usize;
            let size = fixup.kind.byte_size();

            fixup.kind.apply(
                fixup.offset,
                label_offset,
                &mut self.bytes[offset_usize..offset_usize + size],
            );
        }

        // Relocations should already be sorted by offset thanks to limited public API.

        CodeBlob {
            code: self.bytes,
            relocs: self.relocs,
        }
    }
}

impl<M: MachineEmit> Default for CodeBuffer<M> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct InstrBuffer<'a> {
    bytes: &'a mut Vec<u8>,
}

impl<'a> InstrBuffer<'a> {
    pub fn emit(&mut self, bytes: &[u8]) {
        self.bytes.extend_from_slice(bytes);
    }
}

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
                InstrOrCopy::Copy(copy) => {
                    machine.emit_copy(&mut state, &mut buffer, copy.from, copy.to);
                }
            }
        }
    }

    buffer.finish()
}

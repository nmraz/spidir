use alloc::vec::Vec;
use cranelift_entity::{entity_impl, packed_option::ReservedValue, PrimaryMap, SecondaryMap};
use ir::node::FunctionRef;
use smallvec::SmallVec;

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

struct LastBranch {
    start: u32,
}

pub struct CodeBuffer<M: MachineEmit> {
    bytes: Vec<u8>,
    labels: PrimaryMap<Label, Option<u32>>,
    fixups: Vec<Fixup<M::Fixup>>,
    relocs: Vec<Reloc>,
    last_branches: SmallVec<[LastBranch; 4]>,
    last_bound_labels: SmallVec<[Label; 4]>,
}

impl<M: MachineEmit> CodeBuffer<M> {
    pub fn new() -> Self {
        Self {
            bytes: Vec::new(),
            labels: PrimaryMap::new(),
            fixups: Vec::new(),
            relocs: Vec::new(),
            last_branches: SmallVec::new(),
            last_bound_labels: SmallVec::new(),
        }
    }

    pub fn offset(&self) -> u32 {
        self.bytes.len().try_into().unwrap()
    }

    pub fn instr(&mut self, f: impl FnOnce(&mut InstrBuffer<'_>)) {
        self.instr_raw(f);
        // This isn't a tracked branch.
        self.clear_branch_tracking();
    }

    pub fn instr_with_reloc(
        &mut self,
        target: FunctionRef,
        addend: i64,
        reloc_instr_offset: u32,
        reloc_kind: RelocKind,
        f: impl FnOnce(&mut InstrBuffer<'_>),
    ) {
        let offset = self.offset() + reloc_instr_offset;
        self.relocs.push(Reloc {
            kind: reloc_kind,
            offset,
            target,
            addend,
        });
        // This isn't a tracked branch, so use `instr` and not `instr_raw`.
        self.instr(f);
        // We don't actually know when the reloc ends, but at the very least it shouldn't start
        // outside the emitted instruction.
        debug_assert!(offset <= self.offset());
    }

    pub fn instr_with_fixup(
        &mut self,
        label: Label,
        fixup_instr_offset: u32,
        fixup_kind: M::Fixup,
        f: impl FnOnce(&mut InstrBuffer<'_>),
    ) {
        self.instr_with_fixup_raw(label, fixup_instr_offset, fixup_kind, f);
        // This isn't a tracked branch.
        self.clear_branch_tracking();
    }

    pub fn branch(
        &mut self,
        target: Label,
        fixup_instr_offset: u32,
        fixup_kind: M::Fixup,
        f: impl FnOnce(&mut InstrBuffer<'_>),
    ) {
        let start = self.offset();
        // Note: we're recording fixups and branches in lock step.
        self.instr_with_fixup_raw(target, fixup_instr_offset, fixup_kind, f);
        self.last_branches.push(LastBranch { start });
    }

    pub fn create_label(&mut self) -> Label {
        self.labels.push(None)
    }

    pub fn bind_label(&mut self, label: Label) {
        assert!(self.labels[label].is_none(), "label already bound");
        self.prune_branches_before_label(label);
        self.labels[label] = Some(self.offset());
        self.last_bound_labels.push(label);
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

    fn prune_branches_before_label(&mut self, label: Label) {
        let mut first_affected_bound_label = self.last_bound_labels.len();

        while let Some(last_branch) = self.last_branches.last() {
            // Every recorded last branch should always come with a corresponding fixup.
            let branch_target = self.fixups.last().unwrap().label;
            if branch_target == label {
                self.bytes.truncate(last_branch.start as usize);
                self.fixups.pop();
                self.last_branches.pop();

                // Now that we've removed previously emitted code, make sure to repair all labels
                // bound after it.
                let new_offset = self.offset();
                first_affected_bound_label = self.last_bound_labels[..first_affected_bound_label]
                    .partition_point(|&label| self.labels[label].unwrap() < new_offset);
            } else {
                break;
            }
        }

        // Retarget any labels we may have moved back.
        let offset = self.offset();
        for &label in &self.last_bound_labels[first_affected_bound_label..] {
            self.labels[label] = Some(offset);
        }
    }

    fn instr_raw(&mut self, f: impl FnOnce(&mut InstrBuffer<'_>)) {
        f(&mut InstrBuffer {
            bytes: &mut self.bytes,
        });
    }

    fn instr_with_fixup_raw(
        &mut self,
        label: Label,
        fixup_instr_offset: u32,
        fixup_kind: M::Fixup,
        f: impl FnOnce(&mut InstrBuffer<'_>),
    ) {
        let offset = self.offset() + fixup_instr_offset;
        self.fixups.push(Fixup {
            offset,
            label,
            kind: fixup_kind,
        });
        let fixup_end_offset = offset + u32::try_from(fixup_kind.byte_size()).unwrap();
        self.instr_raw(f);
        debug_assert!(fixup_end_offset <= self.offset());
    }

    fn clear_branch_tracking(&mut self) {
        self.last_branches.clear();
        self.last_bound_labels.clear();
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

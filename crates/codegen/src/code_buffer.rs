use core::{cmp, iter, mem};

use alloc::vec::Vec;

use cranelift_entity::{entity_impl, packed_option::PackedOption, PrimaryMap, SecondaryMap};
use ir::node::FunctionRef;
use smallvec::SmallVec;

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

pub trait FixupKind: Copy {
    fn byte_size(&self) -> usize;
    fn apply(&self, offset: u32, label_offset: u32, bytes: &mut [u8]);
}

pub struct InstrBuffer<'a> {
    bytes: &'a mut Vec<u8>,
}

impl<'a> InstrBuffer<'a> {
    pub fn emit(&mut self, bytes: &[u8]) {
        self.bytes.extend_from_slice(bytes);
    }
}

struct Fixup<F> {
    offset: u32,
    label: Label,
    kind: F,
}

struct LastBranch {
    start: u32,
    bound_label_end: u32,
    is_uncond: bool,
}

pub struct CodeBuffer<F: FixupKind> {
    bytes: Vec<u8>,
    labels: PrimaryMap<Label, Option<u32>>,
    threaded_branches: SecondaryMap<Label, PackedOption<Label>>,
    fixups: Vec<Fixup<F>>,
    relocs: Vec<Reloc>,
    last_branches: SmallVec<[LastBranch; 4]>,
    last_bound_labels: SmallVec<[Label; 16]>,
    first_unpinned_bound_label: usize,
}

impl<F: FixupKind> CodeBuffer<F> {
    pub fn new() -> Self {
        Self {
            bytes: Vec::new(),
            labels: PrimaryMap::new(),
            threaded_branches: SecondaryMap::new(),
            fixups: Vec::new(),
            relocs: Vec::new(),
            last_branches: SmallVec::new(),
            last_bound_labels: SmallVec::new(),
            first_unpinned_bound_label: 0,
        }
    }

    pub fn offset(&self) -> u32 {
        self.bytes.len().try_into().unwrap()
    }

    pub fn instr(&mut self, f: impl FnOnce(&mut InstrBuffer<'_>)) {
        // Forget all branch tracking information we currently have, we're about to emit a
        // non-branch.
        self.clear_branch_tracking();
        self.instr_raw(f);
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

    pub fn branch(
        &mut self,
        target: Label,
        fixup_instr_offset: u32,
        fixup_kind: F,
        f: impl FnOnce(&mut InstrBuffer<'_>),
    ) {
        self.branch_raw(false, target, fixup_instr_offset, fixup_kind, f);
    }

    pub fn uncond_branch(
        &mut self,
        target: Label,
        fixup_instr_offset: u32,
        fixup_kind: F,
        f: impl FnOnce(&mut InstrBuffer<'_>),
    ) {
        let last_was_uncond = self
            .last_branches
            .last()
            .is_some_and(|last_branch| last_branch.is_uncond);

        // Redirect any labels bound here to `target`, regardless of whether we end up being able to
        // remove the branch entirely.
        let threaded_all_labels = self.thread_last_bound_labels(target);

        // If the last instruction was a terminator (currently approximated by "unconditional
        // branch"), we can avoid the current branch completely if we manage to thread all incoming
        // branches to it onward.
        if last_was_uncond && threaded_all_labels {
            return;
        }

        self.branch_raw(true, target, fixup_instr_offset, fixup_kind, f);
    }

    pub fn create_label(&mut self) -> Label {
        self.labels.push(None)
    }

    pub fn bind_label(&mut self, label: Label) {
        assert!(self.labels[label].is_none(), "label already bound");

        // This offset might be temporary if we are inside a "last branch" area. The label may be
        // moved earlier and will be pinned at its final offset in `flush_tracked_branches`.
        self.labels[label] = Some(self.offset());
        self.last_bound_labels.push(label);
        self.prune_last_branches();
    }

    pub fn finish(mut self) -> CodeBlob {
        // Resolve all outstanding fixups now that the final layout is known.
        for fixup in mem::take(&mut self.fixups) {
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

    fn prune_last_branches(&mut self) {
        loop {
            let Some(last_branch) = self.last_branches.last() else {
                // If we've cleaned everything up, make sure to pin any remaining labels in place;
                // they can't move back if there are no branches to prune before them.
                self.flush_tracked_branches();
                break;
            };

            let branch_start = last_branch.start as usize;

            // Every recorded last branch should always come with a corresponding fixup.
            let branch_target = self.fixups.last().unwrap().label;

            let Some(branch_target) = self.resolve_label(branch_target) else {
                break;
            };

            // Any resolved targets that are at or above the current offset will end up exactly at
            // the current offset, so their branches can be pruned. We might get targets above the
            // current immediate offset because we haven't retargeted labels from previous
            // iterations/invocations yet.
            if branch_target < self.offset() {
                break;
            }

            self.bytes.truncate(branch_start);
            self.fixups.pop();
            self.last_branches.pop();
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
        fixup_kind: F,
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

    fn branch_raw(
        &mut self,
        is_uncond: bool,
        target: Label,
        fixup_instr_offset: u32,
        fixup_kind: F,
        f: impl FnOnce(&mut InstrBuffer<'_>),
    ) {
        let start = self.offset();

        // Note: we're recording fixups and branches in lock step.
        self.instr_with_fixup_raw(target, fixup_instr_offset, fixup_kind, f);
        let bound_label_end = self.last_bound_labels.len() as u32;
        self.last_branches.push(LastBranch {
            start,
            bound_label_end,
            is_uncond,
        });
    }

    fn resolve_label(&mut self, label: Label) -> Option<u32> {
        let final_label = self.resolve_threaded_target(label);
        self.labels[final_label]
    }

    fn resolve_threaded_target(&mut self, target: Label) -> Label {
        let Some(threaded_target) = self.threaded_branches[target].expand() else {
            // Don't do any more work if this target hasn't been threaded at all.
            return target;
        };

        let final_target = iter::successors(Some(threaded_target), |&label| {
            self.threaded_branches[label].expand()
        })
        .last()
        .unwrap();

        // Opportunistically compress the path we've just walked.
        self.threaded_branches[target] = final_target.into();
        final_target
    }

    fn thread_last_bound_labels(&mut self, target: Label) -> bool {
        let offset = self.offset();
        let final_target = self.resolve_threaded_target(target);

        if self.labels[final_target] == Some(offset) {
            // Don't alias a label to itself or to other labels bound at the same offset; doing so
            // could cause an infinite loop later.
            return false;
        }

        // Grab all recently bound labels pertaining to the current offset.
        let cur_label_base = self
            .last_bound_labels
            .partition_point(|&label| self.labels[label].unwrap() != offset);
        let cur_labels = &self.last_bound_labels[cur_label_base..];

        for &cur_label in cur_labels {
            self.threaded_branches[cur_label] = final_target.into();
        }

        // Completely forget about the redirected labels now that they don't really point to the
        // current offset anymore.
        self.last_bound_labels.truncate(cur_label_base);
        self.first_unpinned_bound_label = cmp::min(self.first_unpinned_bound_label, cur_label_base);

        true
    }

    fn clear_branch_tracking(&mut self) {
        self.flush_tracked_branches();
        self.last_bound_labels.clear();
        self.first_unpinned_bound_label = 0;
    }

    fn flush_tracked_branches(&mut self) {
        let mut first_unpinned_bound_label = self.first_unpinned_bound_label;

        // Finalize any branches that are still "trapped" and shift any labels pointing to them
        // to their final targets.
        for branch in &self.last_branches {
            let bound_label_end = branch.bound_label_end as usize;
            for &label in &self.last_bound_labels[first_unpinned_bound_label..bound_label_end] {
                move_label_back(&mut self.labels, label, branch.start);
            }
            first_unpinned_bound_label = bound_label_end;
        }

        self.last_branches.clear();

        // Any remaining trailing labels just need to be moved back to the current offset after
        // pruning.
        let cur_offset = self.offset();
        for &label in &self.last_bound_labels[first_unpinned_bound_label..] {
            move_label_back(&mut self.labels, label, cur_offset);
        }

        // Remember that all existing labels have now been pinned: they can't be moved back any
        // longer, but we still want them around for branch threading.
        self.first_unpinned_bound_label = self.last_bound_labels.len();
    }
}

impl<F: FixupKind> Default for CodeBuffer<F> {
    fn default() -> Self {
        Self::new()
    }
}

fn move_label_back(labels: &mut PrimaryMap<Label, Option<u32>>, label: Label, offset: u32) {
    debug_assert!(labels[label].unwrap() >= offset);
    labels[label] = Some(offset);
}

#[cfg(test)]
mod tests;

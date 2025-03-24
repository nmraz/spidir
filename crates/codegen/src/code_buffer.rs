use core::{cmp, iter, mem};

use alloc::vec::Vec;

use cranelift_entity::{PrimaryMap, SecondaryMap, entity_impl, packed_option::PackedOption};
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

pub struct InstrSink<'a> {
    bytes: &'a mut Vec<u8>,
}

impl InstrSink<'_> {
    pub fn emit(&mut self, bytes: &[u8]) {
        self.bytes.extend_from_slice(bytes);
    }
}

struct Fixup<F> {
    offset: u32,
    label: Label,
    kind: F,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum LastBranchKind {
    Uncond,
    Cond { reversed_start: u32 },
}

struct LastBranch {
    start: u32,
    bound_label_end: u32,
    kind: LastBranchKind,
}

pub struct CodeBuffer<F: FixupKind> {
    bytes: Vec<u8>,
    reversed_cond_branch_bytes: SmallVec<[u8; 16]>,
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
            reversed_cond_branch_bytes: SmallVec::new(),
            labels: PrimaryMap::new(),
            threaded_branches: SecondaryMap::new(),
            fixups: Vec::new(),
            relocs: Vec::new(),
            last_branches: SmallVec::new(),
            last_bound_labels: SmallVec::new(),
            first_unpinned_bound_label: 0,
        }
    }

    pub fn instr(&mut self, f: impl FnOnce(&mut InstrSink<'_>)) {
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
        f: impl FnOnce(&mut InstrSink<'_>),
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

    pub fn cond_branch(
        &mut self,
        target: Label,
        fixup_instr_offset: u32,
        fixup_kind: F,
        emit_normal: impl FnOnce(&mut InstrSink<'_>),
        emit_reversed: impl FnOnce(&mut InstrSink<'_>),
    ) {
        let tmp_reversed_start = self.bytes.len();
        emit_reversed(&mut InstrSink {
            bytes: &mut self.bytes,
        });

        // Stash the reversed form for when we need it.
        let reversed_start: u32 = self.reversed_cond_branch_bytes.len().try_into().unwrap();
        self.reversed_cond_branch_bytes
            .extend_from_slice(&self.bytes[tmp_reversed_start..]);
        self.bytes.truncate(tmp_reversed_start);
        let reversed_end: u32 = self.reversed_cond_branch_bytes.len().try_into().unwrap();

        let normal_start = self.offset();
        self.branch_raw(
            LastBranchKind::Cond { reversed_start },
            target,
            fixup_instr_offset,
            fixup_kind,
            emit_normal,
        );
        let normal_end = self.offset();

        // Make sure the normal and reversed branches are exactly the same size, as we'll make use
        // of this fact if we need to reverse the branch.
        assert!(normal_end - normal_start == reversed_end - reversed_start);
    }

    pub fn uncond_branch(
        &mut self,
        target: Label,
        fixup_instr_offset: u32,
        fixup_kind: F,
        f: impl FnOnce(&mut InstrSink<'_>),
    ) {
        let last_branch_was_uncond = self
            .last_branches
            .last()
            .is_some_and(|last_branch| last_branch.kind == LastBranchKind::Uncond);

        // Redirect any labels bound here to `target`, regardless of whether we end up being able to
        // remove the branch entirely.
        let threaded_all_labels = self.thread_last_bound_labels(target);

        // If the last instruction was a terminator (currently approximated by "unconditional
        // branch"), we can avoid the current branch completely if we manage to thread all incoming
        // branches to it onward.
        if last_branch_was_uncond && threaded_all_labels {
            return;
        }

        self.branch_raw(
            LastBranchKind::Uncond,
            target,
            fixup_instr_offset,
            fixup_kind,
            f,
        );
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

    fn instr_raw(&mut self, f: impl FnOnce(&mut InstrSink<'_>)) {
        f(&mut InstrSink {
            bytes: &mut self.bytes,
        });
    }

    fn instr_with_fixup_raw(
        &mut self,
        label: Label,
        fixup_instr_offset: u32,
        fixup_kind: F,
        f: impl FnOnce(&mut InstrSink<'_>),
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
        kind: LastBranchKind,
        target: Label,
        fixup_instr_offset: u32,
        fixup_kind: F,
        f: impl FnOnce(&mut InstrSink<'_>),
    ) {
        let start = self.offset();

        // Note: we're recording fixups and branches in lock step.
        self.instr_with_fixup_raw(target, fixup_instr_offset, fixup_kind, f);
        let bound_label_end = self.last_bound_labels.len() as u32;
        self.last_branches.push(LastBranch {
            start,
            bound_label_end,
            kind,
        });
    }

    fn prune_last_branches(&mut self) {
        loop {
            let Some((_, last_branch_kind, last_branch_target)) = self.get_last_nth_branch(1)
            else {
                // If we've cleaned everything up, make sure to pin any remaining labels in place;
                // they can't move back if there are no branches to prune before them.
                self.flush_tracked_branches();
                break;
            };

            if self.is_branch_target_here(last_branch_target) {
                // This is a branch to the instruction immediately succeeding it, remove it.
                self.prune_last_branch();
                continue;
            }

            // Try to identify the following pattern:
            //
            //    L1:  C L3
            //    L2:  U Lx  <--
            //    L3: .....
            //
            // where C is a conditional branch and U is an unconditional branch. When we do, we can
            // simplify it to:
            //
            //    L1: !C Lx  <--
            //    L3: .....
            //
            // where !C has its condition reversed relative to C.

            if last_branch_kind != LastBranchKind::Uncond {
                // We need an unconditional branch last...
                break;
            }

            let Some((
                cond_branch_start,
                LastBranchKind::Cond {
                    reversed_start: cond_branch_reversed_start,
                },
                cond_branch_target,
            )) = self.get_last_nth_branch(2)
            else {
                // ...with a conditional branch immediately preceding it...
                break;
            };

            if !self.is_branch_target_here(cond_branch_target) {
                // ...that jumps to right after the unconditional branch.
                break;
            }

            // Start by getting the unconditional branch out of the way.
            self.prune_last_branch();

            let cond_branch_end = self.bytes.len();
            let cond_branch_len = cond_branch_end - cond_branch_start;
            let cond_branch_reversed_start = cond_branch_reversed_start as usize;
            let cond_branch_reversed_end = cond_branch_reversed_start + cond_branch_len;

            // Swap
            //
            //     reversed_cond_branch_bytes[cond_branch_reversed_start..]
            //
            // and
            //
            //     bytes[cond_branch_start..]
            //
            // by using the end of `bytes` as a temporary buffer.

            self.bytes.extend_from_slice(
                &self.reversed_cond_branch_bytes
                    [cond_branch_reversed_start..cond_branch_reversed_end],
            );
            self.reversed_cond_branch_bytes[cond_branch_reversed_start..cond_branch_reversed_end]
                .copy_from_slice(&self.bytes[cond_branch_start..cond_branch_end]);
            self.bytes.drain(cond_branch_start..cond_branch_end);

            // Retarget the conditional branch to the original unconditional branch's label. Note
            // that the conditional branch has the last fixup here because we've already pruned the
            // unconditional branch.
            self.fixups.last_mut().unwrap().label = last_branch_target;
        }
    }

    fn is_branch_target_here(&mut self, target: Label) -> bool {
        // Any resolved targets that are at or above the current offset will end up exactly at the
        // current offset. We might get targets above the current immediate offset if we haven't yet
        // retargeted labels from previous prune iterations.
        self.resolve_label(target)
            .is_some_and(|offset| offset >= self.offset())
    }

    fn prune_last_branch(&mut self) {
        let last_branch = self.last_branches.pop().unwrap();
        self.bytes.truncate(last_branch.start as usize);
        self.fixups.pop();

        // Note: we don't actually need to clean up `reversed_cond_branch_bytes` here since every
        // conditional branch knows its exact range inside it anyway. Pruning of conditional
        // branches should be rare enough anyway to make the occasional instruction's worth of
        // wasted space unnoticeable.

        // In any event, `reversed_cond_branch_bytes` will always be cleaned up in
        // `flush_tracked_branches`.
    }

    fn get_last_nth_branch(&self, n: usize) -> Option<(usize, LastBranchKind, Label)> {
        if self.last_branches.len() < n {
            return None;
        }

        let last_branch = &self.last_branches[self.last_branches.len() - n];
        let start = last_branch.start as usize;
        let kind = last_branch.kind;

        // Every recorded last branch should always come with a corresponding fixup.
        let target = self.fixups[self.fixups.len() - n].label;

        Some((start, kind, target))
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
            .partition_point(|&label| self.labels[label].unwrap() < offset);
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

        // Any remaining trailing labels just need to be moved back to the current offset after
        // pruning.
        let cur_offset = self.offset();
        for &label in &self.last_bound_labels[first_unpinned_bound_label..] {
            move_label_back(&mut self.labels, label, cur_offset);
        }

        // Remember that all existing labels have now been pinned: they can't be moved back any
        // longer, but we still want them around for branch threading.
        self.first_unpinned_bound_label = self.last_bound_labels.len();

        // Clear any remaining branch-only state.
        self.last_branches.clear();
        self.reversed_cond_branch_bytes.clear();
    }

    fn offset(&self) -> u32 {
        self.bytes.len().try_into().unwrap()
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

use expect_test::{Expect, expect};
use itertools::Itertools;

use super::*;

#[derive(Clone, Copy)]
struct DummyFixup;
impl FixupKind for DummyFixup {
    fn byte_size(&self) -> usize {
        1
    }

    fn apply(&self, offset: u32, label_offset: u32, bytes: &mut [u8]) {
        assert_eq!(bytes.len(), 1);
        let disp = (label_offset.wrapping_sub(offset) & 0xf) as u8;
        bytes[0] |= disp;
    }
}

fn check_emitted_code(f: impl FnOnce(&mut CodeBuffer<DummyFixup>), expected: Expect) {
    let mut buffer = CodeBuffer::new();
    f(&mut buffer);
    let code = buffer.finish();
    let code_bytes = format!("{:02x}", code.code.iter().format(" "));
    expected.assert_eq(&code_bytes);
}

fn emit_instr(buffer: &mut CodeBuffer<DummyFixup>, op: u8) {
    buffer.instr(|sink| sink.emit(&[op]));
}

fn emit_uncond_branch(buffer: &mut CodeBuffer<DummyFixup>, target: Label) {
    buffer.uncond_branch(target, 0, DummyFixup, |sink| sink.emit(&[0xb0]));
}

fn emit_cond_branch(buffer: &mut CodeBuffer<DummyFixup>, target: Label) {
    buffer.cond_branch(
        target,
        0,
        DummyFixup,
        |sink| sink.emit(&[0xc0]),
        |sink| sink.emit(&[0xd0]),
    );
}

#[test]
fn branch_over_instr() {
    check_emitted_code(
        |buffer| {
            emit_instr(buffer, 0x1);
            let target = buffer.create_label();
            emit_uncond_branch(buffer, target);

            // A new label inside the last branch area should still be unresolvable.
            let label = buffer.create_label();
            buffer.bind_label(label);
            emit_instr(buffer, 0x2);

            // Emitting a non-branch instruction should pin the label.
            assert_eq!(buffer.resolve_label(label), Some(2));

            buffer.bind_label(target);

            emit_instr(buffer, 0x3);
        },
        expect!["01 b2 02 03"],
    );
}

#[test]
fn tight_loop() {
    check_emitted_code(
        |buffer| {
            let label = buffer.create_label();
            buffer.bind_label(label);
            emit_uncond_branch(buffer, label);
        },
        expect!["b0"],
    );
}

#[test]
fn tight_loop_with_several_labels() {
    check_emitted_code(
        |buffer| {
            let label1 = buffer.create_label();
            let label2 = buffer.create_label();

            emit_cond_branch(buffer, label2);
            emit_instr(buffer, 0x1);

            buffer.bind_label(label1);
            buffer.bind_label(label2);
            emit_uncond_branch(buffer, label1);
        },
        expect!["c2 01 b0"],
    );
}

#[test]
fn prune_simple_branch() {
    check_emitted_code(
        |buffer| {
            emit_instr(buffer, 0x1);
            let label = buffer.create_label();
            emit_uncond_branch(buffer, label);
            buffer.bind_label(label);
            emit_instr(buffer, 0x2);
        },
        expect!["01 02"],
    );
}

#[test]
fn prune_simple_branch_chain() {
    check_emitted_code(
        |buffer| {
            emit_instr(buffer, 0x1);

            for _ in 0..3 {
                let label = buffer.create_label();
                emit_uncond_branch(buffer, label);
                buffer.bind_label(label);
            }

            emit_instr(buffer, 0x2);
        },
        expect!["01 02"],
    );
}

#[test]
fn prune_branch_funnel() {
    check_emitted_code(
        |buffer| {
            emit_instr(buffer, 0x1);

            let target = buffer.create_label();

            let temp_labels: Vec<_> = (0..3)
                .map(|_| {
                    let label = buffer.create_label();
                    buffer.bind_label(label);
                    emit_cond_branch(buffer, target);
                    label
                })
                .collect();

            buffer.bind_label(target);

            // Binding the target should force all branches to collapse, pinning all the labels to
            // offset 1.
            for &label in temp_labels.iter() {
                assert_eq!(buffer.resolve_label(label), Some(1));
            }

            emit_instr(buffer, 0x2);
        },
        expect!["01 02"],
    );
}

#[test]
fn prune_nested_branches() {
    check_emitted_code(
        |buffer| {
            let target1 = buffer.create_label();
            let target2 = buffer.create_label();

            emit_instr(buffer, 0x1);

            emit_uncond_branch(buffer, target1);
            let label1 = buffer.create_label();
            buffer.bind_label(label1);

            emit_uncond_branch(buffer, target2);
            let label2 = buffer.create_label();
            buffer.bind_label(label2);

            buffer.bind_label(target2);
            buffer.bind_label(target1);

            emit_instr(buffer, 0x2);

            assert_eq!(buffer.resolve_label(target1), Some(1));
            assert_eq!(buffer.resolve_label(target2), Some(1));
            assert_eq!(buffer.resolve_label(label1), Some(1));
            assert_eq!(buffer.resolve_label(label2), Some(1));
        },
        expect!["01 02"],
    );
}

#[test]
fn prune_parallel_branches() {
    check_emitted_code(
        |buffer| {
            let target1 = buffer.create_label();
            let target2 = buffer.create_label();

            emit_instr(buffer, 0x1);

            emit_uncond_branch(buffer, target1);
            let label1 = buffer.create_label();
            buffer.bind_label(label1);

            emit_uncond_branch(buffer, target2);
            let label2 = buffer.create_label();
            buffer.bind_label(label2);

            buffer.bind_label(target1);
            buffer.bind_label(target2);

            emit_instr(buffer, 0x2);

            assert_eq!(buffer.resolve_label(target1), Some(1));
            assert_eq!(buffer.resolve_label(target2), Some(1));
            assert_eq!(buffer.resolve_label(label1), Some(1));
            assert_eq!(buffer.resolve_label(label2), Some(1));
        },
        expect!["01 02"],
    );
}

#[test]
fn dont_prune_unrelated_branch_back() {
    check_emitted_code(
        |buffer| {
            let target1 = buffer.create_label();
            let target2 = buffer.create_label();
            buffer.bind_label(target1);
            emit_instr(buffer, 0x1);
            emit_uncond_branch(buffer, target1);
            emit_uncond_branch(buffer, target2);
            buffer.bind_label(target2);
            emit_instr(buffer, 0x2);
        },
        expect!["01 bf 02"],
    );
}

#[test]
fn dont_prune_unrelated_branch_forward() {
    check_emitted_code(
        |buffer| {
            let target1 = buffer.create_label();
            let target2 = buffer.create_label();
            emit_instr(buffer, 0x1);
            emit_uncond_branch(buffer, target1);
            emit_uncond_branch(buffer, target2);
            buffer.bind_label(target2);
            emit_instr(buffer, 0x2);
            buffer.bind_label(target1);
            emit_instr(buffer, 0x3);
        },
        expect!["01 b2 02 03"],
    );
}

#[test]
fn correct_retargeting_after_prune_replacement() {
    check_emitted_code(
        |buffer| {
            let before = buffer.create_label();
            let after_pruned_branch = buffer.create_label();
            let after = buffer.create_label();

            buffer.bind_label(before);
            emit_instr(buffer, 0x1);

            // Emit only branches now to keep branch tracking active. Make sure not to use
            // unconditional branches so branch threading doesn't try to kick in at all.
            emit_cond_branch(buffer, before);
            emit_cond_branch(buffer, after_pruned_branch);
            buffer.bind_label(after_pruned_branch);
            emit_cond_branch(buffer, after);

            // Emit a plain instruction to finalize any tracked branches.
            emit_instr(buffer, 0x2);
            buffer.bind_label(after);
            emit_instr(buffer, 0x3);

            // The label should still be before the branch to `after`.
            assert_eq!(buffer.resolve_label(after_pruned_branch), Some(2));
        },
        expect!["01 cf c2 02 03"],
    );
}

#[test]
fn dont_delete_threaded_branch_after_cond_branch() {
    check_emitted_code(
        |buffer| {
            let before = buffer.create_label();
            let after_pruned_branch = buffer.create_label();
            let after = buffer.create_label();

            buffer.bind_label(before);
            emit_instr(buffer, 0x1);

            // Emit only branches now to keep branch tracking active.
            emit_cond_branch(buffer, before);
            emit_cond_branch(buffer, after_pruned_branch);
            buffer.bind_label(after_pruned_branch);
            emit_uncond_branch(buffer, after);

            // Emit a plain instruction to finalize any tracked branches.
            emit_instr(buffer, 0x2);
            buffer.bind_label(after);
            emit_instr(buffer, 0x3);

            // The label itself can be threaded, but we need to leave the branch it points to intact
            // because control can still reach it.
            assert_eq!(
                buffer.resolve_label(after_pruned_branch),
                buffer.resolve_label(after)
            );
        },
        expect!["01 cf b2 02 03"],
    );
}

#[test]
fn dont_delete_threaded_branch_after_revealed_cond_branch() {
    check_emitted_code(
        |buffer| {
            let before = buffer.create_label();
            let after_pruned_branch = buffer.create_label();
            let after = buffer.create_label();

            buffer.bind_label(before);
            emit_instr(buffer, 0x1);

            // Emit only branches now to keep branch tracking active.
            emit_cond_branch(buffer, before);
            emit_uncond_branch(buffer, after_pruned_branch);
            buffer.bind_label(after_pruned_branch);
            emit_uncond_branch(buffer, after);

            // Emit a plain instruction to finalize any tracked branches.
            emit_instr(buffer, 0x2);
            buffer.bind_label(after);
            emit_instr(buffer, 0x3);

            // The label itself can be threaded, but we need to leave the branch it points to intact
            // because control can still reach it.
            assert_eq!(
                buffer.resolve_label(after_pruned_branch),
                buffer.resolve_label(after)
            );
        },
        expect!["01 cf b2 02 03"],
    );
}

#[test]
fn thread_uncond_branch_after_uncond_branch() {
    check_emitted_code(
        |buffer| {
            let before = buffer.create_label();
            let after_pruned_branch = buffer.create_label();
            let after = buffer.create_label();

            buffer.bind_label(before);
            emit_instr(buffer, 0x1);

            // Emit only branches now to keep branch tracking active.
            emit_uncond_branch(buffer, before);
            emit_uncond_branch(buffer, after_pruned_branch);
            buffer.bind_label(after_pruned_branch);
            emit_uncond_branch(buffer, after);

            // Emit a plain instruction to finalize any tracked branches.
            emit_instr(buffer, 0x2);
            buffer.bind_label(after);
            emit_instr(buffer, 0x3);

            // The label should now have been threaded directly to `after`.
            assert_eq!(
                buffer.resolve_label(after_pruned_branch),
                buffer.resolve_label(after)
            );
        },
        expect!["01 bf 02 03"],
    );
}

#[test]
fn clean_up_empty_split_critical_edges() {
    check_emitted_code(
        |buffer| {
            let split_edge_1 = buffer.create_label();
            let split_edge_2 = buffer.create_label();
            let after = buffer.create_label();

            emit_instr(buffer, 0x1);
            emit_cond_branch(buffer, split_edge_2);
            emit_uncond_branch(buffer, split_edge_1);
            buffer.bind_label(split_edge_1);
            emit_uncond_branch(buffer, after);
            buffer.bind_label(split_edge_2);
            emit_uncond_branch(buffer, after);
            buffer.bind_label(after);
            emit_instr(buffer, 0x2);
        },
        expect!["01 02"],
    );
}

#[test]
fn clean_up_empty_split_critical_with_padding() {
    check_emitted_code(
        |buffer| {
            let split_edge_1 = buffer.create_label();
            let split_edge_2 = buffer.create_label();
            let after = buffer.create_label();

            let padding_1 = buffer.create_label();
            let padding_2 = buffer.create_label();
            let padding_3 = buffer.create_label();

            emit_instr(buffer, 0x1);
            emit_cond_branch(buffer, split_edge_2);
            emit_uncond_branch(buffer, split_edge_1);

            buffer.bind_label(padding_1);
            emit_instr(buffer, 0x2);

            buffer.bind_label(split_edge_1);
            emit_uncond_branch(buffer, after);

            buffer.bind_label(padding_2);
            emit_instr(buffer, 0x3);

            buffer.bind_label(split_edge_2);
            emit_uncond_branch(buffer, after);

            buffer.bind_label(padding_3);
            emit_instr(buffer, 0x4);

            buffer.bind_label(after);
            emit_instr(buffer, 0x5);
        },
        expect!["01 c7 b6 02 b4 03 b2 04 05"],
    );
}

#[test]
fn clean_up_empty_split_critical_with_terminated_padding() {
    check_emitted_code(
        |buffer| {
            let split_edge_1 = buffer.create_label();
            let split_edge_2 = buffer.create_label();
            let after = buffer.create_label();

            let elsewhere = buffer.create_label();
            let padding_1 = buffer.create_label();
            let padding_2 = buffer.create_label();
            let padding_3 = buffer.create_label();

            emit_instr(buffer, 0x1);
            emit_cond_branch(buffer, split_edge_2);
            emit_uncond_branch(buffer, split_edge_1);

            buffer.bind_label(padding_1);
            emit_instr(buffer, 0x2);
            emit_uncond_branch(buffer, elsewhere);

            buffer.bind_label(split_edge_1);
            emit_uncond_branch(buffer, after);

            buffer.bind_label(padding_2);
            emit_instr(buffer, 0x3);
            emit_uncond_branch(buffer, elsewhere);

            buffer.bind_label(split_edge_2);
            emit_uncond_branch(buffer, after);

            buffer.bind_label(padding_3);
            emit_instr(buffer, 0x4);
            emit_uncond_branch(buffer, elsewhere);

            buffer.bind_label(after);
            emit_instr(buffer, 0x5);

            buffer.bind_label(elsewhere);
        },
        expect!["01 c8 b7 02 b6 03 b4 04 b2 05"],
    );
}

#[test]
fn dont_thread_twice() {
    check_emitted_code(
        |buffer| {
            let before = buffer.create_label();
            let mid = buffer.create_label();
            let elsewhere = buffer.create_label();

            buffer.bind_label(before);
            emit_instr(buffer, 0x1);
            emit_cond_branch(buffer, mid);
            emit_instr(buffer, 0x2);
            emit_uncond_branch(buffer, elsewhere);

            buffer.bind_label(mid);
            // This should redirect `mid` to `before`.
            emit_uncond_branch(buffer, before);
            // This should *not* redirect `mid` to `elsewhere`.
            emit_uncond_branch(buffer, elsewhere);

            emit_instr(buffer, 0x3);
            buffer.bind_label(elsewhere);
        },
        expect!["01 cf 02 b2 03"],
    );
}

#[test]
fn cond_branch_over_uncond_branch() {
    check_emitted_code(
        |buffer| {
            let mid = buffer.create_label();
            let target = buffer.create_label();

            emit_instr(buffer, 0x1);
            emit_cond_branch(buffer, mid);
            emit_uncond_branch(buffer, target);

            buffer.bind_label(mid);
            emit_instr(buffer, 0x2);

            buffer.bind_label(target);
            emit_instr(buffer, 0x3);
        },
        expect!["01 d2 02 03"],
    );
}

#[test]
fn double_cond_branch_over_uncond_branch() {
    check_emitted_code(
        |buffer| {
            let mid1 = buffer.create_label();
            let mid2 = buffer.create_label();
            let target = buffer.create_label();

            emit_instr(buffer, 0x1);
            emit_cond_branch(buffer, mid1);
            emit_uncond_branch(buffer, mid2);

            buffer.bind_label(mid1);
            emit_uncond_branch(buffer, target);

            buffer.bind_label(mid2);
            emit_instr(buffer, 0x2);

            buffer.bind_label(target);
            emit_instr(buffer, 0x3);
        },
        expect!["01 c2 02 03"],
    );
}

#[test]
fn tdn_int_or_op_sub_funclet_89_bug() {
    check_emitted_code(
        |buffer| {
            let block7 = buffer.create_label();
            let block8 = buffer.create_label();
            let block9 = buffer.create_label();
            let block11 = buffer.create_label();
            let block12 = buffer.create_label();

            emit_instr(buffer, 0x6);
            emit_cond_branch(buffer, block7);
            emit_uncond_branch(buffer, block9);

            buffer.bind_label(block7);
            emit_uncond_branch(buffer, block8);

            buffer.bind_label(block9);
            emit_instr(buffer, 0x9);

            emit_cond_branch(buffer, block8);
            emit_uncond_branch(buffer, block11);

            buffer.bind_label(block8);
            emit_uncond_branch(buffer, block12);

            buffer.bind_label(block11);
            emit_instr(buffer, 0xb);

            buffer.bind_label(block12);
            emit_instr(buffer, 0xc);
        },
        expect!["06 c4 09 c2 0b 0c"],
    );
}

#[test]
fn thread_uncond_branch_after_reversed_branch() {
    check_emitted_code(
        |buffer| {
            let block8 = buffer.create_label();
            let block11 = buffer.create_label();
            let block12 = buffer.create_label();

            emit_cond_branch(buffer, block8);
            emit_instr(buffer, 0x1);

            emit_cond_branch(buffer, block8);
            emit_uncond_branch(buffer, block11);

            // Cause the last branch pair to be reversed, which .
            buffer.bind_label(block8);
            emit_uncond_branch(buffer, block12);

            buffer.bind_label(block11);
            emit_instr(buffer, 0x2);

            buffer.bind_label(block12);
            emit_instr(buffer, 0x3);
        },
        expect!["c4 01 c2 02 03"],
    );
}

#[test]
fn tight_loop_after_pruned_branches() {
    check_emitted_code(
        |buffer| {
            // This test's purpose is to make sure we correctly track which labels point to the end
            // of the code buffer, even when the pruning of previous branches causes them to move
            // back.

            let collapsed = buffer.create_label();
            let loop_header = buffer.create_label();
            let end = buffer.create_label();

            // Keep at least one branch in the buffer that won't be pruned when we bind `collapsed`.
            // That way, we won't `flush_tracked_branches` before trying to create the loop, which
            // means `loop_header` will point past the end of the buffer after pruning.
            emit_cond_branch(buffer, end);

            // Add a bunch of branches that will all be pruned when we bind `collapsed`.
            emit_uncond_branch(buffer, collapsed);
            emit_uncond_branch(buffer, collapsed);
            emit_uncond_branch(buffer, collapsed);
            emit_uncond_branch(buffer, collapsed);

            // Bind our loop header now, at buffer offset 5. It won't be moved back when `collapsed`
            // is bound because of the extra branch to `end` at the start of the buffer, which will
            // prevent a `flush_tracked_branches` here.
            buffer.bind_label(loop_header);

            // Bind `collapsed`, which will prune all branches leading to it here.
            buffer.bind_label(collapsed);

            // Loop back to `loop_header`. This branch should _not_ be pruned or threaded, even if
            // its label currently points past the end of the buffer.
            emit_uncond_branch(buffer, loop_header);

            // Flush all branch state now.
            emit_instr(buffer, 0x1);

            buffer.bind_label(end);
        },
        expect!["c3 b0 01"],
    );
}

#[test]
fn cond_loop_after_pruned_branches() {
    check_emitted_code(
        |buffer| {
            // This test's purpose is to make sure we correctly track which labels point to the end
            // of the code buffer, even when the pruning of previous branches causes them to move
            // back.

            let collapsed = buffer.create_label();
            let loop_header = buffer.create_label();
            let end = buffer.create_label();

            // Keep at least one branch in the buffer that won't be pruned when we bind `collapsed`.
            // That way, we won't `flush_tracked_branches` before trying to create the loop, which
            // means `loop_header` will point past the end of the buffer after pruning.
            emit_cond_branch(buffer, end);

            // Add a bunch of branches that will all be pruned when we bind `collapsed`.
            emit_uncond_branch(buffer, collapsed);
            emit_uncond_branch(buffer, collapsed);
            emit_uncond_branch(buffer, collapsed);
            emit_uncond_branch(buffer, collapsed);

            // Bind our loop header now, at buffer offset 5. It won't be moved back when `collapsed`
            // is bound because of the extra branch to `end` at the start of the buffer, which will
            // prevent a `flush_tracked_branches` here.
            buffer.bind_label(loop_header);

            // Bind `collapsed`, which will prune all branches leading to it here.
            buffer.bind_label(collapsed);

            emit_cond_branch(buffer, end);

            // Loop back to `loop_header`. This branch should _not_ be pruned or threaded, even if
            // its label currently points past the end of the buffer.
            emit_uncond_branch(buffer, loop_header);

            // Flush all branch state now.
            emit_instr(buffer, 0x1);

            buffer.bind_label(end);
        },
        expect!["c4 c3 bf 01"],
    );
}

#[test]
fn try_prune_threaded_branch() {
    check_emitted_code(
        |buffer| {
            let start = buffer.create_label();
            let mid = buffer.create_label();
            let end = buffer.create_label();

            buffer.bind_label(start);
            emit_uncond_branch(buffer, end);

            emit_instr(buffer, 0x1);
            emit_cond_branch(buffer, start);

            // Force branch pruning to run here (though it won't actually prune anything).
            buffer.bind_label(mid);

            emit_instr(buffer, 0x2);
            buffer.bind_label(end);
        },
        expect!["b4 01 c2 02"],
    );
}

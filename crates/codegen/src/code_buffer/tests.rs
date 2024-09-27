use expect_test::{expect, Expect};
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
    buffer.instr(|instr| instr.emit(&[op]));
}

fn emit_branch(buffer: &mut CodeBuffer<DummyFixup>, target: Label) {
    buffer.branch(target, 0, DummyFixup, |instr| instr.emit(&[0xb0]));
}

#[test]
fn branch_over_instr() {
    check_emitted_code(
        |buffer| {
            emit_instr(buffer, 0x1);
            let target = buffer.create_label();
            emit_branch(buffer, target);

            // A new label inside the last branch area should still be unresolvable.
            let label = buffer.create_label();
            buffer.bind_label(label);
            assert_eq!(buffer.resolve_label(label), None);

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
            emit_branch(buffer, label);
        },
        expect!["b0"],
    );
}

#[test]
fn prune_simple_branch() {
    check_emitted_code(
        |buffer| {
            emit_instr(buffer, 0x1);
            let label = buffer.create_label();
            emit_branch(buffer, label);
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
                emit_branch(buffer, label);
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
                    emit_branch(buffer, target);
                    label
                })
                .collect();

            // The branches should all still be pending at this point even though the labels are
            // bound. This means that all labels but the first should still be unresolvable.
            assert_eq!(buffer.resolve_label(temp_labels[0]), Some(1));

            for &label in temp_labels.iter().skip(1) {
                assert_eq!(buffer.resolve_label(label), None);
            }

            buffer.bind_label(target);

            // Binding the target should force all branches to collapse, pinning all the labels to
            // offset 1.
            for &label in temp_labels.iter().skip(1) {
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

            emit_branch(buffer, target1);
            let label1 = buffer.create_label();
            buffer.bind_label(label1);

            emit_branch(buffer, target2);
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

            emit_branch(buffer, target1);
            let label1 = buffer.create_label();
            buffer.bind_label(label1);

            emit_branch(buffer, target2);
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
            emit_branch(buffer, target1);
            emit_branch(buffer, target2);
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
            emit_branch(buffer, target1);
            emit_branch(buffer, target2);
            buffer.bind_label(target2);
            emit_instr(buffer, 0x2);
            buffer.bind_label(target1);
            emit_instr(buffer, 0x3);
        },
        expect!["01 b2 02 03"],
    );
}

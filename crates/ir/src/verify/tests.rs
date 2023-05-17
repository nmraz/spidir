use crate::node::{IcmpKind, NodeKind, Type};

use super::*;

#[track_caller]
fn check_verify_graph_errors(graph: &ValGraph, entry: Node, expected_errors: &[VerifierError]) {
    assert_eq!(verify_graph(graph, entry).unwrap_err(), expected_errors);
}

#[track_caller]
fn check_verify_node_kind(graph: &ValGraph, node: Node, expected_err: VerifierError) {
    let mut errors = Vec::new();
    verify_node_kind(graph, node, &mut errors);
    assert_eq!(errors, &[expected_err]);
}

fn all_integer_types() -> Vec<DepValueKind> {
    vec![
        DepValueKind::Value(Type::I32),
        DepValueKind::Value(Type::I64),
    ]
}

fn create_const_typed(graph: &mut ValGraph, ty: Type) -> DepValue {
    let const_node = graph.create_node(NodeKind::IConst(5), [], [DepValueKind::Value(ty)]);
    graph.node_outputs(const_node)[0]
}

fn create_const32(graph: &mut ValGraph) -> DepValue {
    create_const_typed(graph, Type::I32)
}

fn create_const64(graph: &mut ValGraph) -> DepValue {
    create_const_typed(graph, Type::I64)
}

fn create_region<const N: usize>(graph: &mut ValGraph, inputs: [DepValue; N]) -> DepValue {
    let region = graph.create_node(
        NodeKind::Region,
        inputs,
        [DepValueKind::Control, DepValueKind::PhiSelector],
    );
    graph.node_outputs(region)[0]
}

#[test]
fn verify_graph_add_function() {
    let mut graph = ValGraph::new();
    let entry = graph.create_node(
        NodeKind::Entry,
        [],
        [
            DepValueKind::Control,
            DepValueKind::Value(Type::I32),
            DepValueKind::Value(Type::I32),
        ],
    );
    let entry_outputs = graph.node_outputs(entry);
    let control_value = entry_outputs[0];
    let param1 = entry_outputs[1];
    let param2 = entry_outputs[2];

    let add = graph.create_node(
        NodeKind::Iadd,
        [param1, param2],
        [DepValueKind::Value(Type::I32)],
    );
    let add_res = graph.node_outputs(add)[0];
    graph.create_node(NodeKind::Return, [control_value, add_res], []);

    assert_eq!(verify_graph(&graph, entry), Ok(()));
}

#[test]
fn verify_graph_unused_control() {
    let mut graph = ValGraph::new();
    let entry = graph.create_node(NodeKind::Entry, [], [DepValueKind::Control]);
    let entry_control = graph.node_outputs(entry)[0];

    check_verify_graph_errors(
        &graph,
        entry,
        &[VerifierError::UnusedControl(entry_control)],
    );
}

#[test]
fn verify_graph_unused_control_dead_region() {
    let mut graph = ValGraph::new();
    let entry = graph.create_node(NodeKind::Entry, [], [DepValueKind::Control]);
    let entry_control = graph.node_outputs(entry)[0];
    graph.create_node(NodeKind::Return, [entry_control], []);
    create_region(&mut graph, []);
    assert_eq!(verify_graph(&graph, entry), Ok(()));
}

#[test]
fn verify_graph_reused_control() {
    let mut graph = ValGraph::new();
    let entry = graph.create_node(NodeKind::Entry, [], [DepValueKind::Control]);
    let entry_control = graph.node_outputs(entry)[0];

    let region1 = create_region(&mut graph, [entry_control]);
    let region2 = create_region(&mut graph, [entry_control]);

    graph.create_node(NodeKind::Return, [region1], []);
    graph.create_node(NodeKind::Return, [region2], []);

    check_verify_graph_errors(
        &graph,
        entry,
        &[VerifierError::ReusedControl(entry_control)],
    );
}

#[test]
fn verify_iadd_bad_input_count() {
    let mut graph = ValGraph::new();

    let empty_iadd = graph.create_node(NodeKind::Iadd, [], [DepValueKind::Value(Type::I32)]);
    check_verify_node_kind(
        &graph,
        empty_iadd,
        VerifierError::BadInputCount {
            node: empty_iadd,
            expected: 2,
        },
    );

    let const_val = create_const32(&mut graph);

    let overfull_iadd = graph.create_node(
        NodeKind::Iadd,
        [const_val, const_val, const_val],
        [DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        overfull_iadd,
        VerifierError::BadInputCount {
            node: overfull_iadd,
            expected: 2,
        },
    );
}

#[test]
fn verify_iadd_result_kind() {
    let mut graph = ValGraph::new();
    let const_val = create_const32(&mut graph);

    let iadd = graph.create_node(
        NodeKind::Iadd,
        [const_val, const_val],
        [DepValueKind::PhiSelector],
    );
    let iadd_output = graph.node_outputs(iadd)[0];
    check_verify_node_kind(
        &graph,
        iadd,
        VerifierError::BadOutputKind {
            value: iadd_output,
            expected: all_integer_types(),
        },
    );
}

#[test]
fn verify_iadd_input_kind() {
    let mut graph = ValGraph::new();

    let const_val = create_const32(&mut graph);
    let const64_val = create_const64(&mut graph);

    let iadd64_32 = graph.create_node(
        NodeKind::Iadd,
        [const64_val, const_val],
        [DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        iadd64_32,
        VerifierError::BadInputKind {
            node: iadd64_32,
            input: 0,
            expected: vec![DepValueKind::Value(Type::I32)],
        },
    );

    let iadd32_64 = graph.create_node(
        NodeKind::Iadd,
        [const_val, const64_val],
        [DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        iadd32_64,
        VerifierError::BadInputKind {
            node: iadd32_64,
            input: 1,
            expected: vec![DepValueKind::Value(Type::I32)],
        },
    );
}

#[test]
fn verify_graph_shift_function_ok() {
    for shift_input_ty in [Type::I32, Type::I64] {
        for shift_amount_ty in [Type::I32, Type::I64] {
            let mut graph = ValGraph::new();
            let entry = graph.create_node(
                NodeKind::Entry,
                [],
                [
                    DepValueKind::Control,
                    DepValueKind::Value(shift_input_ty),
                    DepValueKind::Value(shift_amount_ty),
                ],
            );
            let entry_outputs = graph.node_outputs(entry);
            let control_value = entry_outputs[0];
            let shift_input = entry_outputs[1];
            let shift_amount = entry_outputs[2];

            let add = graph.create_node(
                NodeKind::Shl,
                [shift_input, shift_amount],
                [DepValueKind::Value(shift_input_ty)],
            );
            let shl_res = graph.node_outputs(add)[0];
            graph.create_node(NodeKind::Return, [control_value, shl_res], []);

            assert_eq!(verify_graph(&graph, entry), Ok(()));
        }
    }
}

#[test]
fn verify_shl_bad_input_count() {
    let mut graph = ValGraph::new();

    let empty_shl = graph.create_node(NodeKind::Shl, [], [DepValueKind::Value(Type::I32)]);
    check_verify_node_kind(
        &graph,
        empty_shl,
        VerifierError::BadInputCount {
            node: empty_shl,
            expected: 2,
        },
    );

    let const_val = create_const32(&mut graph);

    let overfull_shl = graph.create_node(
        NodeKind::Shl,
        [const_val, const_val, const_val],
        [DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        overfull_shl,
        VerifierError::BadInputCount {
            node: overfull_shl,
            expected: 2,
        },
    );
}

#[test]
fn verify_shl_result_kind() {
    let mut graph = ValGraph::new();
    let const_val = create_const32(&mut graph);

    let shl = graph.create_node(
        NodeKind::Shl,
        [const_val, const_val],
        [DepValueKind::PhiSelector],
    );
    let shl_output = graph.node_outputs(shl)[0];
    check_verify_node_kind(
        &graph,
        shl,
        VerifierError::BadOutputKind {
            value: shl_output,
            expected: all_integer_types(),
        },
    );
}

#[test]
fn verify_shl_input_kind() {
    let mut graph = ValGraph::new();

    let const_val = create_const32(&mut graph);
    let const64_val = create_const64(&mut graph);

    let shl64_32 = graph.create_node(
        NodeKind::Shl,
        [const64_val, const_val],
        [DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        shl64_32,
        VerifierError::BadInputKind {
            node: shl64_32,
            input: 0,
            expected: vec![DepValueKind::Value(Type::I32)],
        },
    );

    let non_int_val = create_region(&mut graph, []);
    let shl_non_int = graph.create_node(
        NodeKind::Shl,
        [const_val, non_int_val],
        [DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        shl_non_int,
        VerifierError::BadInputKind {
            node: shl_non_int,
            input: 1,
            expected: all_integer_types(),
        },
    );
}

#[test]
fn verify_graph_icmp_function_ok() {
    for input_ty in [Type::I32, Type::I64, Type::Ptr] {
        for output_ty in [Type::I32, Type::I64] {
            let mut graph = ValGraph::new();
            let entry = graph.create_node(
                NodeKind::Entry,
                [],
                [
                    DepValueKind::Control,
                    DepValueKind::Value(input_ty),
                    DepValueKind::Value(input_ty),
                ],
            );
            let entry_outputs = graph.node_outputs(entry);
            let control_value = entry_outputs[0];
            let param1 = entry_outputs[1];
            let param2 = entry_outputs[2];

            let add = graph.create_node(
                NodeKind::Icmp(IcmpKind::Eq),
                [param1, param2],
                [DepValueKind::Value(output_ty)],
            );
            let icmp_res = graph.node_outputs(add)[0];
            graph.create_node(NodeKind::Return, [control_value, icmp_res], []);

            assert_eq!(verify_graph(&graph, entry), Ok(()));
        }
    }
}

#[test]
fn verify_icmp_input_count() {
    let mut graph = ValGraph::new();
    let empty_icmp = graph.create_node(
        NodeKind::Icmp(IcmpKind::Eq),
        [],
        [DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        empty_icmp,
        VerifierError::BadInputCount {
            node: empty_icmp,
            expected: 2,
        },
    );
}

#[test]
fn verify_icmp_input_kind() {
    let mut graph = ValGraph::new();

    let non_int_val = create_region(&mut graph, []);
    let const_val = create_const32(&mut graph);
    let const64_val = create_const64(&mut graph);

    let non_int_icmp = graph.create_node(
        NodeKind::Icmp(IcmpKind::Eq),
        [non_int_val, const_val],
        [DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        non_int_icmp,
        VerifierError::BadInputKind {
            node: non_int_icmp,
            input: 0,
            expected: vec![
                DepValueKind::Value(Type::I32),
                DepValueKind::Value(Type::I64),
                DepValueKind::Value(Type::Ptr),
            ],
        },
    );

    let mismatched_icmp = graph.create_node(
        NodeKind::Icmp(IcmpKind::Eq),
        [const_val, const64_val],
        [DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        mismatched_icmp,
        VerifierError::BadInputKind {
            node: mismatched_icmp,
            input: 1,
            expected: vec![DepValueKind::Value(Type::I32)],
        },
    );
}

#[test]
fn verify_icmp_output_kind() {
    let mut graph = ValGraph::new();
    let const_val = create_const32(&mut graph);

    let icmp = graph.create_node(
        NodeKind::Icmp(IcmpKind::Eq),
        [const_val, const_val],
        [DepValueKind::Value(Type::Ptr)],
    );
    check_verify_node_kind(
        &graph,
        icmp,
        VerifierError::BadOutputKind {
            value: graph.node_outputs(icmp)[0],
            expected: all_integer_types(),
        },
    );
}

#[test]
fn verify_iconst_input_count() {
    let mut graph = ValGraph::new();
    let const_val = create_const32(&mut graph);
    let iconst = graph.create_node(
        NodeKind::IConst(3),
        [const_val],
        [DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        iconst,
        VerifierError::BadInputCount {
            node: iconst,
            expected: 0,
        },
    );
}

#[test]
fn verify_iconst_output_kind() {
    let mut graph = ValGraph::new();

    let iconst_ctrl = graph.create_node(NodeKind::IConst(3), [], [DepValueKind::Control]);
    check_verify_node_kind(
        &graph,
        iconst_ctrl,
        VerifierError::BadOutputKind {
            value: graph.node_outputs(iconst_ctrl)[0],
            expected: all_integer_types(),
        },
    );

    let iconst_ptr = graph.create_node(NodeKind::IConst(3), [], [DepValueKind::Value(Type::Ptr)]);
    check_verify_node_kind(
        &graph,
        iconst_ptr,
        VerifierError::BadOutputKind {
            value: graph.node_outputs(iconst_ptr)[0],
            expected: all_integer_types(),
        },
    );
}

#[test]
fn verify_iconst_range() {
    let mut graph = ValGraph::new();
    let not_u32 = graph.create_node(
        NodeKind::IConst(u32::MAX as u64 + 1),
        [],
        [DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(&graph, not_u32, VerifierError::ConstantOutOfRange(not_u32));
}

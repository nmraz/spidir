use crate::node::{NodeKind, Type};

use super::*;

#[track_caller]
fn check_verify_graph_errors(graph: &ValGraph, entry: Node, expected_errors: &[VerifierError]) {
    assert_eq!(verify_graph(graph, entry).unwrap_err(), expected_errors);
}

#[track_caller]
fn check_verify_int_binop(graph: &ValGraph, node: Node, expected_err: VerifierError) {
    let mut errors = Vec::new();
    verify_int_binop(graph, node, &mut errors);
    assert_eq!(errors, &[expected_err]);
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
    graph.create_node(NodeKind::Region, [], [DepValueKind::Control]);
    assert_eq!(verify_graph(&graph, entry), Ok(()));
}

#[test]
fn verify_graph_reused_control() {
    let mut graph = ValGraph::new();
    let entry = graph.create_node(NodeKind::Entry, [], [DepValueKind::Control]);
    let entry_control = graph.node_outputs(entry)[0];
    graph.create_node(NodeKind::Region, [entry_control], []);
    graph.create_node(NodeKind::Region, [entry_control], []);

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
    check_verify_int_binop(
        &graph,
        empty_iadd,
        VerifierError::BadInputCount {
            node: empty_iadd,
            expected: 2,
        },
    );

    let five = graph.create_node(NodeKind::IConst(5), [], [DepValueKind::Value(Type::I32)]);
    let five_val = graph.node_outputs(five)[0];

    let overful_iadd = graph.create_node(
        NodeKind::Iadd,
        [five_val, five_val, five_val],
        [DepValueKind::Value(Type::I32)],
    );
    check_verify_int_binop(
        &graph,
        overful_iadd,
        VerifierError::BadInputCount {
            node: overful_iadd,
            expected: 2,
        },
    );
}

#[test]
fn verify_iadd_result_kind() {
    let mut graph = ValGraph::new();

    let five = graph.create_node(NodeKind::IConst(5), [], [DepValueKind::Value(Type::I32)]);
    let five_val = graph.node_outputs(five)[0];

    let iadd = graph.create_node(
        NodeKind::Iadd,
        [five_val, five_val],
        [DepValueKind::PhiSelector],
    );
    let iadd_output = graph.node_outputs(iadd)[0];
    check_verify_int_binop(
        &graph,
        iadd,
        VerifierError::BadOutputKind {
            value: iadd_output,
            expected: vec![
                DepValueKind::Value(Type::I32),
                DepValueKind::Value(Type::I64),
            ],
        },
    );
}

#[test]
fn verify_iadd_input_kind() {
    let mut graph = ValGraph::new();

    let five = graph.create_node(NodeKind::IConst(5), [], [DepValueKind::Value(Type::I32)]);
    let five_val = graph.node_outputs(five)[0];

    let five64 = graph.create_node(NodeKind::IConst(5), [], [DepValueKind::Value(Type::I64)]);
    let five64_val = graph.node_outputs(five64)[0];

    let iadd64_32 = graph.create_node(
        NodeKind::Iadd,
        [five64_val, five_val],
        [DepValueKind::Value(Type::I32)],
    );
    check_verify_int_binop(
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
        [five_val, five64_val],
        [DepValueKind::Value(Type::I32)],
    );
    check_verify_int_binop(
        &graph,
        iadd32_64,
        VerifierError::BadInputKind {
            node: iadd32_64,
            input: 1,
            expected: vec![DepValueKind::Value(Type::I32)],
        },
    );
}

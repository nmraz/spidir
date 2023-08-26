use crate::{
    node::{NodeKind, Type},
    test_utils::create_entry,
};

use super::*;

#[track_caller]
fn check_verify_node_kind(graph: &ValGraph, node: Node, expected_err: GraphVerifierError) {
    let mut errors = Vec::new();
    // Dummy signature since we usually don't care about it.
    let signature = Signature {
        ret_type: None,
        param_types: vec![],
    };
    // Dummy entry node since we usually don't care about it.
    verify_node_kind(graph, &signature, node, node, &mut errors);
    assert_eq!(errors, &[expected_err]);
}

#[test]
fn verify_brcond_input_count() {
    let mut graph = ValGraph::new();
    let brcond = graph.create_node(
        NodeKind::BrCond,
        [],
        [DepValueKind::Control, DepValueKind::Control],
    );
    check_verify_node_kind(
        &graph,
        brcond,
        GraphVerifierError::BadInputCount {
            node: brcond,
            expected: 2,
        },
    );
}

#[test]
fn verify_brcond_output_count() {
    let mut graph = ValGraph::new();
    let (_, entry_ctrl, [param]) = create_entry(&mut graph, [Type::I32]);
    let brcond = graph.create_node(NodeKind::BrCond, [entry_ctrl, param], []);
    check_verify_node_kind(
        &graph,
        brcond,
        GraphVerifierError::BadOutputCount {
            node: brcond,
            expected: 2,
        },
    );
}

#[test]
fn verify_brcond_input_kinds() {
    let mut graph = ValGraph::new();
    let (_, entry_ctrl, [int_val, ptr_val]) = create_entry(&mut graph, [Type::I64, Type::Ptr]);

    let non_ctrl_brcond = graph.create_node(
        NodeKind::BrCond,
        [int_val, int_val],
        [DepValueKind::Control, DepValueKind::Control],
    );
    check_verify_node_kind(
        &graph,
        non_ctrl_brcond,
        GraphVerifierError::BadInputKind {
            node: non_ctrl_brcond,
            input: 0,
            expected: vec![DepValueKind::Control],
        },
    );

    let non_int_brcond = graph.create_node(
        NodeKind::BrCond,
        [entry_ctrl, ptr_val],
        [DepValueKind::Control, DepValueKind::Control],
    );
    check_verify_node_kind(
        &graph,
        non_int_brcond,
        GraphVerifierError::BadInputKind {
            node: non_int_brcond,
            input: 1,
            expected: vec![
                DepValueKind::Value(Type::I32),
                DepValueKind::Value(Type::I64),
            ],
        },
    );
}

#[test]
fn verify_brcond_output_kinds() {
    let mut graph = ValGraph::new();
    let (_, entry_ctrl, [param]) = create_entry(&mut graph, [Type::I64]);

    let non_ctrl1_brcond = graph.create_node(
        NodeKind::BrCond,
        [entry_ctrl, param],
        [DepValueKind::PhiSelector, DepValueKind::Control],
    );
    check_verify_node_kind(
        &graph,
        non_ctrl1_brcond,
        GraphVerifierError::BadOutputKind {
            value: graph.node_outputs(non_ctrl1_brcond)[0],
            expected: vec![DepValueKind::Control],
        },
    );

    let non_ctrl1_brcond = graph.create_node(
        NodeKind::BrCond,
        [entry_ctrl, param],
        [DepValueKind::Control, DepValueKind::PhiSelector],
    );
    check_verify_node_kind(
        &graph,
        non_ctrl1_brcond,
        GraphVerifierError::BadOutputKind {
            value: graph.node_outputs(non_ctrl1_brcond)[1],
            expected: vec![DepValueKind::Control],
        },
    );
}

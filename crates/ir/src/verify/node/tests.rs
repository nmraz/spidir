use crate::{
    node::{NodeKind, Type},
    test_utils::{create_entry, create_region},
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
fn verify_load_input_count() {
    let mut graph = ValGraph::new();

    let control_val = create_region(&mut graph, []);
    let load = graph.create_node(
        NodeKind::Load,
        [control_val],
        [DepValueKind::Control, DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        load,
        GraphVerifierError::BadInputCount {
            node: load,
            expected: 2,
        },
    );
}

#[test]
fn verify_load_output_count() {
    let mut graph = ValGraph::new();

    let (_, control_val, [ptr_val]) = create_entry(&mut graph, [Type::Ptr]);
    let load = graph.create_node(
        NodeKind::Load,
        [control_val, ptr_val],
        [DepValueKind::Control],
    );
    check_verify_node_kind(
        &graph,
        load,
        GraphVerifierError::BadOutputCount {
            node: load,
            expected: 2,
        },
    );
}

#[test]
fn verify_load_input_kinds() {
    let mut graph = ValGraph::new();

    let (_, control_val, [val32, ptr_val]) = create_entry(&mut graph, [Type::I32, Type::Ptr]);

    let non_ptr_load = graph.create_node(
        NodeKind::Load,
        [control_val, val32],
        [DepValueKind::Control, DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        non_ptr_load,
        GraphVerifierError::BadInputKind {
            node: non_ptr_load,
            input: 1,
            expected: vec![DepValueKind::Value(Type::Ptr)],
        },
    );

    let non_ctrl_load = graph.create_node(
        NodeKind::Load,
        [val32, ptr_val],
        [DepValueKind::Control, DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        non_ctrl_load,
        GraphVerifierError::BadInputKind {
            node: non_ctrl_load,
            input: 0,
            expected: vec![DepValueKind::Control],
        },
    );
}

#[test]
fn verify_load_output_kinds() {
    let mut graph = ValGraph::new();

    let (_, control_val, [ptr_val]) = create_entry(&mut graph, [Type::Ptr]);

    let non_value_load = graph.create_node(
        NodeKind::Load,
        [control_val, ptr_val],
        [DepValueKind::Control, DepValueKind::Control],
    );
    check_verify_node_kind(
        &graph,
        non_value_load,
        GraphVerifierError::BadOutputKind {
            value: graph.node_outputs(non_value_load)[1],
            expected: vec![
                DepValueKind::Value(Type::I32),
                DepValueKind::Value(Type::I64),
                DepValueKind::Value(Type::F64),
                DepValueKind::Value(Type::Ptr),
            ],
        },
    );

    let non_value_load = graph.create_node(
        NodeKind::Load,
        [control_val, ptr_val],
        [
            DepValueKind::Value(Type::I32),
            DepValueKind::Value(Type::I32),
        ],
    );
    check_verify_node_kind(
        &graph,
        non_value_load,
        GraphVerifierError::BadOutputKind {
            value: graph.node_outputs(non_value_load)[0],
            expected: vec![DepValueKind::Control],
        },
    );
}

#[test]
fn verify_store_input_count() {
    let mut graph = ValGraph::new();
    let store = graph.create_node(NodeKind::Store, [], [DepValueKind::Control]);
    check_verify_node_kind(
        &graph,
        store,
        GraphVerifierError::BadInputCount {
            node: store,
            expected: 3,
        },
    );
}

#[test]
fn verify_store_output_count() {
    let mut graph = ValGraph::new();
    let (_, entry_control, [value, ptr]) = create_entry(&mut graph, [Type::I32, Type::Ptr]);
    let store = graph.create_node(NodeKind::Store, [entry_control, value, ptr], []);
    check_verify_node_kind(
        &graph,
        store,
        GraphVerifierError::BadOutputCount {
            node: store,
            expected: 1,
        },
    );
}

#[test]
fn verify_store_input_kinds() {
    let mut graph = ValGraph::new();
    let (_, entry_ctrl, [value, ptr]) = create_entry(&mut graph, [Type::I32, Type::Ptr]);

    let non_ctrl_store = graph.create_node(
        NodeKind::Store,
        [value, value, ptr],
        [DepValueKind::Control],
    );
    check_verify_node_kind(
        &graph,
        non_ctrl_store,
        GraphVerifierError::BadInputKind {
            node: non_ctrl_store,
            input: 0,
            expected: vec![DepValueKind::Control],
        },
    );

    let non_value_store = graph.create_node(
        NodeKind::Store,
        [entry_ctrl, entry_ctrl, ptr],
        [DepValueKind::Control],
    );
    check_verify_node_kind(
        &graph,
        non_value_store,
        GraphVerifierError::BadInputKind {
            node: non_value_store,
            input: 1,
            expected: vec![
                DepValueKind::Value(Type::I32),
                DepValueKind::Value(Type::I64),
                DepValueKind::Value(Type::F64),
                DepValueKind::Value(Type::Ptr),
            ],
        },
    );

    let non_ptr_store = graph.create_node(
        NodeKind::Store,
        [entry_ctrl, value, value],
        [DepValueKind::Control],
    );
    check_verify_node_kind(
        &graph,
        non_ptr_store,
        GraphVerifierError::BadInputKind {
            node: non_ptr_store,
            input: 2,
            expected: vec![DepValueKind::Value(Type::Ptr)],
        },
    );
}

#[test]
fn verify_store_output_kinds() {
    let mut graph = ValGraph::new();
    let (_, entry_ctrl, [value, ptr]) = create_entry(&mut graph, [Type::I32, Type::Ptr]);

    let non_ctrl_store = graph.create_node(
        NodeKind::Store,
        [entry_ctrl, value, ptr],
        [DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        non_ctrl_store,
        GraphVerifierError::BadOutputKind {
            value: graph.node_outputs(non_ctrl_store)[0],
            expected: vec![DepValueKind::Control],
        },
    );
}

#[test]
fn verify_stack_slot_output_kinds() {
    let mut graph = ValGraph::new();

    let slot_addr_node = graph.create_node(
        NodeKind::StackSlot { size: 4, align: 4 },
        [],
        [DepValueKind::Value(Type::F64)],
    );
    let slot_addr = graph.node_outputs(slot_addr_node)[0];
    check_verify_node_kind(
        &graph,
        slot_addr_node,
        GraphVerifierError::BadOutputKind {
            value: slot_addr,
            expected: vec![DepValueKind::Value(Type::Ptr)],
        },
    );
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

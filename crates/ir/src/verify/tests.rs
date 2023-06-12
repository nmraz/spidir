use crate::{
    node::Type,
    test_utils::{create_entry, create_loop_graph, create_region, create_return},
};

use super::*;

#[track_caller]
fn check_verify_graph_errors(graph: &ValGraph, entry: Node, expected_errors: &[VerifierError]) {
    let signature = Signature {
        ret_type: None,
        arg_types: vec![],
    };
    assert_eq!(
        verify_graph(graph, &signature, entry).unwrap_err(),
        expected_errors
    );
}

#[test]
fn verify_graph_loop_function() {
    let (graph, entry) = create_loop_graph();
    let signature = Signature {
        ret_type: Some(Type::I32),
        arg_types: vec![Type::I32],
    };
    assert_eq!(verify_graph(&graph, &signature, entry), Ok(()));
}

#[test]
fn verify_graph_unused_control() {
    let mut graph = ValGraph::new();
    let (entry, entry_control, []) = create_entry(&mut graph, []);

    check_verify_graph_errors(
        &graph,
        entry,
        &[VerifierError::UnusedControl(entry_control)],
    );
}

#[test]
fn verify_graph_unused_control_dead_region() {
    let mut graph = ValGraph::new();
    let (entry, entry_control, []) = create_entry(&mut graph, []);
    create_return(&mut graph, [entry_control]);
    create_region(&mut graph, []);
    let signature = Signature {
        ret_type: None,
        arg_types: vec![],
    };
    assert_eq!(verify_graph(&graph, &signature, entry), Ok(()));
}

#[test]
fn verify_graph_reused_control() {
    let mut graph = ValGraph::new();
    let (entry, entry_control, []) = create_entry(&mut graph, []);

    let region1 = create_region(&mut graph, [entry_control]);
    let region2 = create_region(&mut graph, [entry_control]);

    create_return(&mut graph, [region1]);
    create_return(&mut graph, [region2]);

    check_verify_graph_errors(
        &graph,
        entry,
        &[VerifierError::ReusedControl(entry_control)],
    );
}

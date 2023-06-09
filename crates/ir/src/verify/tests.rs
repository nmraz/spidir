use crate::{
    module::{ExternFunctionData, FunctionData},
    node::Type,
    test_utils::{create_const32, create_entry, create_loop_graph, create_region, create_return},
};

use super::*;

#[track_caller]
fn check_verify_graph_errors(
    graph: &ValGraph,
    entry: Node,
    expected_errors: &[GraphVerifierError],
) {
    let signature = Signature {
        ret_type: None,
        param_types: vec![],
    };
    assert_eq!(
        verify_graph(graph, &signature, entry).unwrap_err(),
        expected_errors
    );
}

#[track_caller]
fn check_verify_module_errors(module: &Module, expected_errors: &[ModuleVerifierError]) {
    assert_eq!(verify_module(module).unwrap_err(), expected_errors);
}

#[test]
fn verify_graph_loop_function() {
    let (graph, entry) = create_loop_graph();
    let signature = Signature {
        ret_type: Some(Type::I32),
        param_types: vec![Type::I32],
    };
    verify_graph(&graph, &signature, entry).expect("expected a valid graph");
}

#[test]
fn verify_graph_unused_control() {
    let mut graph = ValGraph::new();
    let (entry, entry_control, []) = create_entry(&mut graph, []);

    check_verify_graph_errors(
        &graph,
        entry,
        &[GraphVerifierError::UnusedControl(entry_control)],
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
        param_types: vec![],
    };
    verify_graph(&graph, &signature, entry).expect("expected a valid graph");
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
        &[GraphVerifierError::ReusedControl(entry_control)],
    );
}

#[test]
fn verify_graph_bad_entry() {
    let mut graph = ValGraph::new();
    let region = graph.create_node(
        NodeKind::Region,
        [],
        [DepValueKind::Control, DepValueKind::PhiSelector],
    );
    let region_ctrl = graph.node_outputs(region)[0];
    create_return(&mut graph, [region_ctrl]);
    check_verify_graph_errors(&graph, region, &[GraphVerifierError::BadEntry(region)]);
}

#[test]
fn verify_graph_bad_entry_multi_err() {
    let mut graph = ValGraph::new();
    let region = graph.create_node(
        NodeKind::Region,
        [],
        [DepValueKind::Control, DepValueKind::PhiSelector],
    );
    let region_ctrl = graph.node_outputs(region)[0];
    let const_val = create_const32(&mut graph);
    let ret = create_return(&mut graph, [region_ctrl, const_val]);

    check_verify_graph_errors(
        &graph,
        region,
        &[
            GraphVerifierError::BadEntry(region),
            GraphVerifierError::BadInputCount {
                node: ret,
                expected: 1,
            },
        ],
    );
}

#[test]
fn verify_graph_misplaced_entry() {
    let mut graph = ValGraph::new();
    let (entry1, entry1_ctrl, []) = create_entry(&mut graph, []);
    let (entry2, entry2_ctrl, []) = create_entry(&mut graph, []);
    let region_ctrl = create_region(&mut graph, [entry1_ctrl, entry2_ctrl]);
    create_return(&mut graph, [region_ctrl]);
    check_verify_graph_errors(
        &graph,
        entry1,
        &[GraphVerifierError::MisplacedEntry(entry2)],
    );
}

#[test]
fn verify_module_loop_function() {
    let mut module = Module::new();
    let (graph, entry) = create_loop_graph();
    let sig = Signature {
        ret_type: Some(Type::I32),
        param_types: vec![Type::I32],
    };
    module.functions.push(FunctionData {
        name: "sum_to_n".to_owned(),
        sig,
        graph,
        entry,
    });
    verify_module(&module).expect("expected a valid module");
}

#[test]
fn verify_module_propagate_graph_error() {
    let mut module = Module::new();

    let func = module.functions.push(FunctionData::new(
        "broken".to_owned(),
        Signature {
            ret_type: None,
            param_types: vec![],
        },
    ));
    let func_data = &module.functions[func];
    let entry_ctrl = func_data.graph.node_outputs(func_data.entry)[0];

    check_verify_module_errors(
        &module,
        &[ModuleVerifierError::Graph {
            function: func,
            error: GraphVerifierError::UnusedControl(entry_ctrl),
        }],
    );
}

#[test]
fn verify_module_duplicate_extern_names() {
    let mut module = Module::new();
    module.extern_functions.push(ExternFunctionData {
        name: "func".to_owned(),
        sig: Signature {
            ret_type: None,
            param_types: vec![],
        },
    });
    module.extern_functions.push(ExternFunctionData {
        name: "func".to_owned(),
        sig: Signature {
            ret_type: Some(Type::I32),
            param_types: vec![],
        },
    });

    check_verify_module_errors(
        &module,
        &[ModuleVerifierError::ReusedFunctionName("func".to_owned())],
    );
}

#[test]
fn verify_module_duplicate_intern_extern_names() {
    let mut module = Module::new();
    module.extern_functions.push(ExternFunctionData {
        name: "func".to_owned(),
        sig: Signature {
            ret_type: None,
            param_types: vec![],
        },
    });

    let f = module.functions.push(FunctionData::new(
        "func".to_owned(),
        Signature {
            ret_type: None,
            param_types: vec![],
        },
    ));
    let f = &mut module.functions[f];
    let graph = &mut f.graph;
    let entry_ctrl = graph.node_outputs(f.entry)[0];
    create_return(graph, [entry_ctrl]);

    check_verify_module_errors(
        &module,
        &[ModuleVerifierError::ReusedFunctionName("func".to_owned())],
    );
}

#[test]
fn verify_module_duplicate_intern_names() {
    let mut module = Module::new();

    let f = module.functions.push(FunctionData::new(
        "func".to_owned(),
        Signature {
            ret_type: None,
            param_types: vec![],
        },
    ));
    let f = &mut module.functions[f];
    let graph = &mut f.graph;
    let entry_ctrl = graph.node_outputs(f.entry)[0];
    create_return(graph, [entry_ctrl]);

    let f = module.functions.push(FunctionData::new(
        "func".to_owned(),
        Signature {
            ret_type: None,
            param_types: vec![],
        },
    ));
    let f = &mut module.functions[f];
    let graph = &mut f.graph;
    let entry_ctrl = graph.node_outputs(f.entry)[0];
    create_return(graph, [entry_ctrl]);

    check_verify_module_errors(
        &module,
        &[ModuleVerifierError::ReusedFunctionName("func".to_owned())],
    );
}

#[test]
fn verify_module_many_duplicate_names() {
    let mut module = Module::new();
    module.extern_functions.push(ExternFunctionData {
        name: "func".to_owned(),
        sig: Signature {
            ret_type: None,
            param_types: vec![],
        },
    });
    module.extern_functions.push(ExternFunctionData {
        name: "func".to_owned(),
        sig: Signature {
            ret_type: Some(Type::I32),
            param_types: vec![],
        },
    });
    module.extern_functions.push(ExternFunctionData {
        name: "func".to_owned(),
        sig: Signature {
            ret_type: Some(Type::I32),
            param_types: vec![],
        },
    });

    check_verify_module_errors(
        &module,
        &[ModuleVerifierError::ReusedFunctionName("func".to_owned())],
    );
}

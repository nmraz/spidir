use crate::{
    module::{FunctionBody, FunctionData, FunctionMetadata, Signature},
    node::Type,
    test_utils::{create_const32, create_entry, create_region, create_return},
};

use super::*;

#[track_caller]
fn check_verify_func_errors(graph: ValGraph, entry: Node, expected_errors: &[GraphVerifierError]) {
    let func = FunctionData {
        metadata: FunctionMetadata {
            name: "func".to_owned(),
            sig: Signature {
                ret_type: None,
                param_types: vec![],
            },
        },
        body: FunctionBody { graph, entry },
    };
    let mut module = Module::new();
    let func = module.functions.push(func);
    assert_eq!(
        verify_func(&module, &module.functions[func]).unwrap_err(),
        expected_errors
    );
}

#[track_caller]
fn check_verify_module_errors(module: &Module, expected_errors: &[ModuleVerifierError]) {
    assert_eq!(verify_module(module).unwrap_err(), expected_errors);
}

#[test]
fn verify_func_bad_entry() {
    let mut graph = ValGraph::new();
    let region = graph.create_node(
        NodeKind::Region,
        [],
        [DepValueKind::Control, DepValueKind::PhiSelector],
    );
    let region_ctrl = graph.node_outputs(region)[0];
    create_return(&mut graph, [region_ctrl]);
    check_verify_func_errors(graph, region, &[GraphVerifierError::BadEntry(region)]);
}

#[test]
fn verify_func_bad_entry_multi_err() {
    let mut graph = ValGraph::new();
    let region = graph.create_node(
        NodeKind::Region,
        [],
        [DepValueKind::Control, DepValueKind::PhiSelector],
    );
    let region_ctrl = graph.node_outputs(region)[0];
    let const_val = create_const32(&mut graph);
    let ret = create_return(&mut graph, [region_ctrl, const_val]);

    check_verify_func_errors(
        graph,
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
fn verify_func_misplaced_entry() {
    let mut graph = ValGraph::new();
    let (entry1, entry1_ctrl, []) = create_entry(&mut graph, []);
    let (entry2, entry2_ctrl, []) = create_entry(&mut graph, []);
    let region_ctrl = create_region(&mut graph, [entry1_ctrl, entry2_ctrl]);
    create_return(&mut graph, [region_ctrl]);
    check_verify_func_errors(graph, entry1, &[GraphVerifierError::MisplacedEntry(entry2)]);
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
    let body = &module.functions[func].body;
    let entry_ctrl = body.graph.node_outputs(body.entry)[0];

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
    module.extern_functions.push(FunctionMetadata {
        name: "func".to_owned(),
        sig: Signature {
            ret_type: None,
            param_types: vec![],
        },
    });
    module.extern_functions.push(FunctionMetadata {
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
    module.extern_functions.push(FunctionMetadata {
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
    let body = &mut module.functions[f].body;
    let graph = &mut body.graph;
    let entry_ctrl = graph.node_outputs(body.entry)[0];
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
    let body = &mut module.functions[f].body;
    let graph = &mut body.graph;
    let entry_ctrl = graph.node_outputs(body.entry)[0];
    create_return(graph, [entry_ctrl]);

    let f = module.functions.push(FunctionData::new(
        "func".to_owned(),
        Signature {
            ret_type: None,
            param_types: vec![],
        },
    ));
    let body = &mut module.functions[f].body;
    let graph = &mut body.graph;
    let entry_ctrl = graph.node_outputs(body.entry)[0];
    create_return(graph, [entry_ctrl]);

    check_verify_module_errors(
        &module,
        &[ModuleVerifierError::ReusedFunctionName("func".to_owned())],
    );
}

#[test]
fn verify_module_many_duplicate_names() {
    let mut module = Module::new();
    module.extern_functions.push(FunctionMetadata {
        name: "func".to_owned(),
        sig: Signature {
            ret_type: None,
            param_types: vec![],
        },
    });
    module.extern_functions.push(FunctionMetadata {
        name: "func".to_owned(),
        sig: Signature {
            ret_type: Some(Type::I32),
            param_types: vec![],
        },
    });
    module.extern_functions.push(FunctionMetadata {
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

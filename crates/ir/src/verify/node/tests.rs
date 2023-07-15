use crate::{
    node::{BitwiseF64, IcmpKind, NodeKind, Type},
    test_utils::{create_const32, create_const64, create_entry, create_region, create_return},
    verify::verify_graph,
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

#[track_caller]
fn check_verify_graph_ok(
    graph: &ValGraph,
    entry: Node,
    param_types: &[Type],
    ret_type: Option<Type>,
) {
    let signature = Signature {
        ret_type,
        param_types: param_types.to_owned(),
    };
    verify_graph(graph, &signature, entry).expect("expected a valid graph");
}

fn all_integer_types() -> Vec<DepValueKind> {
    vec![
        DepValueKind::Value(Type::I32),
        DepValueKind::Value(Type::I64),
    ]
}

#[test]
fn verify_graph_add_function() {
    let mut graph = ValGraph::new();

    let (entry, control_value, [param1, param2]) = create_entry(&mut graph, [Type::I32, Type::I32]);

    let add = graph.create_node(
        NodeKind::Iadd,
        [param1, param2],
        [DepValueKind::Value(Type::I32)],
    );
    let add_res = graph.node_outputs(add)[0];
    create_return(&mut graph, [control_value, add_res]);

    check_verify_graph_ok(&graph, entry, &[Type::I32, Type::I32], Some(Type::I32));
}

#[test]
fn verify_iadd_bad_input_count() {
    let mut graph = ValGraph::new();

    let empty_iadd = graph.create_node(NodeKind::Iadd, [], [DepValueKind::Value(Type::I32)]);
    check_verify_node_kind(
        &graph,
        empty_iadd,
        GraphVerifierError::BadInputCount {
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
        GraphVerifierError::BadInputCount {
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
        GraphVerifierError::BadOutputKind {
            value: iadd_output,
            expected: all_integer_types(),
        },
    );
}

#[test]
fn verify_iadd_input_kinds() {
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
        GraphVerifierError::BadInputKind {
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
        GraphVerifierError::BadInputKind {
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

            let (entry, control_value, [shift_input, shift_amount]) =
                create_entry(&mut graph, [shift_input_ty, shift_amount_ty]);

            let shl = graph.create_node(
                NodeKind::Shl,
                [shift_input, shift_amount],
                [DepValueKind::Value(shift_input_ty)],
            );
            let shl_res = graph.node_outputs(shl)[0];
            create_return(&mut graph, [control_value, shl_res]);

            check_verify_graph_ok(
                &graph,
                entry,
                &[shift_input_ty, shift_amount_ty],
                Some(shift_input_ty),
            );
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
        GraphVerifierError::BadInputCount {
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
        GraphVerifierError::BadInputCount {
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
        GraphVerifierError::BadOutputKind {
            value: shl_output,
            expected: all_integer_types(),
        },
    );
}

#[test]
fn verify_shl_input_kinds() {
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
        GraphVerifierError::BadInputKind {
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
        GraphVerifierError::BadInputKind {
            node: shl_non_int,
            input: 1,
            expected: all_integer_types(),
        },
    );
}

#[test]
fn verify_graph_div_function_ok() {
    for kind in [NodeKind::Sdiv, NodeKind::Udiv] {
        for ty in [Type::I32, Type::I64] {
            let mut graph = ValGraph::new();

            let (entry, control_value, [shift_input, shift_amount]) =
                create_entry(&mut graph, [ty, ty]);

            let div = graph.create_node(
                kind,
                [control_value, shift_input, shift_amount],
                [DepValueKind::Control, DepValueKind::Value(ty)],
            );
            let div_outputs = graph.node_outputs(div);
            let div_control = div_outputs[0];
            let div_res = div_outputs[1];
            create_return(&mut graph, [div_control, div_res]);

            check_verify_graph_ok(&graph, entry, &[ty, ty], Some(ty));
        }
    }
}

#[test]
fn verify_div_input_count() {
    let mut graph = ValGraph::new();
    let div = graph.create_node(
        NodeKind::Sdiv,
        [],
        [DepValueKind::Control, DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        div,
        GraphVerifierError::BadInputCount {
            node: div,
            expected: 3,
        },
    );
}

#[test]
fn verify_div_output_count() {
    let mut graph = ValGraph::new();
    let (_, entry_control, [param1, param2]) = create_entry(&mut graph, [Type::I32, Type::I64]);
    let div = graph.create_node(NodeKind::Sdiv, [entry_control, param1, param2], []);
    check_verify_node_kind(
        &graph,
        div,
        GraphVerifierError::BadOutputCount {
            node: div,
            expected: 2,
        },
    );
}

#[test]
fn verify_div_input_kinds() {
    let mut graph = ValGraph::new();
    let (_, entry_control, [val32, val64]) = create_entry(&mut graph, [Type::I32, Type::I64]);

    let mistyped_div = graph.create_node(
        NodeKind::Sdiv,
        [entry_control, val32, val64],
        [DepValueKind::Control, DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        mistyped_div,
        GraphVerifierError::BadInputKind {
            node: mistyped_div,
            input: 2,
            expected: vec![DepValueKind::Value(Type::I32)],
        },
    );

    let non_ctrl_div = graph.create_node(
        NodeKind::Sdiv,
        [val64, val32, val32],
        [DepValueKind::Control, DepValueKind::Value(Type::I32)],
    );
    check_verify_node_kind(
        &graph,
        non_ctrl_div,
        GraphVerifierError::BadInputKind {
            node: non_ctrl_div,
            input: 0,
            expected: vec![DepValueKind::Control],
        },
    );
}

#[test]
fn verify_div_output_kinds() {
    let mut graph = ValGraph::new();
    let (_, entry_control, [param]) = create_entry(&mut graph, [Type::I32]);

    let non_int_div = graph.create_node(
        NodeKind::Sdiv,
        [entry_control, param, param],
        [DepValueKind::Control, DepValueKind::Value(Type::Ptr)],
    );
    check_verify_node_kind(
        &graph,
        non_int_div,
        GraphVerifierError::BadOutputKind {
            value: graph.node_outputs(non_int_div)[1],
            expected: vec![
                DepValueKind::Value(Type::I32),
                DepValueKind::Value(Type::I64),
            ],
        },
    );

    let non_ctrl_div = graph.create_node(
        NodeKind::Sdiv,
        [entry_control, param, param],
        [
            DepValueKind::Value(Type::I32),
            DepValueKind::Value(Type::I32),
        ],
    );
    check_verify_node_kind(
        &graph,
        non_ctrl_div,
        GraphVerifierError::BadOutputKind {
            value: graph.node_outputs(non_ctrl_div)[0],
            expected: vec![DepValueKind::Control],
        },
    );
}

#[test]
fn verify_graph_icmp_function_ok() {
    for input_ty in [Type::I32, Type::I64, Type::Ptr] {
        for output_ty in [Type::I32, Type::I64] {
            let mut graph = ValGraph::new();
            let (entry, control_value, [param1, param2]) =
                create_entry(&mut graph, [input_ty, input_ty]);

            let icmp = graph.create_node(
                NodeKind::Icmp(IcmpKind::Eq),
                [param1, param2],
                [DepValueKind::Value(output_ty)],
            );
            let icmp_res = graph.node_outputs(icmp)[0];
            create_return(&mut graph, [control_value, icmp_res]);

            check_verify_graph_ok(&graph, entry, &[input_ty, input_ty], Some(output_ty));
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
        GraphVerifierError::BadInputCount {
            node: empty_icmp,
            expected: 2,
        },
    );
}

#[test]
fn verify_icmp_input_kinds() {
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
        GraphVerifierError::BadInputKind {
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
        GraphVerifierError::BadInputKind {
            node: mismatched_icmp,
            input: 1,
            expected: vec![DepValueKind::Value(Type::I32)],
        },
    );
}

#[test]
fn verify_icmp_output_kinds() {
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
        GraphVerifierError::BadOutputKind {
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
        GraphVerifierError::BadInputCount {
            node: iconst,
            expected: 0,
        },
    );
}

#[test]
fn verify_iconst_output_kinds() {
    let mut graph = ValGraph::new();

    let iconst_ctrl = graph.create_node(NodeKind::IConst(3), [], [DepValueKind::Control]);
    check_verify_node_kind(
        &graph,
        iconst_ctrl,
        GraphVerifierError::BadOutputKind {
            value: graph.node_outputs(iconst_ctrl)[0],
            expected: all_integer_types(),
        },
    );

    let iconst_ptr = graph.create_node(NodeKind::IConst(3), [], [DepValueKind::Value(Type::Ptr)]);
    check_verify_node_kind(
        &graph,
        iconst_ptr,
        GraphVerifierError::BadOutputKind {
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
    check_verify_node_kind(
        &graph,
        not_u32,
        GraphVerifierError::ConstantOutOfRange(not_u32),
    );
}

#[test]
fn verify_fconst_input_count() {
    let mut graph = ValGraph::new();
    let const_val = create_const32(&mut graph);
    let fconst = graph.create_node(
        NodeKind::FConst(BitwiseF64(3.0)),
        [const_val],
        [DepValueKind::Value(Type::F64)],
    );
    check_verify_node_kind(
        &graph,
        fconst,
        GraphVerifierError::BadInputCount {
            node: fconst,
            expected: 0,
        },
    );
}

#[test]
fn verify_fconst_output_kinds() {
    let mut graph = ValGraph::new();

    let fconst_ctrl = graph.create_node(
        NodeKind::FConst(BitwiseF64(3.0)),
        [],
        [DepValueKind::Control],
    );
    check_verify_node_kind(
        &graph,
        fconst_ctrl,
        GraphVerifierError::BadOutputKind {
            value: graph.node_outputs(fconst_ctrl)[0],
            expected: vec![DepValueKind::Value(Type::F64)],
        },
    );
}

#[test]
fn verify_graph_ptroff_function_ok() {
    let mut graph = ValGraph::new();
    let (entry, control_value, [ptr, offset]) = create_entry(&mut graph, [Type::Ptr, Type::I64]);
    let ptroff = graph.create_node(
        NodeKind::PtrOff,
        [ptr, offset],
        [DepValueKind::Value(Type::Ptr)],
    );
    let ptroff_res = graph.node_outputs(ptroff)[0];
    create_return(&mut graph, [control_value, ptroff_res]);
    check_verify_graph_ok(&graph, entry, &[Type::Ptr, Type::I64], Some(Type::Ptr));
}

#[test]
fn verify_ptroff_input_kinds() {
    let mut graph = ValGraph::new();
    let (_entry, _control_value, [ptr, offset]) = create_entry(&mut graph, [Type::Ptr, Type::I64]);

    let ptroff1 = graph.create_node(
        NodeKind::PtrOff,
        [offset, offset],
        [DepValueKind::Value(Type::Ptr)],
    );
    check_verify_node_kind(
        &graph,
        ptroff1,
        GraphVerifierError::BadInputKind {
            node: ptroff1,
            input: 0,
            expected: vec![DepValueKind::Value(Type::Ptr)],
        },
    );

    let ptroff2 = graph.create_node(
        NodeKind::PtrOff,
        [ptr, ptr],
        [DepValueKind::Value(Type::Ptr)],
    );
    check_verify_node_kind(
        &graph,
        ptroff2,
        GraphVerifierError::BadInputKind {
            node: ptroff2,
            input: 1,
            expected: vec![
                DepValueKind::Value(Type::I32),
                DepValueKind::Value(Type::I64),
            ],
        },
    );
}

#[test]
fn verify_ptroff_output_kinds() {
    let mut graph = ValGraph::new();
    let (_entry, _control_value, [ptr, offset]) = create_entry(&mut graph, [Type::Ptr, Type::I64]);
    let ptroff = graph.create_node(
        NodeKind::PtrOff,
        [ptr, offset],
        [DepValueKind::Value(Type::F64)],
    );
    check_verify_node_kind(
        &graph,
        ptroff,
        GraphVerifierError::BadOutputKind {
            value: graph.node_outputs(ptroff)[0],
            expected: vec![DepValueKind::Value(Type::Ptr)],
        },
    );
}

#[test]
fn verify_graph_load_function_ok() {
    for output_ty in [Type::I32, Type::I64, Type::Ptr] {
        let mut graph = ValGraph::new();

        let (entry, control_value, [param1]) = create_entry(&mut graph, [Type::Ptr]);

        let load = graph.create_node(
            NodeKind::Load,
            [control_value, param1],
            [DepValueKind::Control, DepValueKind::Value(output_ty)],
        );
        let load_control = graph.node_outputs(load)[0];

        let load_res = graph.node_outputs(load)[1];
        create_return(&mut graph, [load_control, load_res]);
        check_verify_graph_ok(&graph, entry, &[Type::Ptr], Some(output_ty));
    }
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
fn verify_graph_store_function_ok() {
    for input_ty in [Type::I32, Type::I64, Type::Ptr, Type::F64] {
        let mut graph = ValGraph::new();

        let (entry, control_value, [ptr, value]) = create_entry(&mut graph, [Type::Ptr, input_ty]);

        let store = graph.create_node(
            NodeKind::Store,
            [control_value, value, ptr],
            [DepValueKind::Control],
        );
        let store_control = graph.node_outputs(store)[0];

        create_return(&mut graph, [store_control]);
        check_verify_graph_ok(&graph, entry, &[Type::Ptr, input_ty], None);
    }
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
fn verify_graph_stack_slot_function_ok() {
    let mut graph = ValGraph::new();

    let (entry, entry_ctrl, [input]) = create_entry(&mut graph, [Type::I32]);
    let slot_addr_node = graph.create_node(
        NodeKind::StackSlot { size: 4, align: 4 },
        [],
        [DepValueKind::Value(Type::Ptr)],
    );
    let slot_addr = graph.node_outputs(slot_addr_node)[0];
    let store = graph.create_node(
        NodeKind::Store,
        [entry_ctrl, input, slot_addr],
        [DepValueKind::Control],
    );
    let store_ctrl = graph.node_outputs(store)[0];
    create_return(&mut graph, [store_ctrl]);
    check_verify_graph_ok(&graph, entry, &[Type::I32], None);
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

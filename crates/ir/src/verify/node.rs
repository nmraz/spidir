use core::array;

use alloc::{borrow::ToOwned, vec::Vec};

use crate::{
    module::{FunctionData, Module},
    node::{DepValueKind, NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
};

use super::GraphVerifierError;

pub fn verify_node_kind(
    _module: &Module,
    func: &FunctionData,
    node: Node,
    errors: &mut Vec<GraphVerifierError>,
) {
    let graph = &func.graph;
    match graph.node_kind(node) {
        NodeKind::Entry => verify_entry(func, node, errors),
        NodeKind::Return => verify_return(func, node, errors),
        NodeKind::Region => verify_region(graph, node, errors),
        NodeKind::Phi => verify_phi(graph, node, errors),
        NodeKind::IConst(val) => verify_iconst(graph, node, *val, errors),
        NodeKind::Iadd => verify_int_binop(graph, node, errors),
        NodeKind::Isub => verify_int_binop(graph, node, errors),
        NodeKind::And => verify_int_binop(graph, node, errors),
        NodeKind::Or => verify_int_binop(graph, node, errors),
        NodeKind::Xor => verify_int_binop(graph, node, errors),
        NodeKind::Shl => verify_shift(graph, node, errors),
        NodeKind::Lshr => verify_shift(graph, node, errors),
        NodeKind::Ashr => verify_shift(graph, node, errors),
        NodeKind::Imul => verify_int_binop(graph, node, errors),
        NodeKind::Sdiv => verify_int_div(graph, node, errors),
        NodeKind::Udiv => verify_int_div(graph, node, errors),
        NodeKind::Icmp(_) => verify_icmp(graph, node, errors),
        NodeKind::FConst(_) => verify_fconst(graph, node, errors),
        NodeKind::PtrOff => verify_ptroff(graph, node, errors),
        NodeKind::Load => verify_load(graph, node, errors),
        NodeKind::Store => verify_store(graph, node, errors),
        NodeKind::StackSlot { align, .. } => verify_stack_slot(graph, node, *align, errors),
        NodeKind::BrCond => verify_brcond(graph, node, errors),
        NodeKind::Call(_) => {}
    }
}

fn verify_entry(func: &FunctionData, node: Node, errors: &mut Vec<GraphVerifierError>) {
    if node != func.entry {
        errors.push(GraphVerifierError::MisplacedEntry(node));
        return;
    }

    let expected_output_count = func.sig.param_types.len() + 1;
    let outputs = func.graph.node_outputs(node);
    if outputs.len() != expected_output_count {
        errors.push(GraphVerifierError::BadOutputCount {
            node,
            expected: expected_output_count as u32,
        });
        return;
    }

    let _ = verify_output_kind(&func.graph, outputs[0], &[DepValueKind::Control], errors);
    for (output, &expected_ty) in outputs.into_iter().skip(1).zip(&func.sig.param_types) {
        let _ = verify_output_kind(
            &func.graph,
            output,
            &[DepValueKind::Value(expected_ty)],
            errors,
        );
    }
}

fn verify_return(func: &FunctionData, node: Node, errors: &mut Vec<GraphVerifierError>) {
    match func.sig.ret_type {
        Some(ret_type) => {
            let Ok([]) = verify_node_arity(&func.graph, node, 2, errors) else {
                return;
            };
            let _ = verify_input_kind(&func.graph, node, 0, &[DepValueKind::Control], errors);
            let _ = verify_input_kind(
                &func.graph,
                node,
                1,
                &[DepValueKind::Value(ret_type)],
                errors,
            );
        }
        None => {
            let Ok([]) = verify_node_arity(&func.graph, node, 1, errors) else {
                return;
            };
            let _ = verify_input_kind(&func.graph, node, 0, &[DepValueKind::Control], errors);
        }
    }
}

fn verify_region(graph: &ValGraph, node: Node, errors: &mut Vec<GraphVerifierError>) {
    let Ok([ctrl, phisel]) = verify_outputs(graph, node, errors) else {
        return;
    };
    let _ = verify_output_kind(graph, ctrl, &[DepValueKind::Control], errors);
    let _ = verify_output_kind(graph, phisel, &[DepValueKind::PhiSelector], errors);

    for input in 0..graph.node_inputs(node).len() as u32 {
        let _ = verify_input_kind(graph, node, input, &[DepValueKind::Control], errors);
    }
}

fn verify_phi(graph: &ValGraph, node: Node, errors: &mut Vec<GraphVerifierError>) {
    let Ok([result]) = verify_outputs(graph, node, errors) else {
        return;
    };

    let inputs = graph.node_inputs(node);
    if inputs.is_empty() {
        errors.push(GraphVerifierError::BadInputCount { node, expected: 1 });
        return;
    }

    if verify_input_kind(graph, node, 0, &[DepValueKind::PhiSelector], errors).is_err() {
        return;
    }

    let phisel = inputs[0];
    let phi_region = graph.value_def(phisel).0;

    let expected_input_count = graph.node_inputs(phi_region).len() + 1;
    if inputs.len() != expected_input_count {
        errors.push(GraphVerifierError::BadInputCount {
            node,
            expected: expected_input_count as u32,
        });
        return;
    }

    if verify_output_kind(graph, result, ALL_VALUE_TYPES, errors).is_err() {
        return;
    }

    let result_kind = graph.value_kind(result);
    for input in 1..inputs.len() as u32 {
        let _ = verify_input_kind(graph, node, input, &[result_kind], errors);
    }
}

fn verify_iconst(graph: &ValGraph, node: Node, val: u64, errors: &mut Vec<GraphVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 0, errors) else {
        return;
    };
    if verify_integer_output_kind(graph, result, errors).is_err() {
        return;
    }

    let result_type = graph.value_kind(result).as_value().expect("type verified");
    match result_type {
        Type::I32 => {
            if val > u32::MAX as u64 {
                errors.push(GraphVerifierError::ConstantOutOfRange(node));
            }
        }
        Type::I64 => {
            // Value is already a `u64`
        }
        _ => panic!("type should have been verified here"),
    }
}

fn verify_fconst(graph: &ValGraph, node: Node, errors: &mut Vec<GraphVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 0, errors) else {
        return;
    };
    let _ = verify_output_kind(graph, result, &[DepValueKind::Value(Type::F64)], errors);
}

fn verify_int_binop(graph: &ValGraph, node: Node, errors: &mut Vec<GraphVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 2, errors) else {
        return;
    };

    if verify_integer_output_kind(graph, result, errors).is_err() {
        return;
    }

    let result_kind = graph.value_kind(result);
    let _ = verify_input_kind(graph, node, 0, &[result_kind], errors);
    let _ = verify_input_kind(graph, node, 1, &[result_kind], errors);
}

fn verify_shift(graph: &ValGraph, node: Node, errors: &mut Vec<GraphVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 2, errors) else {
        return;
    };

    if verify_integer_output_kind(graph, result, errors).is_err()
        || verify_integer_input_kind(graph, node, 1, errors).is_err()
    {
        return;
    }

    let result_kind = graph.value_kind(result);
    let _ = verify_input_kind(graph, node, 0, &[result_kind], errors);
}

fn verify_int_div(graph: &ValGraph, node: Node, errors: &mut Vec<GraphVerifierError>) {
    let Ok([out_ctrl, result]) = verify_node_arity(graph, node, 3, errors) else {
        return;
    };
    let _ = verify_input_kind(graph, node, 0, &[DepValueKind::Control], errors);
    let _ = verify_output_kind(graph, out_ctrl, &[DepValueKind::Control], errors);

    if verify_integer_output_kind(graph, result, errors).is_err() {
        return;
    }

    let result_kind = graph.value_kind(result);
    let _ = verify_input_kind(graph, node, 1, &[result_kind], errors);
    let _ = verify_input_kind(graph, node, 2, &[result_kind], errors);
}

fn verify_icmp(graph: &ValGraph, node: Node, errors: &mut Vec<GraphVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 2, errors) else {
        return;
    };

    let _ = verify_integer_output_kind(graph, result, errors);

    if verify_input_kind(
        graph,
        node,
        0,
        &[Type::I32, Type::I64, Type::Ptr].map(DepValueKind::Value),
        errors,
    )
    .is_err()
    {
        return;
    }

    let _ = verify_input_kind(
        graph,
        node,
        1,
        &[graph.value_kind(graph.node_inputs(node)[0])],
        errors,
    );
}

fn verify_ptroff(graph: &ValGraph, node: Node, errors: &mut Vec<GraphVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 2, errors) else {
        return;
    };
    let _ = verify_output_kind(graph, result, &[DepValueKind::Value(Type::Ptr)], errors);
    let _ = verify_input_kind(graph, node, 0, &[DepValueKind::Value(Type::Ptr)], errors);
    let _ = verify_input_kind(graph, node, 1, &[DepValueKind::Value(Type::I64)], errors);
}

fn verify_load(graph: &ValGraph, node: Node, errors: &mut Vec<GraphVerifierError>) {
    let Ok([out_ctrl, result]) = verify_node_arity(graph, node, 2, errors) else {
        return;
    };
    let _ = verify_output_kind(graph, out_ctrl, &[DepValueKind::Control], errors);
    let _ = verify_output_kind(graph, result, ALL_VALUE_TYPES, errors);
    let _ = verify_input_kind(graph, node, 0, &[DepValueKind::Control], errors);
    let _ = verify_input_kind(graph, node, 1, &[DepValueKind::Value(Type::Ptr)], errors);
}

fn verify_store(graph: &ValGraph, node: Node, errors: &mut Vec<GraphVerifierError>) {
    let Ok([out_ctrl]) = verify_node_arity(graph, node, 3, errors) else {
        return;
    };
    let _ = verify_output_kind(graph, out_ctrl, &[DepValueKind::Control], errors);
    let _ = verify_input_kind(graph, node, 0, &[DepValueKind::Control], errors);
    let _ = verify_input_kind(graph, node, 1, ALL_VALUE_TYPES, errors);
    let _ = verify_input_kind(graph, node, 2, &[DepValueKind::Value(Type::Ptr)], errors);
}

fn verify_stack_slot(
    graph: &ValGraph,
    node: Node,
    align: u32,
    errors: &mut Vec<GraphVerifierError>,
) {
    let Ok([result]) = verify_node_arity(graph, node, 0, errors) else {
        return;
    };
    let _ = verify_output_kind(graph, result, &[DepValueKind::Value(Type::Ptr)], errors);
    if !align.is_power_of_two() {
        errors.push(GraphVerifierError::BadStackSlotAlign(node));
    }
}

fn verify_brcond(graph: &ValGraph, node: Node, errors: &mut Vec<GraphVerifierError>) {
    let Ok([taken_ctrl, nontaken_ctrl]) = verify_node_arity(graph, node, 2, errors) else {
        return;
    };
    let _ = verify_output_kind(graph, taken_ctrl, &[DepValueKind::Control], errors);
    let _ = verify_output_kind(graph, nontaken_ctrl, &[DepValueKind::Control], errors);
    let _ = verify_input_kind(graph, node, 0, &[DepValueKind::Control], errors);
    let _ = verify_integer_input_kind(graph, node, 1, errors);
}

fn verify_node_arity<const O: usize>(
    graph: &ValGraph,
    node: Node,
    input_count: u32,
    errors: &mut Vec<GraphVerifierError>,
) -> Result<[DepValue; O], ()> {
    let inputs = graph.node_inputs(node);

    if inputs.len() != input_count as usize {
        errors.push(GraphVerifierError::BadInputCount {
            node,
            expected: input_count,
        });
        return Err(());
    }

    verify_outputs(graph, node, errors)
}

fn verify_outputs<const O: usize>(
    graph: &ValGraph,
    node: Node,
    errors: &mut Vec<GraphVerifierError>,
) -> Result<[DepValue; O], ()> {
    let outputs = graph.node_outputs(node);
    if outputs.len() != O {
        errors.push(GraphVerifierError::BadOutputCount {
            node,
            expected: O as u32,
        });
        return Err(());
    }

    Ok(array::from_fn(|i| outputs[i]))
}

fn verify_integer_input_kind(
    graph: &ValGraph,
    node: Node,
    input: u32,
    errors: &mut Vec<GraphVerifierError>,
) -> Result<(), ()> {
    verify_input_kind(graph, node, input, ALL_INTEGER_TYPES, errors)
}

fn verify_integer_output_kind(
    graph: &ValGraph,
    value: DepValue,
    errors: &mut Vec<GraphVerifierError>,
) -> Result<(), ()> {
    verify_output_kind(graph, value, ALL_INTEGER_TYPES, errors)
}

fn verify_input_kind(
    graph: &ValGraph,
    node: Node,
    input: u32,
    expected_kinds: &[DepValueKind],
    errors: &mut Vec<GraphVerifierError>,
) -> Result<(), ()> {
    if !expected_kinds.contains(&graph.value_kind(graph.node_inputs(node)[input as usize])) {
        errors.push(GraphVerifierError::BadInputKind {
            node,
            input,
            expected: expected_kinds.to_owned(),
        });
        return Err(());
    }

    Ok(())
}

fn verify_output_kind(
    graph: &ValGraph,
    value: DepValue,
    expected_kinds: &[DepValueKind],
    errors: &mut Vec<GraphVerifierError>,
) -> Result<(), ()> {
    if !expected_kinds.contains(&graph.value_kind(value)) {
        errors.push(GraphVerifierError::BadOutputKind {
            value,
            expected: expected_kinds.to_owned(),
        });
        return Err(());
    }
    Ok(())
}

const ALL_VALUE_TYPES: &[DepValueKind] = &[
    DepValueKind::Value(Type::I32),
    DepValueKind::Value(Type::I64),
    DepValueKind::Value(Type::F64),
    DepValueKind::Value(Type::Ptr),
];

const ALL_INTEGER_TYPES: &[DepValueKind] = &[
    DepValueKind::Value(Type::I32),
    DepValueKind::Value(Type::I64),
];

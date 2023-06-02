use core::array;

use alloc::{borrow::ToOwned, vec::Vec};

use crate::{
    node::{DepValueKind, NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
    valwalk::LiveNodeInfo,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerifierError {
    UnusedControl(DepValue),
    ReusedControl(DepValue),
    BadInputCount {
        node: Node,
        expected: u32,
    },
    BadOutputCount {
        node: Node,
        expected: u32,
    },
    BadInputKind {
        node: Node,
        input: u32,
        expected: Vec<DepValueKind>,
    },
    BadOutputKind {
        value: DepValue,
        expected: Vec<DepValueKind>,
    },
    ConstantOutOfRange(Node),
}

pub fn verify_graph(graph: &ValGraph, entry: Node) -> Result<(), Vec<VerifierError>> {
    let mut errors = Vec::new();

    for node in LiveNodeInfo::compute(graph, entry).iter_live_nodes() {
        verify_node_kind(graph, node, &mut errors);
        verify_control_outputs(graph, node, &mut errors);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn verify_control_outputs(graph: &ValGraph, node: Node, errors: &mut Vec<VerifierError>) {
    for output in graph.node_outputs(node) {
        if graph.value_kind(output) != DepValueKind::Control {
            continue;
        }

        let mut uses = graph.value_uses(output);
        if uses.next().is_none() {
            errors.push(VerifierError::UnusedControl(output));
            continue;
        }

        if uses.next().is_some() {
            errors.push(VerifierError::ReusedControl(output));
        }
    }
}

fn verify_node_kind(graph: &ValGraph, node: Node, errors: &mut Vec<VerifierError>) {
    match graph.node_kind(node) {
        NodeKind::Entry => {}
        NodeKind::Return => {}
        NodeKind::Region => verify_region(graph, node, errors),
        NodeKind::Phi => verify_phi(graph, node, errors),
        NodeKind::IConst(val) => verify_iconst(graph, node, *val, errors),
        NodeKind::Iadd => verify_int_binop(graph, node, errors),
        NodeKind::Isub => verify_int_binop(graph, node, errors),
        NodeKind::And => verify_int_binop(graph, node, errors),
        NodeKind::Or => verify_int_binop(graph, node, errors),
        NodeKind::Xor => verify_int_binop(graph, node, errors),
        NodeKind::Shl => verify_shift_op(graph, node, errors),
        NodeKind::Lshr => verify_shift_op(graph, node, errors),
        NodeKind::Ashr => verify_shift_op(graph, node, errors),
        NodeKind::Imul => verify_int_binop(graph, node, errors),
        NodeKind::Sdiv => verify_div_op(graph, node, errors),
        NodeKind::Udiv => verify_div_op(graph, node, errors),
        NodeKind::Icmp(_) => verify_icmp(graph, node, errors),
        NodeKind::FConst(_) => verify_fconst(graph, node, errors),
        NodeKind::Load => verify_load(graph, node, errors),
        NodeKind::Store => {}
        NodeKind::BrCond => {}
        NodeKind::Call(_) => {}
    }
}

fn verify_region(graph: &ValGraph, node: Node, errors: &mut Vec<VerifierError>) {
    let Ok([ctrl, phisel]) = verify_outputs(graph, node, errors) else { return };
    let _ = verify_output_kind(graph, ctrl, &[DepValueKind::Control], errors);
    let _ = verify_output_kind(graph, phisel, &[DepValueKind::PhiSelector], errors);

    for input in 0..graph.node_inputs(node).len() as u32 {
        let _ = verify_input_kind(graph, node, input, &[DepValueKind::Control], errors);
    }
}

fn verify_phi(graph: &ValGraph, node: Node, errors: &mut Vec<VerifierError>) {
    let Ok([result]) = verify_outputs(graph, node, errors) else { return };

    let inputs = graph.node_inputs(node);
    if inputs.is_empty() {
        errors.push(VerifierError::BadInputCount { node, expected: 1 });
        return;
    }

    if verify_input_kind(graph, node, 0, &[DepValueKind::PhiSelector], errors).is_err() {
        return;
    }

    let phisel = inputs[0];
    let phi_region = graph.value_def(phisel).0;

    let expected_input_count = graph.node_inputs(phi_region).len() + 1;
    if inputs.len() != expected_input_count {
        errors.push(VerifierError::BadInputCount {
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

fn verify_iconst(graph: &ValGraph, node: Node, val: u64, errors: &mut Vec<VerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 0, errors) else { return };
    if verify_integer_output_kind(graph, result, errors).is_err() {
        return;
    }

    let result_type = graph.value_kind(result).as_value().expect("type verified");
    match result_type {
        Type::I32 => {
            if val > u32::MAX as u64 {
                errors.push(VerifierError::ConstantOutOfRange(node));
            }
        }
        Type::I64 => {
            // Value is already a `u64`
        }
        _ => panic!("type should have been verified here"),
    }
}

fn verify_fconst(graph: &ValGraph, node: Node, errors: &mut Vec<VerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 0, errors) else { return };
    let _ = verify_output_kind(graph, result, &[DepValueKind::Value(Type::F64)], errors);
}

fn verify_int_binop(graph: &ValGraph, node: Node, errors: &mut Vec<VerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 2, errors) else { return };

    if verify_integer_output_kind(graph, result, errors).is_err() {
        return;
    }

    let result_kind = graph.value_kind(result);
    let _ = verify_input_kind(graph, node, 0, &[result_kind], errors);
    let _ = verify_input_kind(graph, node, 1, &[result_kind], errors);
}

fn verify_shift_op(graph: &ValGraph, node: Node, errors: &mut Vec<VerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 2, errors) else { return };

    if verify_integer_output_kind(graph, result, errors).is_err()
        || verify_integer_input_kind(graph, node, 1, errors).is_err()
    {
        return;
    }

    let result_kind = graph.value_kind(result);
    let _ = verify_input_kind(graph, node, 0, &[result_kind], errors);
}

fn verify_div_op(graph: &ValGraph, node: Node, errors: &mut Vec<VerifierError>) {
    let Ok([out_ctrl, result]) = verify_node_arity(graph, node, 3, errors) else { return };
    let _ = verify_input_kind(graph, node, 0, &[DepValueKind::Control], errors);
    let _ = verify_output_kind(graph, out_ctrl, &[DepValueKind::Control], errors);

    if verify_integer_output_kind(graph, result, errors).is_err() {
        return;
    }

    let result_kind = graph.value_kind(result);
    let _ = verify_input_kind(graph, node, 1, &[result_kind], errors);
    let _ = verify_input_kind(graph, node, 2, &[result_kind], errors);
}

fn verify_icmp(graph: &ValGraph, node: Node, errors: &mut Vec<VerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 2, errors) else { return };

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

fn verify_load(graph: &ValGraph, node: Node, errors: &mut Vec<VerifierError>) {
    let Ok([out_ctrl, result]) = verify_node_arity(graph, node, 2, errors) else { return };
    let _ = verify_output_kind(graph, out_ctrl, &[DepValueKind::Control], errors);
    let _ = verify_output_kind(graph, result, ALL_VALUE_TYPES, errors);
    let _ = verify_input_kind(graph, node, 0, &[DepValueKind::Control], errors);
    let _ = verify_input_kind(graph, node, 1, &[DepValueKind::Value(Type::Ptr)], errors);
}

fn verify_node_arity<const O: usize>(
    graph: &ValGraph,
    node: Node,
    input_count: u32,
    errors: &mut Vec<VerifierError>,
) -> Result<[DepValue; O], ()> {
    let inputs = graph.node_inputs(node);

    if inputs.len() != input_count as usize {
        errors.push(VerifierError::BadInputCount {
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
    errors: &mut Vec<VerifierError>,
) -> Result<[DepValue; O], ()> {
    let outputs = graph.node_outputs(node);
    if outputs.len() != O {
        errors.push(VerifierError::BadOutputCount {
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
    errors: &mut Vec<VerifierError>,
) -> Result<(), ()> {
    verify_input_kind(graph, node, input, ALL_INTEGER_TYPES, errors)
}

fn verify_integer_output_kind(
    graph: &ValGraph,
    value: DepValue,
    errors: &mut Vec<VerifierError>,
) -> Result<(), ()> {
    verify_output_kind(graph, value, ALL_INTEGER_TYPES, errors)
}

fn verify_input_kind(
    graph: &ValGraph,
    node: Node,
    input: u32,
    expected_kinds: &[DepValueKind],
    errors: &mut Vec<VerifierError>,
) -> Result<(), ()> {
    if !expected_kinds.contains(&graph.value_kind(graph.node_inputs(node)[input as usize])) {
        errors.push(VerifierError::BadInputKind {
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
    errors: &mut Vec<VerifierError>,
) -> Result<(), ()> {
    if !expected_kinds.contains(&graph.value_kind(value)) {
        errors.push(VerifierError::BadOutputKind {
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

#[cfg(test)]
mod tests;

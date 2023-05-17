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
        NodeKind::Region => {}
        NodeKind::Phi => {}
        NodeKind::IConst(val) => verify_iconst(graph, node, *val, errors),
        NodeKind::Iadd => verify_int_binop(graph, node, errors),
        NodeKind::Isub => verify_int_binop(graph, node, errors),
        NodeKind::And => verify_int_binop(graph, node, errors),
        NodeKind::Or => verify_int_binop(graph, node, errors),
        NodeKind::Xor => verify_int_binop(graph, node, errors),
        NodeKind::Shl => verify_shift_op(graph, node, errors),
        NodeKind::Lshr => verify_shift_op(graph, node, errors),
        NodeKind::Ashr => verify_shift_op(graph, node, errors),
        NodeKind::Smul => verify_int_binop(graph, node, errors),
        NodeKind::Umul => verify_int_binop(graph, node, errors),
        NodeKind::Sdiv => verify_int_binop(graph, node, errors),
        NodeKind::Udiv => verify_int_binop(graph, node, errors),
        NodeKind::Icmp(_) => verify_icmp(graph, node, errors),
        NodeKind::FConst(_) => verify_fconst(graph, node, errors),
        NodeKind::Load => {}
        NodeKind::Store => {}
        NodeKind::BrCond => {}
        NodeKind::Call(_) => {}
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

fn verify_node_arity<const O: usize>(
    graph: &ValGraph,
    node: Node,
    input_count: u32,
    errors: &mut Vec<VerifierError>,
) -> Result<[DepValue; O], ()> {
    let inputs = graph.node_inputs(node);
    let outputs = graph.node_outputs(node);

    if inputs.len() != input_count as usize {
        errors.push(VerifierError::BadInputCount {
            node,
            expected: input_count,
        });
        return Err(());
    }

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
    verify_input_kind(graph, node, input, INTEGER_TYPES, errors)
}

fn verify_integer_output_kind(
    graph: &ValGraph,
    value: DepValue,
    errors: &mut Vec<VerifierError>,
) -> Result<(), ()> {
    verify_output_kind(graph, value, INTEGER_TYPES, errors)
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

const INTEGER_TYPES: &[DepValueKind] = &[
    DepValueKind::Value(Type::I32),
    DepValueKind::Value(Type::I64),
];

#[cfg(test)]
mod tests;

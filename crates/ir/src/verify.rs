use core::array;

use alloc::{vec, vec::Vec};

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
        NodeKind::IConst(_) => {}
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
        NodeKind::Icmp(_) => {}
        NodeKind::FConst(_) => {}
        NodeKind::Load => {}
        NodeKind::Store => {}
        NodeKind::BrCond => {}
        NodeKind::Call(_) => {}
    }
}

fn verify_int_binop(graph: &ValGraph, node: Node, errors: &mut Vec<VerifierError>) {
    let Ok(([in_a, in_b], [result])) = verify_node_arity(graph, node, errors) else { return };

    if verify_integer_output_kind(graph, result, errors).is_err() {
        return;
    }

    let result_kind = graph.value_kind(result);
    if graph.value_kind(in_a) != result_kind {
        errors.push(VerifierError::BadInputKind {
            node,
            input: 0,
            expected: vec![result_kind],
        });
    }

    if graph.value_kind(in_b) != result_kind {
        errors.push(VerifierError::BadInputKind {
            node,
            input: 1,
            expected: vec![result_kind],
        });
    }
}

fn verify_shift_op(graph: &ValGraph, node: Node, errors: &mut Vec<VerifierError>) {
    let Ok(([shift_value, _shift_amount], [result])) = verify_node_arity(graph, node, errors) else {
        return;
    };

    if verify_integer_output_kind(graph, result, errors).is_err()
        || verify_integer_input_kind(graph, node, 1, errors).is_err()
    {
        return;
    }

    let result_kind = graph.value_kind(result);
    if graph.value_kind(shift_value) != result_kind {
        errors.push(VerifierError::BadInputKind {
            node,
            input: 0,
            expected: vec![result_kind],
        })
    }
}

fn verify_node_arity<const I: usize, const O: usize>(
    graph: &ValGraph,
    node: Node,
    errors: &mut Vec<VerifierError>,
) -> Result<([DepValue; I], [DepValue; O]), ()> {
    let inputs = graph.node_inputs(node);
    let outputs = graph.node_outputs(node);

    if inputs.len() != I {
        errors.push(VerifierError::BadInputCount {
            node,
            expected: I as u32,
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

    Ok((
        array::from_fn(|i| inputs[i]),
        array::from_fn(|i| outputs[i]),
    ))
}

fn verify_integer_input_kind(
    graph: &ValGraph,
    node: Node,
    input: u32,
    errors: &mut Vec<VerifierError>,
) -> Result<(), ()> {
    if !graph
        .value_kind(graph.node_inputs(node)[input as usize])
        .is_integer_value()
    {
        errors.push(VerifierError::BadInputKind {
            node,
            input,
            expected: all_integer_types(),
        });
        return Err(());
    }

    Ok(())
}

fn verify_integer_output_kind(
    graph: &ValGraph,
    value: DepValue,
    errors: &mut Vec<VerifierError>,
) -> Result<(), ()> {
    if !graph.value_kind(value).is_integer_value() {
        errors.push(VerifierError::BadOutputKind {
            value,
            expected: all_integer_types(),
        });
        return Err(());
    }
    Ok(())
}

fn all_integer_types() -> Vec<DepValueKind> {
    vec![
        DepValueKind::Value(Type::I32),
        DepValueKind::Value(Type::I64),
    ]
}

#[cfg(test)]
mod tests;

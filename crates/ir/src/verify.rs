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
        NodeKind::Shl => {}
        NodeKind::Lshr => {}
        NodeKind::Ashr => {}
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
    let inputs = graph.node_inputs(node);
    let outputs = graph.node_outputs(node);

    if inputs.len() != 2 {
        errors.push(VerifierError::BadInputCount { node, expected: 2 });
        return;
    }

    if outputs.len() != 1 {
        errors.push(VerifierError::BadOutputCount { node, expected: 1 });
        return;
    }

    let result = outputs[0];
    let in_a = inputs[0];
    let in_b = inputs[1];
    let result_kind = graph.value_kind(result);
    if !result_kind.is_integer_value() {
        errors.push(VerifierError::BadOutputKind {
            value: result,
            expected: vec![
                DepValueKind::Value(Type::I32),
                DepValueKind::Value(Type::I64),
            ],
        });
        return;
    }

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

#[cfg(test)]
mod tests;

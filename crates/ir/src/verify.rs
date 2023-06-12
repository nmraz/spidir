use alloc::vec::Vec;

use crate::{
    module::Signature,
    node::DepValueKind,
    valgraph::{DepValue, Node, ValGraph},
    valwalk::LiveNodeInfo,
};

use self::node::verify_node_kind;

mod node;

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

pub fn verify_graph(
    graph: &ValGraph,
    signature: &Signature,
    entry: Node,
) -> Result<(), Vec<VerifierError>> {
    let mut errors = Vec::new();

    for node in LiveNodeInfo::compute(graph, entry).iter_live_nodes() {
        verify_node_kind(graph, signature, node, &mut errors);
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

#[cfg(test)]
mod tests;

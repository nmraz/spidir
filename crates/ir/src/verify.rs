use alloc::vec::Vec;

use crate::valwalk::PostOrder;
use crate::{
    node::DepValueKind,
    valgraph::{DepValue, Node, ValGraph},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VerifierError {
    UnusedControl(DepValue),
    ReusedControl(DepValue),
}

pub fn verify_graph(graph: &ValGraph, entry: Node) -> Result<(), Vec<VerifierError>> {
    let mut errors = Vec::new();

    for node in PostOrder::with_entry(graph, entry) {
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
mod tests {
    use crate::node::{NodeKind, Type};

    use super::*;

    #[test]
    fn verify_add_graph() {
        let mut graph = ValGraph::new();
        let entry = graph.create_node(
            NodeKind::Entry,
            [],
            [
                DepValueKind::Control,
                DepValueKind::Value(Type::I32),
                DepValueKind::Value(Type::I32),
            ],
        );
        let entry_outputs = graph.node_outputs(entry);
        let control_value = entry_outputs[0];
        let param1 = entry_outputs[1];
        let param2 = entry_outputs[2];

        let add = graph.create_node(
            NodeKind::Iadd,
            [param1, param2],
            [DepValueKind::Value(Type::I32)],
        );
        let add_res = graph.node_outputs(add)[0];
        graph.create_node(NodeKind::Return, [control_value, add_res], []);

        assert_eq!(verify_graph(&graph, entry), Ok(()));
    }

    #[test]
    fn verify_unused_control() {
        let mut graph = ValGraph::new();
        let entry = graph.create_node(NodeKind::Entry, [], [DepValueKind::Control]);
        let entry_control = graph.node_outputs(entry)[0];

        assert_eq!(
            verify_graph(&graph, entry),
            Err(vec![VerifierError::UnusedControl(entry_control)])
        );
    }

    #[test]
    fn verify_unused_control_dead_region() {
        let mut graph = ValGraph::new();
        let entry = graph.create_node(NodeKind::Entry, [], [DepValueKind::Control]);
        let entry_control = graph.node_outputs(entry)[0];
        graph.create_node(NodeKind::Return, [entry_control], []);
        graph.create_node(NodeKind::Region, [], [DepValueKind::Control]);
        assert_eq!(verify_graph(&graph, entry), Ok(()));
    }

    #[test]
    fn verify_reused_control() {
        let mut graph = ValGraph::new();
        let entry = graph.create_node(NodeKind::Entry, [], [DepValueKind::Control]);
        let entry_control = graph.node_outputs(entry)[0];
        graph.create_node(NodeKind::Region, [entry_control], []);
        graph.create_node(NodeKind::Region, [entry_control], []);

        assert_eq!(
            verify_graph(&graph, entry),
            Err(vec![VerifierError::ReusedControl(entry_control)])
        );
    }
}

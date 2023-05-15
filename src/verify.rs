use alloc::vec::Vec;

use crate::valgraph::{DepValue, DepValueKind, Node, ValGraph};
use crate::valwalk::PostOrder;

pub enum VerifierError {
    UnusedControl(DepValue),
    ReusedControl(DepValue),
}

pub fn verify(graph: &ValGraph, entry: Node) -> Result<(), Vec<VerifierError>> {
    let mut errors = Vec::new();

    for node in PostOrder::with_entry(graph, entry) {
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

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

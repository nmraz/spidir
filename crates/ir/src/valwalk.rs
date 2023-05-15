use alloc::{vec, vec::Vec};

use cranelift_entity::EntitySet;

use crate::{
    node::DepValueKind,
    valgraph::{Node, ValGraph},
};

pub fn compute_live_roots(graph: &ValGraph, entry: Node) -> Vec<Node> {
    let mut stack = vec![entry];
    let mut visited = EntitySet::new();
    let mut live_roots = Vec::new();

    while let Some(node) = stack.pop() {
        if visited.contains(node) {
            continue;
        }

        visited.insert(node);
        stack.extend(live_succs(graph, node));

        // This node is reachable in the liveness graph but has no inputs, so make sure it is treated
        // as a root.
        if graph.node_inputs(node).is_empty() {
            live_roots.push(node);
        }
    }

    live_roots
}

pub struct PostOrder<'a> {
    graph: &'a ValGraph,
    stack: Vec<(WalkPhase, Node)>,
    visited: EntitySet<Node>,
}

impl<'a> PostOrder<'a> {
    pub fn with_entry(graph: &'a ValGraph, entry: Node) -> Self {
        Self::with_roots(graph, compute_live_roots(graph, entry))
    }

    pub fn with_roots(graph: &'a ValGraph, roots: impl IntoIterator<Item = Node>) -> Self {
        let stack = roots
            .into_iter()
            .map(|node| (WalkPhase::Pre, node))
            .collect();

        Self {
            graph,
            stack,
            visited: EntitySet::new(),
        }
    }
}

impl<'a> Iterator for PostOrder<'a> {
    type Item = Node;

    fn next(&mut self) -> Option<Node> {
        loop {
            let (phase, node) = self.stack.pop()?;
            match phase {
                WalkPhase::Pre if !self.visited.contains(node) => {
                    self.visited.insert(node);
                    self.stack.push((WalkPhase::Post, node));
                    for output in self.graph.node_outputs(node) {
                        for (user, _) in self.graph.value_uses(output) {
                            self.stack.push((WalkPhase::Pre, user))
                        }
                    }
                }
                WalkPhase::Pre => {}
                WalkPhase::Post => return Some(node),
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WalkPhase {
    Pre,
    Post,
}

fn live_succs(graph: &ValGraph, node: Node) -> impl Iterator<Item = Node> + '_ {
    // Consider all inputs as "live" so we don't cause cases where uses are traversed without their
    // corresponding defs. Users that want to treat regions with no control inputs as dead should do
    // so themselves.
    let input_succs = graph
        .node_inputs(node)
        .into_iter()
        .map(|input| graph.value_def(input).0);

    let output_succs = graph.node_outputs(node).into_iter().filter_map(|output| {
        // For outputs, a control output indicates that the node receiving control is live.
        if graph.value_kind(output) == DepValueKind::Control {
            // Note: we only take the first output because a control value should be used at most once.
            graph.value_uses(output).next().map(|(node, _)| node)
        } else {
            None
        }
    });

    input_succs.chain(output_succs)
}

#[cfg(test)]
mod tests {
    use crate::node::{NodeKind, Type};

    use super::*;

    fn check_live_roots(graph: &ValGraph, entry: Node, expected: &[Node]) {
        assert_eq!(compute_live_roots(graph, entry), expected);
    }

    fn check_postorder(graph: &ValGraph, entry: Node, expected: &[Node]) {
        let postorder: Vec<_> = PostOrder::with_entry(graph, entry).collect();
        assert_eq!(postorder, expected);
    }

    #[test]
    fn live_roots_add_params() {
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

        check_live_roots(&graph, entry, &[entry]);
    }

    #[test]
    fn live_roots_add_const() {
        let mut graph = ValGraph::new();
        let entry = graph.create_node(
            NodeKind::Entry,
            [],
            [DepValueKind::Control, DepValueKind::Value(Type::I32)],
        );
        let entry_outputs = graph.node_outputs(entry);
        let control_value = entry_outputs[0];
        let param1 = entry_outputs[1];

        let five = graph.create_node(NodeKind::IConst(5), [], [DepValueKind::Value(Type::I32)]);
        let five_val = graph.node_outputs(five)[0];
        let add = graph.create_node(
            NodeKind::Iadd,
            [param1, five_val],
            [DepValueKind::Value(Type::I32)],
        );
        let add_res = graph.node_outputs(add)[0];
        graph.create_node(NodeKind::Return, [control_value, add_res], []);

        check_live_roots(&graph, entry, &[entry, five]);
    }

    #[test]
    fn live_roots_add_consts() {
        let mut graph = ValGraph::new();
        let entry = graph.create_node(NodeKind::Entry, [], [DepValueKind::Control]);
        let entry_outputs = graph.node_outputs(entry);
        let control_value = entry_outputs[0];

        let five = graph.create_node(NodeKind::IConst(5), [], [DepValueKind::Value(Type::I32)]);
        let five_val = graph.node_outputs(five)[0];
        let three = graph.create_node(NodeKind::IConst(3), [], [DepValueKind::Value(Type::I32)]);
        let three_val = graph.node_outputs(three)[0];
        let add = graph.create_node(
            NodeKind::Iadd,
            [five_val, three_val],
            [DepValueKind::Value(Type::I32)],
        );
        let add_res = graph.node_outputs(add)[0];
        graph.create_node(NodeKind::Return, [control_value, add_res], []);

        check_live_roots(&graph, entry, &[entry, three, five]);
    }

    #[test]
    fn live_roots_dead_value() {
        let mut graph = ValGraph::new();
        let entry = graph.create_node(
            NodeKind::Entry,
            [],
            [DepValueKind::Control, DepValueKind::Value(Type::I32)],
        );
        let entry_outputs = graph.node_outputs(entry);
        let control_value = entry_outputs[0];
        let param1 = entry_outputs[1];

        graph.create_node(NodeKind::IConst(5), [], [DepValueKind::Value(Type::I32)]);
        graph.create_node(NodeKind::Return, [control_value, param1], []);

        check_live_roots(&graph, entry, &[entry]);
    }

    #[test]
    fn live_roots_dead_region() {
        let mut graph = ValGraph::new();
        let entry = graph.create_node(
            NodeKind::Entry,
            [],
            [DepValueKind::Control, DepValueKind::Value(Type::I32)],
        );
        let entry_outputs = graph.node_outputs(entry);
        let entry_control = entry_outputs[0];
        let param1 = entry_outputs[1];

        let dead_region = graph.create_node(
            NodeKind::Region,
            [],
            [DepValueKind::Control, DepValueKind::PhiSelector],
        );
        let dead_region_control = graph.node_outputs(dead_region)[0];

        let exit_region = graph.create_node(
            NodeKind::Region,
            [entry_control, dead_region_control],
            [DepValueKind::Control, DepValueKind::PhiSelector],
        );
        let exit_region_control = graph.node_outputs(exit_region)[0];
        graph.create_node(NodeKind::Return, [exit_region_control, param1], []);

        check_live_roots(&graph, entry, &[entry, dead_region]);
    }

    #[test]
    fn live_roots_detached_region() {
        let mut graph = ValGraph::new();
        let entry = graph.create_node(
            NodeKind::Entry,
            [],
            [DepValueKind::Control, DepValueKind::Value(Type::I32)],
        );
        let entry_outputs = graph.node_outputs(entry);
        let entry_control = entry_outputs[0];
        let param1 = entry_outputs[1];

        // Create a pair of completely detached regions.
        let detached_region = graph.create_node(
            NodeKind::Region,
            [],
            [DepValueKind::Control, DepValueKind::PhiSelector],
        );
        let detached_region_control = graph.node_outputs(detached_region)[0];
        graph.create_node(
            NodeKind::Region,
            [detached_region_control],
            [DepValueKind::Control, DepValueKind::PhiSelector],
        );

        let exit_region = graph.create_node(
            NodeKind::Region,
            [entry_control],
            [DepValueKind::Control, DepValueKind::PhiSelector],
        );
        let exit_region_control = graph.node_outputs(exit_region)[0];
        graph.create_node(NodeKind::Return, [exit_region_control, param1], []);

        check_live_roots(&graph, entry, &[entry]);
    }

    #[test]
    fn postorder_add_params() {
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
        let ret = graph.create_node(NodeKind::Return, [control_value, add_res], []);

        check_postorder(&graph, entry, &[ret, add, entry]);
    }

    #[test]
    fn postorder_add_const() {
        let mut graph = ValGraph::new();
        let entry = graph.create_node(
            NodeKind::Entry,
            [],
            [DepValueKind::Control, DepValueKind::Value(Type::I32)],
        );
        let entry_outputs = graph.node_outputs(entry);
        let control_value = entry_outputs[0];
        let param1 = entry_outputs[1];

        let five = graph.create_node(NodeKind::IConst(5), [], [DepValueKind::Value(Type::I32)]);
        let five_val = graph.node_outputs(five)[0];
        let add = graph.create_node(
            NodeKind::Iadd,
            [param1, five_val],
            [DepValueKind::Value(Type::I32)],
        );
        let add_res = graph.node_outputs(add)[0];
        let ret = graph.create_node(NodeKind::Return, [control_value, add_res], []);

        check_postorder(&graph, entry, &[ret, add, five, entry]);
    }
}

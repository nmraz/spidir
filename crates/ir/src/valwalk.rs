use alloc::{vec, vec::Vec};

use cranelift_entity::EntitySet;

use crate::{
    node::DepValueKind,
    valgraph::{Node, ValGraph},
};

#[derive(Debug, Clone)]
pub struct LiveNodeInfo {
    roots: Vec<Node>,
    live_nodes: EntitySet<Node>,
}

impl LiveNodeInfo {
    pub fn compute(graph: &ValGraph, entry: Node) -> Self {
        let mut stack = vec![entry];
        let mut visited = EntitySet::new();
        let mut roots = Vec::new();

        while let Some(node) = stack.pop() {
            if visited.contains(node) {
                continue;
            }

            visited.insert(node);
            stack.extend(live_succs(graph, node));

            // This node is reachable in the liveness graph but has no inputs, so make sure it is treated
            // as a root.
            if graph.node_inputs(node).is_empty() {
                roots.push(node);
            }
        }

        Self {
            roots,
            live_nodes: visited,
        }
    }

    pub fn roots(&self) -> &[Node] {
        &self.roots
    }

    pub fn live_nodes(&self) -> &EntitySet<Node> {
        &self.live_nodes
    }

    pub fn iter_live_nodes(&self) -> impl Iterator<Item = Node> + '_ {
        // Somewhat unbelievably, there is no easy way to just iterate over an `EntitySet`.
        self.live_nodes
            .keys()
            .filter(|&node| self.live_nodes.contains(node))
    }

    pub fn postorder<'a>(&'a self, graph: &'a ValGraph) -> PostOrder<'a> {
        PostOrder::new(graph, self.roots.iter().copied(), &self.live_nodes)
    }
}

pub struct PostOrder<'a> {
    graph: &'a ValGraph,
    live_nodes: &'a EntitySet<Node>,
    stack: Vec<(WalkPhase, Node)>,
    visited: EntitySet<Node>,
}

impl<'a> PostOrder<'a> {
    pub fn new(
        graph: &'a ValGraph,
        roots: impl IntoIterator<Item = Node>,
        live_nodes: &'a EntitySet<Node>,
    ) -> Self {
        let stack = roots
            .into_iter()
            .map(|node| (WalkPhase::Pre, node))
            .collect();

        Self {
            graph,
            live_nodes,
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
                WalkPhase::Pre
                    if !self.visited.contains(node) && self.live_nodes.contains(node) =>
                {
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
    use fx_utils::FxHashSet;

    use crate::{
        node::{NodeKind, Type},
        test_utils::create_entry,
    };

    use super::*;

    #[track_caller]
    fn check_live_info(
        graph: &ValGraph,
        entry: Node,
        expected_roots: &[Node],
        expected_live_nodes: &[Node],
    ) {
        let live_info = LiveNodeInfo::compute(graph, entry);
        assert_eq!(live_info.roots(), expected_roots);

        let expected_live_nodes: FxHashSet<_> = expected_live_nodes.iter().copied().collect();
        let actual_live_nodes: FxHashSet<_> = live_info.iter_live_nodes().collect();

        assert_eq!(actual_live_nodes, expected_live_nodes);
    }

    #[track_caller]
    fn check_postorder(graph: &ValGraph, entry: Node, expected: &[Node]) {
        let postorder: Vec<_> = LiveNodeInfo::compute(graph, entry)
            .postorder(graph)
            .collect();
        assert_eq!(postorder, expected);
    }

    #[test]
    fn live_info_add_params() {
        let mut graph = ValGraph::new();

        let (entry, control_value, [param1, param2]) =
            create_entry(&mut graph, [Type::I32, Type::I32]);

        let add = graph.create_node(
            NodeKind::Iadd,
            [param1, param2],
            [DepValueKind::Value(Type::I32)],
        );
        let add_res = graph.node_outputs(add)[0];
        let ret = crate::test_utils::create_return(&mut graph, [control_value, add_res]);

        check_live_info(&graph, entry, &[entry], &[entry, add, ret]);
    }

    #[test]
    fn live_info_add_const() {
        let mut graph = ValGraph::new();
        let (entry, control_value, [param1]) = create_entry(&mut graph, [Type::I32]);

        let five = graph.create_node(NodeKind::IConst(5), [], [DepValueKind::Value(Type::I32)]);
        let five_val = graph.node_outputs(five)[0];
        let add = graph.create_node(
            NodeKind::Iadd,
            [param1, five_val],
            [DepValueKind::Value(Type::I32)],
        );
        let add_res = graph.node_outputs(add)[0];
        let ret = crate::test_utils::create_return(&mut graph, [control_value, add_res]);

        check_live_info(&graph, entry, &[entry, five], &[entry, five, add, ret]);
    }

    #[test]
    fn live_info_add_consts() {
        let mut graph = ValGraph::new();
        let (entry, control_value, []) = create_entry(&mut graph, []);
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
        let ret = crate::test_utils::create_return(&mut graph, [control_value, add_res]);

        check_live_info(
            &graph,
            entry,
            &[entry, three, five],
            &[entry, three, five, add, ret],
        );
    }

    #[test]
    fn live_info_dead_value() {
        let mut graph = ValGraph::new();
        let (entry, control_value, [param1]) = create_entry(&mut graph, [Type::I32]);
        graph.create_node(NodeKind::IConst(5), [], [DepValueKind::Value(Type::I32)]);
        let ret = crate::test_utils::create_return(&mut graph, [control_value, param1]);

        check_live_info(&graph, entry, &[entry], &[entry, ret]);
    }

    #[test]
    fn live_info_dead_region() {
        let mut graph = ValGraph::new();

        let (entry, entry_control, [param1]) = create_entry(&mut graph, [Type::I32]);

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
        let ret = crate::test_utils::create_return(&mut graph, [exit_region_control, param1]);

        check_live_info(
            &graph,
            entry,
            &[entry, dead_region],
            &[entry, dead_region, exit_region, ret],
        );
    }

    #[test]
    fn live_info_detached_region() {
        let mut graph = ValGraph::new();
        let (entry, entry_control, [param1]) = create_entry(&mut graph, [Type::I32]);

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
        let ret = crate::test_utils::create_return(&mut graph, [exit_region_control, param1]);

        check_live_info(&graph, entry, &[entry], &[entry, exit_region, ret]);
    }

    #[test]
    fn live_info_reused_const() {
        let mut graph = ValGraph::new();

        let (entry, control_value, []) = create_entry(&mut graph, []);
        let five = graph.create_node(NodeKind::IConst(5), [], [DepValueKind::Value(Type::I32)]);
        let five_val = graph.node_outputs(five)[0];

        let add1 = graph.create_node(
            NodeKind::Iadd,
            [five_val, five_val],
            [DepValueKind::Value(Type::I32)],
        );
        let add_res = graph.node_outputs(add1)[0];

        // Create a dead add using the same constant input as the live one.
        graph.create_node(
            NodeKind::Iadd,
            [five_val, five_val],
            [DepValueKind::Value(Type::I32)],
        );

        let ret = crate::test_utils::create_return(&mut graph, [control_value, add_res]);

        check_live_info(&graph, entry, &[entry, five], &[entry, five, add1, ret]);
    }

    #[test]
    fn postorder_add_params() {
        let mut graph = ValGraph::new();

        let (entry, control_value, [param1, param2]) =
            create_entry(&mut graph, [Type::I32, Type::I32]);

        let add = graph.create_node(
            NodeKind::Iadd,
            [param1, param2],
            [DepValueKind::Value(Type::I32)],
        );
        let add_res = graph.node_outputs(add)[0];
        let ret = crate::test_utils::create_return(&mut graph, [control_value, add_res]);

        check_postorder(&graph, entry, &[ret, add, entry]);
    }

    #[test]
    fn postorder_add_const() {
        let mut graph = ValGraph::new();
        let (entry, control_value, [param1]) = create_entry(&mut graph, [Type::I32]);

        let five = graph.create_node(NodeKind::IConst(5), [], [DepValueKind::Value(Type::I32)]);
        let five_val = graph.node_outputs(five)[0];
        let add = graph.create_node(
            NodeKind::Iadd,
            [param1, five_val],
            [DepValueKind::Value(Type::I32)],
        );
        let add_res = graph.node_outputs(add)[0];
        let ret = crate::test_utils::create_return(&mut graph, [control_value, add_res]);

        check_postorder(&graph, entry, &[ret, add, five, entry]);
    }
}

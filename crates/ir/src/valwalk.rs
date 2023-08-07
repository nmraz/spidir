use core::iter;

use alloc::{vec, vec::Vec};

use cranelift_entity::EntitySet;

use crate::{
    node::DepValueKind,
    valgraph::{Node, ValGraph},
};

pub trait Succs {
    fn successors(&self, node: Node, f: impl FnMut(Node));
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WalkPhase {
    Pre,
    Post,
}

pub struct PreOrder<S> {
    pub visited: EntitySet<Node>,
    succs: S,
    stack: Vec<Node>,
}

impl<S> PreOrder<S> {
    pub fn new(succs: S, roots: impl IntoIterator<Item = Node>) -> Self {
        let stack = roots.into_iter().collect();
        Self {
            visited: EntitySet::new(),
            succs,
            stack,
        }
    }
}

impl<S: Succs> Iterator for PreOrder<S> {
    type Item = Node;

    fn next(&mut self) -> Option<Node> {
        let node = loop {
            let node = self.stack.pop()?;
            if !self.visited.contains(node) {
                break node;
            }
        };

        self.visited.insert(node);

        self.succs.successors(node, |succ| {
            // This extra check here is an optimization to avoid needlessly placing
            // an obviously-visited node on to the stack. Even if the node is not
            // visited now, it may be by the time it is popped off the stack later.
            if !self.visited.contains(succ) {
                self.stack.push(succ);
            }
        });

        Some(node)
    }
}

pub struct PostOrder<S> {
    pub visited: EntitySet<Node>,
    succs: S,
    stack: Vec<(WalkPhase, Node)>,
}

impl<S> PostOrder<S> {
    pub fn new(succs: S, roots: impl IntoIterator<Item = Node>) -> Self {
        // Note: push the roots onto the stack in source order so that this order is preserved in
        // any RPO. Some clients depend on this: for example, the live-node RPO of a function graph
        // should always start with its entry node.
        let stack = roots
            .into_iter()
            .map(|node| (WalkPhase::Pre, node))
            .collect();

        Self {
            visited: EntitySet::new(),
            succs,
            stack,
        }
    }
}

impl<S: Succs> Iterator for PostOrder<S> {
    type Item = Node;

    fn next(&mut self) -> Option<Node> {
        loop {
            let (phase, node) = self.stack.pop()?;
            match phase {
                WalkPhase::Pre => {
                    if !self.visited.contains(node) {
                        self.visited.insert(node);
                        self.stack.push((WalkPhase::Post, node));
                        self.succs.successors(node, |succ| {
                            // This extra check here is an optimization to avoid needlessly placing
                            // an obviously-visited node on to the stack. Even if the node is not
                            // visited now, it may be by the time it is popped off the stack later.
                            if !self.visited.contains(succ) {
                                self.stack.push((WalkPhase::Pre, succ));
                            }
                        });
                    }
                }
                WalkPhase::Post => return Some(node),
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct LiveNodeSuccs<'a>(&'a ValGraph);

impl<'a> LiveNodeSuccs<'a> {
    pub fn new(graph: &'a ValGraph) -> Self {
        Self(graph)
    }
}

impl<'a> Succs for LiveNodeSuccs<'a> {
    fn successors(&self, node: Node, mut f: impl FnMut(Node)) {
        // Consider all inputs as "live" so we don't cause cases where uses are traversed without their
        // corresponding defs. Users that want to treat regions with no control inputs as dead should do
        // so themselves.
        for input in self.0.node_inputs(node) {
            f(self.0.value_def(input).0);
        }

        for output in self.0.node_outputs(node) {
            // For outputs, a control output indicates that the node receiving control is live.
            if self.0.value_kind(output) == DepValueKind::Control {
                // Note: we explicitly cope with malformed unused/reused control values here as this
                // traversal is used in the verifier itself and the graph visualizer.
                for (succ, _) in self.0.value_uses(output) {
                    f(succ);
                }
            }
        }
    }
}

pub type LiveNodeWalk<'a> = PreOrder<LiveNodeSuccs<'a>>;

/// Walks the nodes currently live in `graph` given `entry` in an unspecified order.
///
/// `entry` is guaranteed to be the first node returned.
pub fn walk_live_nodes(graph: &ValGraph, entry: Node) -> LiveNodeWalk<'_> {
    PreOrder::new(LiveNodeSuccs::new(graph), iter::once(entry))
}

#[derive(Clone, Copy)]
pub struct DefUseSuccs<'a> {
    graph: &'a ValGraph,
    live_nodes: &'a EntitySet<Node>,
}

impl<'a> DefUseSuccs<'a> {
    pub fn new(graph: &'a ValGraph, live_nodes: &'a EntitySet<Node>) -> Self {
        Self { graph, live_nodes }
    }
}

impl<'a> Succs for DefUseSuccs<'a> {
    fn successors(&self, node: Node, mut f: impl FnMut(Node)) {
        for output in self.graph.node_outputs(node) {
            for (user, _) in self.graph.value_uses(output) {
                if self.live_nodes.contains(user) {
                    f(user);
                }
            }
        }
    }
}

pub type DefUsePostorder<'a> = PostOrder<DefUseSuccs<'a>>;

#[derive(Debug, Clone)]
pub struct LiveNodeInfo {
    roots: Vec<Node>,
    live_nodes: EntitySet<Node>,
}

impl LiveNodeInfo {
    pub fn compute(graph: &ValGraph, entry: Node) -> Self {
        let mut walk = walk_live_nodes(graph, entry);
        let mut roots = vec![];
        for node in walk.by_ref() {
            if graph.node_inputs(node).is_empty() {
                roots.push(node);
            }
        }

        Self {
            roots,
            live_nodes: walk.visited,
        }
    }

    pub fn roots(&self) -> &[Node] {
        &self.roots
    }

    pub fn live_nodes(&self) -> &EntitySet<Node> {
        &self.live_nodes
    }

    pub fn postorder<'a>(&'a self, graph: &'a ValGraph) -> DefUsePostorder<'a> {
        PostOrder::new(
            DefUseSuccs::new(graph, &self.live_nodes),
            self.roots.iter().copied(),
        )
    }

    pub fn reverse_postorder(&self, graph: &ValGraph) -> Vec<Node> {
        let mut rpo: Vec<_> = self.postorder(graph).collect();
        rpo.reverse();
        rpo
    }
}

#[cfg(test)]
mod tests {
    use fx_utils::FxHashSet;

    use crate::{
        node::{NodeKind, Type},
        test_utils::{create_entry, create_return},
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

        let live_node_set = live_info.live_nodes();
        let expected_live_nodes: FxHashSet<_> = expected_live_nodes.iter().copied().collect();
        let actual_live_nodes: FxHashSet<_> = live_node_set
            .keys()
            .filter(|&node| live_node_set.contains(node))
            .collect();

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
        let ret = create_return(&mut graph, [control_value, add_res]);

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
        let ret = create_return(&mut graph, [control_value, add_res]);

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
        let ret = create_return(&mut graph, [control_value, add_res]);

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
        let ret = create_return(&mut graph, [control_value, param1]);

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
        let ret = create_return(&mut graph, [exit_region_control, param1]);

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
        let ret = create_return(&mut graph, [exit_region_control, param1]);

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

        let ret = create_return(&mut graph, [control_value, add_res]);

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
        let ret = create_return(&mut graph, [control_value, add_res]);

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
        let ret = create_return(&mut graph, [control_value, add_res]);

        check_postorder(&graph, entry, &[ret, add, five, entry]);
    }
}

use core::iter;

use alloc::{vec, vec::Vec};

use cranelift_entity::EntitySet;
use dominators::IntoCfg;

use crate::valgraph::{DepValue, Node, ValGraph};

pub type PreOrder<G> = graphwalk::PreOrder<G, EntitySet<Node>>;
pub type PostOrder<G> = graphwalk::PostOrder<G, EntitySet<Node>>;

pub fn live_node_succs(graph: &ValGraph, node: Node) -> impl Iterator<Item = Node> + '_ {
    // Consider all inputs as "live" so we don't cause cases where uses are traversed without their
    // corresponding defs. Users that want to treat regions with no control inputs as dead should do
    // so themselves.
    graph
        .node_inputs(node)
        .into_iter()
        .map(move |input| graph.value_def(input).0)
        .chain(
            // For outputs, a control output indicates that the node receiving control is live.
            graph
                .node_outputs(node)
                .into_iter()
                .filter(move |&output| graph.value_kind(output).is_control())
                .flat_map(move |output| {
                    // Note: we explicitly cope with malformed unused/reused control values here as this
                    // traversal is used in the verifier itself and the graph visualizer.
                    graph.value_uses(output)
                })
                .map(|(node, _input_idx)| node),
        )
}

#[derive(Clone, Copy)]
pub struct LiveNodeSuccs<'a>(&'a ValGraph);

impl<'a> LiveNodeSuccs<'a> {
    #[inline]
    pub fn new(graph: &'a ValGraph) -> Self {
        Self(graph)
    }
}

impl<'a> graphwalk::GraphRef for LiveNodeSuccs<'a> {
    type Node = Node;

    fn successors(&self, node: Node, f: impl FnMut(Node)) {
        live_node_succs(self.0, node).for_each(f);
    }
}

pub type LiveNodeWalk<'a> = PreOrder<LiveNodeSuccs<'a>>;

/// Walks the nodes currently live in `graph` given `entry` in an unspecified order.
///
/// `entry` is guaranteed to be the last node returned if it has no inputs (as should be the case
/// with every well-formed graph).
pub fn walk_live_nodes(graph: &ValGraph, entry: Node) -> LiveNodeWalk<'_> {
    PreOrder::new(LiveNodeSuccs::new(graph), iter::once(entry))
}

pub fn raw_def_use_succs(graph: &ValGraph, node: Node) -> impl Iterator<Item = (Node, u32)> + '_ {
    graph
        .node_outputs(node)
        .into_iter()
        .flat_map(move |output| graph.value_uses(output))
}

pub fn def_use_succs<'a>(
    graph: &'a ValGraph,
    live_nodes: &'a EntitySet<Node>,
    node: Node,
) -> impl Iterator<Item = (Node, u32)> + 'a {
    raw_def_use_succs(graph, node).filter(move |&(succ, _use_idx)| live_nodes.contains(succ))
}

#[derive(Clone, Copy)]
pub struct DefUseSuccs<'a> {
    graph: &'a ValGraph,
    live_nodes: &'a EntitySet<Node>,
}

impl<'a> DefUseSuccs<'a> {
    #[inline]
    pub fn new(graph: &'a ValGraph, live_nodes: &'a EntitySet<Node>) -> Self {
        Self { graph, live_nodes }
    }
}

impl<'a> graphwalk::GraphRef for DefUseSuccs<'a> {
    type Node = Node;

    fn successors(&self, node: Node, mut f: impl FnMut(Node)) {
        def_use_succs(self.graph, self.live_nodes, node).for_each(|(succ, _input_idx)| f(succ));
    }
}

pub type DefUsePostorder<'a> = PostOrder<DefUseSuccs<'a>>;

pub fn def_use_preds(graph: &ValGraph, node: Node) -> impl Iterator<Item = Node> + '_ {
    graph
        .node_inputs(node)
        .into_iter()
        .map(move |input| graph.value_def(input).0)
}

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

    #[inline]
    pub fn roots(&self) -> &[Node] {
        &self.roots
    }

    #[inline]
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

pub fn dataflow_inputs(graph: &ValGraph, node: Node) -> impl Iterator<Item = DepValue> + '_ {
    graph
        .node_inputs(node)
        .into_iter()
        .filter(move |&input| graph.value_kind(input).is_value())
}

pub fn dataflow_preds(graph: &ValGraph, node: Node) -> impl Iterator<Item = Node> + '_ {
    dataflow_inputs(graph, node).map(|input| graph.value_def(input).0)
}

pub fn dataflow_outputs(graph: &ValGraph, node: Node) -> impl Iterator<Item = DepValue> + '_ {
    graph
        .node_outputs(node)
        .into_iter()
        .filter(move |&output| graph.value_kind(output).is_value())
}

pub fn raw_dataflow_succs(graph: &ValGraph, node: Node) -> impl Iterator<Item = (Node, u32)> + '_ {
    dataflow_outputs(graph, node).flat_map(move |output| graph.value_uses(output))
}

pub fn dataflow_succs<'a>(
    graph: &'a ValGraph,
    live_nodes: &'a EntitySet<Node>,
    node: Node,
) -> impl Iterator<Item = (Node, u32)> + 'a {
    raw_dataflow_succs(graph, node).filter(move |&(succ, _input_idx)| live_nodes.contains(succ))
}

pub fn cfg_outputs(graph: &ValGraph, node: Node) -> impl Iterator<Item = DepValue> + '_ {
    graph
        .node_outputs(node)
        .into_iter()
        .filter(|&output| graph.value_kind(output).is_control())
}

pub fn cfg_succs(graph: &ValGraph, node: Node) -> impl Iterator<Item = Node> + '_ {
    cfg_outputs(graph, node)
        .flat_map(|output| graph.value_uses(output))
        .map(|(succ_node, _succ_input_idx)| succ_node)
}

#[derive(Clone, Copy)]
pub struct ForwardCfg<'a>(&'a ValGraph);

impl<'a> ForwardCfg<'a> {
    #[inline]
    pub fn new(graph: &'a ValGraph) -> Self {
        Self(graph)
    }
}

impl graphwalk::GraphRef for ForwardCfg<'_> {
    type Node = Node;

    fn successors(&self, node: Self::Node, f: impl FnMut(Node)) {
        cfg_succs(self.0, node).for_each(f);
    }
}

impl graphwalk::PredGraphRef for ForwardCfg<'_> {
    fn predecessors(&self, node: Self::Node, f: impl FnMut(Self::Node)) {
        cfg_preds(self.0, node).for_each(f);
    }
}

impl<'a> IntoCfg for &'a ValGraph {
    type Node = Node;
    type Cfg = ForwardCfg<'a>;

    fn into_cfg(self) -> Self::Cfg {
        ForwardCfg::new(self)
    }
}

pub type CfgPreorder<'a> = PreOrder<ForwardCfg<'a>>;

pub fn cfg_preorder(graph: &ValGraph, entry: Node) -> CfgPreorder<'_> {
    CfgPreorder::new(ForwardCfg::new(graph), [entry])
}

pub fn cfg_inputs(graph: &ValGraph, node: Node) -> impl Iterator<Item = DepValue> + '_ {
    graph
        .node_inputs(node)
        .into_iter()
        .filter(|&input| graph.value_kind(input).is_control())
}

pub fn cfg_preds(graph: &ValGraph, node: Node) -> impl Iterator<Item = Node> + '_ {
    cfg_inputs(graph, node).map(|input| graph.value_def(input).0)
}

pub fn get_attached_phis(graph: &ValGraph, node: Node) -> impl Iterator<Item = Node> + '_ {
    graph
        .node_outputs(node)
        .into_iter()
        .filter(|&output| graph.value_kind(output).is_phisel())
        .flat_map(|phisel| graph.value_uses(phisel))
        .map(|(phi, _)| phi)
}

#[cfg(test)]
mod tests {
    use fx_utils::FxHashSet;

    use crate::{
        node::{DepValueKind, NodeKind, Type},
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

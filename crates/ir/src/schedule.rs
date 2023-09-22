use core::iter;

use alloc::vec::Vec;

use cranelift_entity::{packed_option::PackedOption, EntitySet, SecondaryMap};
use graphwalk::PostOrderContext;

use crate::{
    domtree::{DomTree, DomTreeNode},
    valgraph::{Node, ValGraph},
    valwalk::{cfg_preorder, dataflow_preds, dataflow_succs, get_attached_phis, LiveNodeInfo},
};

pub type ByNodeSchedule = SecondaryMap<Node, PackedOption<DomTreeNode>>;

pub struct ScheduleCtx<'a> {
    graph: &'a ValGraph,
    domtree: &'a DomTree,
    cfg_preorder: Vec<Node>,
    live_node_info: LiveNodeInfo,
    pinned_nodes: ByNodeSchedule,
}

impl<'a> ScheduleCtx<'a> {
    pub fn prepare(graph: &'a ValGraph, domtree: &'a DomTree) -> Self {
        let entry = domtree.get_cfg_node(domtree.root());
        let cfg_preorder: Vec<_> = cfg_preorder(graph, entry).collect();
        let live_node_info = LiveNodeInfo::compute(graph, domtree.get_cfg_node(domtree.root()));
        let mut pinned_nodes = ByNodeSchedule::new();

        for &node in &cfg_preorder {
            let domtree_node = domtree
                .get_tree_node(node)
                .expect("live CFG node not in dominator tree");
            pinned_nodes[node] = domtree_node.into();
            for phi in get_attached_phis(graph, node) {
                pinned_nodes[phi] = domtree_node.into();
            }
        }

        Self {
            graph,
            domtree,
            cfg_preorder,
            live_node_info,
            pinned_nodes,
        }
    }

    #[inline]
    pub fn graph(&self) -> &ValGraph {
        self.graph
    }

    #[inline]
    pub fn domtree(&self) -> &DomTree {
        self.domtree
    }

    #[inline]
    pub fn cfg_preorder(&self) -> &[Node] {
        &self.cfg_preorder
    }

    #[inline]
    pub fn live_node_info(&self) -> &LiveNodeInfo {
        &self.live_node_info
    }

    #[inline]
    pub fn live_nodes(&self) -> &EntitySet<Node> {
        self.live_node_info.live_nodes()
    }

    #[inline]
    pub fn is_live(&self, node: Node) -> bool {
        self.live_nodes().contains(node)
    }

    #[inline]
    pub fn pinned_nodes(&self) -> &ByNodeSchedule {
        &self.pinned_nodes
    }

    pub fn walk_pinned_nodes(&self) -> impl Iterator<Item = Node> + '_ {
        self.cfg_preorder
            .iter()
            .flat_map(move |&cfg_node| iter::once(cfg_node).chain(self.get_attached_phis(cfg_node)))
    }

    pub fn get_attached_phis(&self, cfg_node: Node) -> impl Iterator<Item = Node> + '_ {
        get_attached_phis(self.graph, cfg_node).filter(move |&phi| self.is_live(phi))
    }
}

pub fn schedule_early(
    ctx: &ScheduleCtx<'_>,
    mut schedule_node: impl FnMut(&ByNodeSchedule, Node) -> DomTreeNode,
) -> ByNodeSchedule {
    let mut schedule = ctx.pinned_nodes.clone();
    let mut visited = EntitySet::new();
    let mut data_pred_postorder = PostOrderContext::new(UnpinnedDataPreds::new(ctx.graph()), []);

    for pinned in ctx.walk_pinned_nodes() {
        data_pred_postorder.reset(unpinned_dataflow_preds(ctx.graph(), pinned));
        while let Some(pred) = data_pred_postorder.next(&mut visited) {
            debug_assert!(!is_pinned_node(ctx.graph(), pred));
            debug_assert!(schedule[pred].is_none());
            schedule[pred] = schedule_node(&schedule, pred).into();
        }
    }

    schedule
}

pub fn schedule_late(
    ctx: &ScheduleCtx<'_>,
    mut schedule_node: impl FnMut(&ByNodeSchedule, Node) -> DomTreeNode,
) -> ByNodeSchedule {
    let mut schedule = ctx.pinned_nodes.clone();
    let mut visited = EntitySet::new();
    let mut data_succ_postorder =
        PostOrderContext::new(UnpinnedDataSuccs::new(ctx.graph(), ctx.live_nodes()), []);

    for pinned in ctx.walk_pinned_nodes() {
        data_succ_postorder.reset(
            unpinned_dataflow_succs(ctx.graph(), ctx.live_nodes(), pinned)
                .map(|(node, _input_idx)| node),
        );
        while let Some(succ) = data_succ_postorder.next(&mut visited) {
            debug_assert!(!is_pinned_node(ctx.graph(), succ));
            debug_assert!(schedule[succ].is_none());
            schedule[succ] = schedule_node(&schedule, succ).into();
        }
    }

    for &root in ctx.live_node_info().roots() {
        if schedule[root].is_none() {
            debug_assert!(
                !is_pinned_node(ctx.graph(), root),
                "unscheduled liveness root has control outputs"
            );
            schedule[root] = schedule_node(&schedule, root).into();
        }
    }

    schedule
}

fn is_pinned_node(graph: &ValGraph, node: Node) -> bool {
    graph
        .node_inputs(node)
        .into_iter()
        .any(|input| graph.value_kind(input).is_control_or_phisel())
        || graph
            .node_outputs(node)
            .into_iter()
            .any(|input| graph.value_kind(input).is_control())
}

fn unpinned_dataflow_preds(graph: &ValGraph, node: Node) -> impl Iterator<Item = Node> + '_ {
    dataflow_preds(graph, node).filter(move |&pred| !is_pinned_node(graph, pred))
}

struct UnpinnedDataPreds<'a> {
    graph: &'a ValGraph,
}

impl<'a> UnpinnedDataPreds<'a> {
    fn new(graph: &'a ValGraph) -> Self {
        Self { graph }
    }
}

impl<'a> graphwalk::Graph for UnpinnedDataPreds<'a> {
    type Node = Node;

    fn successors(&self, node: Node, f: impl FnMut(Node)) {
        unpinned_dataflow_preds(self.graph, node).for_each(f);
    }
}

fn unpinned_dataflow_succs<'a>(
    graph: &'a ValGraph,
    live_nodes: &'a EntitySet<Node>,
    node: Node,
) -> impl Iterator<Item = (Node, u32)> + 'a {
    dataflow_succs(graph, live_nodes, node)
        .filter(|&(succ, _input_idx)| !is_pinned_node(graph, succ))
}

struct UnpinnedDataSuccs<'a> {
    graph: &'a ValGraph,
    live_nodes: &'a EntitySet<Node>,
}

impl<'a> UnpinnedDataSuccs<'a> {
    fn new(graph: &'a ValGraph, live_nodes: &'a EntitySet<Node>) -> Self {
        Self { graph, live_nodes }
    }
}

impl<'a> graphwalk::Graph for UnpinnedDataSuccs<'a> {
    type Node = Node;

    fn successors(&self, node: Node, mut f: impl FnMut(Node)) {
        unpinned_dataflow_succs(self.graph, self.live_nodes, node)
            .for_each(|(succ, _input_idx)| f(succ));
    }
}

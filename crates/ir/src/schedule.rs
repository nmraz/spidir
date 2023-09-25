use core::iter;

use cranelift_entity::EntitySet;
use graphwalk::PostOrderContext;

use crate::{
    valgraph::{Node, ValGraph},
    valwalk::{dataflow_preds, dataflow_succs, get_attached_phis, LiveNodeInfo},
};

pub struct ScheduleCtx<'a> {
    graph: &'a ValGraph,
    cfg_preorder: &'a [Node],
    live_node_info: &'a LiveNodeInfo,
}

impl<'a> ScheduleCtx<'a> {
    pub fn new(
        graph: &'a ValGraph,
        live_node_info: &'a LiveNodeInfo,
        cfg_preorder: &'a [Node],
    ) -> Self {
        Self {
            graph,
            cfg_preorder,
            live_node_info,
        }
    }

    #[inline]
    pub fn graph(&self) -> &ValGraph {
        self.graph
    }

    #[inline]
    pub fn cfg_preorder(&self) -> &[Node] {
        self.cfg_preorder
    }

    #[inline]
    pub fn live_node_info(&self) -> &LiveNodeInfo {
        self.live_node_info
    }

    #[inline]
    pub fn live_nodes(&self) -> &EntitySet<Node> {
        self.live_node_info.live_nodes()
    }

    #[inline]
    pub fn is_live(&self, node: Node) -> bool {
        self.live_nodes().contains(node)
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

pub trait PinNodes {
    type Block: Copy;
    fn control_node_block(&self, node: Node) -> Self::Block;
    fn pin(&mut self, ctx: &ScheduleCtx<'_>, node: Node, block: Self::Block);
}

pub fn pin_nodes(ctx: &ScheduleCtx<'_>, pin_nodes: &mut impl PinNodes) {
    for &node in ctx.cfg_preorder() {
        let block = pin_nodes.control_node_block(node);
        pin_nodes.pin(ctx, node, block);
        for phi in ctx.get_attached_phis(node) {
            pin_nodes.pin(ctx, phi, block);
        }
    }
}

pub fn schedule_early(ctx: &ScheduleCtx<'_>, mut schedule: impl FnMut(&ScheduleCtx<'_>, Node)) {
    let mut visited = EntitySet::new();
    let mut data_pred_postorder = PostOrderContext::new(UnpinnedDataPreds::new(ctx.graph()), []);

    for pinned in ctx.walk_pinned_nodes() {
        data_pred_postorder.reset(unpinned_dataflow_preds(ctx.graph(), pinned));
        while let Some(pred) = data_pred_postorder.next(&mut visited) {
            debug_assert!(!is_pinned_node(ctx.graph(), pred));
            schedule(ctx, pred);
        }
    }
}

pub fn schedule_late(ctx: &ScheduleCtx<'_>, mut schedule: impl FnMut(&ScheduleCtx<'_>, Node)) {
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
            schedule(ctx, succ);
        }
    }

    let entry = ctx.cfg_preorder()[0];
    for &root in ctx.live_node_info().roots() {
        if root != entry {
            debug_assert!(
                !is_pinned_node(ctx.graph(), root),
                "unscheduled liveness root has control outputs"
            );
            schedule(ctx, root);
        }
    }
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

impl<'a> graphwalk::GraphRef for UnpinnedDataPreds<'a> {
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

impl<'a> graphwalk::GraphRef for UnpinnedDataSuccs<'a> {
    type Node = Node;

    fn successors(&self, node: Node, mut f: impl FnMut(Node)) {
        unpinned_dataflow_succs(self.graph, self.live_nodes, node)
            .for_each(|(succ, _input_idx)| f(succ));
    }
}

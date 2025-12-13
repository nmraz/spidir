use core::{iter, ops::ControlFlow};

use entity_utils::set::DenseEntitySet;
use graphwalk::PostOrderContext;

use crate::{
    node::NodeKind,
    valgraph::{Node, ValGraph},
    valwalk::{GraphWalkInfo, dataflow_preds, dataflow_succs, get_attached_phis},
};

pub struct ScheduleContext<'a> {
    pub graph: &'a ValGraph,
    pub cfg_preorder: &'a [Node],
    pub walk_info: &'a GraphWalkInfo,
}

impl<'a> ScheduleContext<'a> {
    pub fn new(
        graph: &'a ValGraph,
        walk_info: &'a GraphWalkInfo,
        cfg_preorder: &'a [Node],
    ) -> Self {
        Self {
            graph,
            cfg_preorder,
            walk_info,
        }
    }

    #[inline]
    pub fn live_nodes(&self) -> &DenseEntitySet<Node> {
        &self.walk_info.live_nodes
    }

    pub fn walk_pinned_nodes(&self) -> impl Iterator<Item = Node> + '_ {
        self.cfg_preorder
            .iter()
            .flat_map(move |&cfg_node| iter::once(cfg_node).chain(self.get_attached_phis(cfg_node)))
    }

    pub fn get_attached_phis(&self, cfg_node: Node) -> impl Iterator<Item = Node> + '_ {
        get_attached_phis(self.graph, cfg_node).filter(move |&phi| self.live_nodes().contains(phi))
    }
}

pub fn schedule_early(
    ctx: &ScheduleContext<'_>,
    scratch_postorder: &mut PostOrderContext<Node>,
    mut schedule: impl FnMut(&ScheduleContext<'_>, Node),
) {
    let mut visited = DenseEntitySet::new();
    let unpinned_data_preds = UnpinnedDataPreds::new(ctx.graph, ctx.live_nodes());

    for pinned in ctx.walk_pinned_nodes() {
        scratch_postorder.reset(unpinned_dataflow_preds(ctx.graph, ctx.live_nodes(), pinned));
        while let Some(pred) = scratch_postorder.next(&unpinned_data_preds, &mut visited) {
            debug_assert!(!is_pinned_node(ctx.graph, pred));
            schedule(ctx, pred);
        }
    }
}

pub fn schedule_late(
    ctx: &ScheduleContext<'_>,
    scratch_postorder: &mut PostOrderContext<Node>,
    mut schedule: impl FnMut(&ScheduleContext<'_>, Node),
) {
    let mut visited = DenseEntitySet::new();
    let unpinned_data_succs = UnpinnedDataSuccs::new(ctx.graph, ctx.live_nodes());

    for pinned in ctx.walk_pinned_nodes() {
        scratch_postorder.reset(
            unpinned_dataflow_succs(ctx.graph, ctx.live_nodes(), pinned)
                .map(|(node, _input_idx)| node),
        );
        while let Some(succ) = scratch_postorder.next(&unpinned_data_succs, &mut visited) {
            debug_assert!(!is_pinned_node(ctx.graph, succ));
            schedule(ctx, succ);
        }
    }

    for &root in &ctx.walk_info.roots {
        // We want only floating data nodes here, not dead regions and the like.
        if !is_pinned_node(ctx.graph, root) {
            scratch_postorder.reset([root]);
            while let Some(succ) = scratch_postorder.next(&unpinned_data_succs, &mut visited) {
                debug_assert!(!is_pinned_node(ctx.graph, succ));
                schedule(ctx, succ);
            }
        }
    }
}

fn is_pinned_node(graph: &ValGraph, node: Node) -> bool {
    let kind = graph.node_kind(node);
    kind.has_control_flow() || matches!(kind, NodeKind::Phi)
}

fn unpinned_dataflow_preds<'a>(
    graph: &'a ValGraph,
    live_nodes: &'a DenseEntitySet<Node>,
    node: Node,
) -> impl Iterator<Item = Node> + 'a {
    dataflow_preds(graph, node)
        .filter(|&pred| live_nodes.contains(pred))
        .filter(move |&pred| !is_pinned_node(graph, pred))
}

struct UnpinnedDataPreds<'a> {
    graph: &'a ValGraph,
    live_nodes: &'a DenseEntitySet<Node>,
}

impl<'a> UnpinnedDataPreds<'a> {
    fn new(graph: &'a ValGraph, live_nodes: &'a DenseEntitySet<Node>) -> Self {
        Self { graph, live_nodes }
    }
}

impl graphwalk::GraphRef for UnpinnedDataPreds<'_> {
    type Node = Node;

    fn try_successors(
        &self,
        node: Node,
        f: impl FnMut(Node) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        unpinned_dataflow_preds(self.graph, self.live_nodes, node).try_for_each(f)
    }
}

fn unpinned_dataflow_succs<'a>(
    graph: &'a ValGraph,
    live_nodes: &'a DenseEntitySet<Node>,
    node: Node,
) -> impl Iterator<Item = (Node, u32)> + 'a {
    dataflow_succs(graph, live_nodes, node)
        .filter(|&(succ, _input_idx)| !is_pinned_node(graph, succ))
}

struct UnpinnedDataSuccs<'a> {
    graph: &'a ValGraph,
    live_nodes: &'a DenseEntitySet<Node>,
}

impl<'a> UnpinnedDataSuccs<'a> {
    fn new(graph: &'a ValGraph, live_nodes: &'a DenseEntitySet<Node>) -> Self {
        Self { graph, live_nodes }
    }
}

impl graphwalk::GraphRef for UnpinnedDataSuccs<'_> {
    type Node = Node;

    fn try_successors(
        &self,
        node: Node,
        mut f: impl FnMut(Node) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        unpinned_dataflow_succs(self.graph, self.live_nodes, node)
            .try_for_each(|(succ, _input_idx)| f(succ))
    }
}

#[cfg(test)]
mod tests;

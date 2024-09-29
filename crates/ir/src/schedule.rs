use core::{iter, ops::ControlFlow};

use cranelift_entity::EntitySet;
use graphwalk::PostOrderContext;

use crate::{
    node::NodeKind,
    valgraph::{Node, ValGraph},
    valwalk::{dataflow_preds, dataflow_succs, get_attached_phis, LiveNodeInfo},
};

pub struct ScheduleContext<'a> {
    pub graph: &'a ValGraph,
    pub cfg_preorder: &'a [Node],
    pub live_node_info: &'a LiveNodeInfo,
}

impl<'a> ScheduleContext<'a> {
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

pub fn schedule_early(
    ctx: &ScheduleContext<'_>,
    scratch_postorder: &mut PostOrderContext<Node>,
    mut schedule: impl FnMut(&ScheduleContext<'_>, Node),
) {
    let mut visited = EntitySet::new();
    let unpinned_data_preds = UnpinnedDataPreds::new(ctx.graph);

    for pinned in ctx.walk_pinned_nodes() {
        scratch_postorder.reset(unpinned_dataflow_preds(ctx.graph, pinned));
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
    let mut visited = EntitySet::new();
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

    let entry = ctx.cfg_preorder[0];
    for &root in ctx.live_node_info.roots() {
        if root != entry {
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

    fn successors(&self, node: Node, f: impl FnMut(Node) -> ControlFlow<()>) -> ControlFlow<()> {
        unpinned_dataflow_preds(self.graph, node).try_for_each(f)
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

    fn successors(
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

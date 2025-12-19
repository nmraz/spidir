use core::iter;

use entity_utils::set::DenseEntitySet;
use graphwalk::PostOrderContext;

use crate::{
    valgraph::{Node, ValGraph},
    valwalk::{
        GraphWalkInfo, UnpinnedDataflowPreds, UnpinnedDataflowSuccs, get_attached_phis,
        is_cfg_pinned_node, unpinned_dataflow_preds, unpinned_dataflow_succs,
    },
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
    let unpinned_data_preds = UnpinnedDataflowPreds::new(ctx.graph, ctx.live_nodes());

    for pinned in ctx.walk_pinned_nodes() {
        scratch_postorder.reset(unpinned_dataflow_preds(ctx.graph, ctx.live_nodes(), pinned));
        while let Some(pred) = scratch_postorder.next(&unpinned_data_preds, &mut visited) {
            debug_assert!(!is_cfg_pinned_node(ctx.graph, pred));
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
    let unpinned_data_succs = UnpinnedDataflowSuccs::new(ctx.graph, ctx.live_nodes());

    for pinned in ctx.walk_pinned_nodes() {
        scratch_postorder.reset(
            unpinned_dataflow_succs(ctx.graph, ctx.live_nodes(), pinned)
                .map(|(node, _input_idx)| node),
        );
        while let Some(succ) = scratch_postorder.next(&unpinned_data_succs, &mut visited) {
            debug_assert!(!is_cfg_pinned_node(ctx.graph, succ));
            schedule(ctx, succ);
        }
    }

    for &root in &ctx.walk_info.roots {
        // We want only floating data nodes here, not dead regions and the like.
        if !is_cfg_pinned_node(ctx.graph, root) {
            scratch_postorder.reset([root]);
            while let Some(succ) = scratch_postorder.next(&unpinned_data_succs, &mut visited) {
                debug_assert!(!is_cfg_pinned_node(ctx.graph, succ));
                schedule(ctx, succ);
            }
        }
    }
}

#[cfg(test)]
mod tests;

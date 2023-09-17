use core::iter;

use alloc::vec::Vec;

use cranelift_entity::{packed_option::PackedOption, EntitySet, SecondaryMap};
use graphwalk::PostOrderContext;

use crate::{
    domtree::{DomTree, TreeNode},
    valgraph::{Node, ValGraph},
    valwalk::{dataflow_preds, get_attached_phis, LiveNodeInfo},
};

pub type ByNodeSchedule = SecondaryMap<Node, PackedOption<TreeNode>>;

pub struct ScheduleCtx<'a> {
    graph: &'a ValGraph,
    domtree: &'a DomTree,
    domtree_preorder: Vec<TreeNode>,
    live_node_info: LiveNodeInfo,
    pinned_nodes: ByNodeSchedule,
}

impl<'a> ScheduleCtx<'a> {
    pub fn prepare(graph: &'a ValGraph, domtree: &'a DomTree) -> Self {
        let domtree_preorder: Vec<_> = domtree.preorder().collect();
        let live_node_info = LiveNodeInfo::compute(graph, domtree.get_cfg_node(domtree.root()));
        let mut pinned_nodes = ByNodeSchedule::new();

        for &domtree_node in &domtree_preorder {
            let cfg_node = domtree.get_cfg_node(domtree_node);
            pinned_nodes[cfg_node] = domtree_node.into();
            for phi in get_attached_phis(graph, cfg_node) {
                pinned_nodes[phi] = domtree_node.into();
            }
        }

        Self {
            graph,
            domtree,
            domtree_preorder,
            live_node_info,
            pinned_nodes,
        }
    }

    pub fn graph(&self) -> &ValGraph {
        self.graph
    }

    pub fn domtree(&self) -> &DomTree {
        self.domtree
    }

    pub fn domtree_preorder(&self) -> &[TreeNode] {
        &self.domtree_preorder
    }

    pub fn live_node_info(&self) -> &LiveNodeInfo {
        &self.live_node_info
    }

    pub fn live_nodes(&self) -> &EntitySet<Node> {
        self.live_node_info.live_nodes()
    }

    pub fn is_live(&self, node: Node) -> bool {
        self.live_nodes().contains(node)
    }

    pub fn pinned_nodes(&self) -> &ByNodeSchedule {
        &self.pinned_nodes
    }

    pub fn walk_pinned_nodes(&self) -> impl Iterator<Item = Node> + '_ {
        self.domtree_preorder.iter().flat_map(move |&domtree_node| {
            let cfg_node = self.domtree.get_cfg_node(domtree_node);
            iter::once(cfg_node).chain(self.get_attached_phis(cfg_node))
        })
    }

    pub fn get_attached_phis(&self, cfg_node: Node) -> impl Iterator<Item = Node> + '_ {
        get_attached_phis(self.graph, cfg_node).filter(move |&phi| self.is_live(phi))
    }
}

pub fn schedule_early(
    ctx: &ScheduleCtx<'_>,
    mut schedule_node: impl FnMut(&ByNodeSchedule, Node) -> TreeNode,
) -> ByNodeSchedule {
    let mut schedule = ctx.pinned_nodes.clone();
    let mut visited = EntitySet::new();
    let mut data_pred_postorder =
        DataPredPostOrder::new(UnpinnedDataPreds::new(ctx.graph(), ctx.pinned_nodes()), []);

    for pinned in ctx.walk_pinned_nodes() {
        data_pred_postorder.reset([pinned]);
        while let Some(pred) = data_pred_postorder.next(&mut visited) {
            if !is_pinned_node(ctx.graph, pred) {
                debug_assert!(schedule[pred].is_none());
                schedule[pred] = schedule_node(&schedule, pred).into();
            }
        }
    }

    schedule
}

pub fn is_pinned_node(graph: &ValGraph, node: Node) -> bool {
    graph
        .node_inputs(node)
        .into_iter()
        .any(|input| graph.value_kind(input).is_control_or_phisel())
        || graph
            .node_outputs(node)
            .into_iter()
            .any(|input| graph.value_kind(input).is_control())
}

struct UnpinnedDataPreds<'a> {
    graph: &'a ValGraph,
    pinned_nodes: &'a ByNodeSchedule,
}

impl<'a> UnpinnedDataPreds<'a> {
    fn new(graph: &'a ValGraph, pinned_nodes: &'a ByNodeSchedule) -> Self {
        Self {
            graph,
            pinned_nodes,
        }
    }
}

impl<'a> graphwalk::Graph for UnpinnedDataPreds<'a> {
    type Node = Node;

    fn successors(&self, node: Node, mut f: impl FnMut(Node)) {
        for pred in dataflow_preds(self.graph, node) {
            if self.pinned_nodes[pred].is_none() {
                f(pred);
            }
        }
    }
}

type DataPredPostOrder<'a> = PostOrderContext<UnpinnedDataPreds<'a>>;

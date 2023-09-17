use alloc::vec::Vec;

use cranelift_entity::{
    packed_option::PackedOption, EntityList, EntitySet, ListPool, SecondaryMap,
};
use graphwalk::{Graph, PostOrderContext};
use ir::{
    domtree::{DomTree, TreeNode as DomTreeNode},
    loop_forest::LoopForest,
    node::NodeKind,
    valgraph::{Node, ValGraph},
    valwalk::{get_attached_phis, LiveNodeInfo},
};

pub struct Schedule {
    schedule: SecondaryMap<DomTreeNode, EntityList<Node>>,
    node_list_pool: ListPool<Node>,
}

impl Schedule {
    pub fn compute(graph: &ValGraph, domtree: &DomTree, loop_forest: &LoopForest) -> Self {
        let ctx = ScheduleCtx::prepare(graph, domtree, loop_forest);

        let early_schedule = schedule_early(&ctx);

        schedule_late(&ctx, &early_schedule);

        todo!()
    }

    pub fn attached_nodes(&self, domtree_node: DomTreeNode) -> &[Node] {
        self.schedule
            .get(domtree_node)
            .map_or(&[], |attached_nodes| {
                attached_nodes.as_slice(&self.node_list_pool)
            })
    }
}

type ByNodeSchedule = SecondaryMap<Node, PackedOption<DomTreeNode>>;

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

impl<'a> Graph for UnpinnedDataPreds<'a> {
    type Node = Node;

    fn successors(&self, node: Node, mut f: impl FnMut(Node)) {
        for pred in data_preds(self.graph, node) {
            if self.pinned_nodes[node].is_none() {
                f(pred);
            }
        }
    }
}

type DataPredPostOrder<'a> = PostOrderContext<UnpinnedDataPreds<'a>>;

struct UnpinnedDataSuccs<'a> {
    graph: &'a ValGraph,
    live_nodes: &'a EntitySet<Node>,
    pinned_nodes: &'a ByNodeSchedule,
}

impl<'a> UnpinnedDataSuccs<'a> {
    fn new(
        graph: &'a ValGraph,
        live_nodes: &'a EntitySet<Node>,
        pinned_nodes: &'a ByNodeSchedule,
    ) -> Self {
        Self {
            graph,
            live_nodes,
            pinned_nodes,
        }
    }
}

impl<'a> Graph for UnpinnedDataSuccs<'a> {
    type Node = Node;

    fn successors(&self, node: Node, mut f: impl FnMut(Node)) {
        for (succ, _input_idx) in data_succs(self.graph, self.live_nodes, node) {
            if self.pinned_nodes[succ].is_none() {
                f(succ);
            }
        }
    }
}

struct ScheduleCtx<'a> {
    graph: &'a ValGraph,
    domtree: &'a DomTree,
    loop_forest: &'a LoopForest,
    domtree_preorder: Vec<DomTreeNode>,
    live_node_info: LiveNodeInfo,
    pinned_nodes: ByNodeSchedule,
}

impl<'a> ScheduleCtx<'a> {
    fn prepare(graph: &'a ValGraph, domtree: &'a DomTree, loop_forest: &'a LoopForest) -> Self {
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
            loop_forest,
            domtree_preorder,
            live_node_info,
            pinned_nodes,
        }
    }
}

fn schedule_early(ctx: &ScheduleCtx<'_>) -> ByNodeSchedule {
    let mut schedule = ctx.pinned_nodes.clone();
    let mut visited = EntitySet::new();

    let mut input_postorder =
        PostOrderContext::new(UnpinnedDataPreds::new(ctx.graph, &ctx.pinned_nodes), []);

    for &domtree_node in &ctx.domtree_preorder {
        let cfg_node = ctx.domtree.get_cfg_node(domtree_node);
        schedule_inputs_early(
            ctx,
            &mut input_postorder,
            &mut visited,
            &mut schedule,
            cfg_node,
        );

        for phi in get_attached_phis(ctx.graph, cfg_node) {
            if ctx.live_node_info.live_nodes().contains(phi) {
                schedule_inputs_early(ctx, &mut input_postorder, &mut visited, &mut schedule, phi);
            }
        }
    }

    schedule
}

fn schedule_inputs_early(
    ctx: &ScheduleCtx<'_>,
    input_postorder: &mut DataPredPostOrder<'_>,
    visited: &mut EntitySet<Node>,
    schedule: &mut ByNodeSchedule,
    node: Node,
) {
    input_postorder.reset([node]);
    while let Some(data_pred) = input_postorder.next(visited) {
        debug_assert!(ctx.pinned_nodes[data_pred].is_none());
        debug_assert!(schedule[data_pred].is_none());
        schedule[data_pred] = early_schedule_location(ctx, schedule, data_pred).into();
    }
}

fn early_schedule_location(
    ctx: &ScheduleCtx<'_>,
    schedule: &ByNodeSchedule,
    node: Node,
) -> DomTreeNode {
    data_preds(ctx.graph, node)
        .map(|pred| schedule[pred].expect("data flow cycle or dead input"))
        .reduce(|acc, cur| {
            // Find the deepest input schedule location in the dominator tree, assuming they are
            // totally ordered by dominance.
            if ctx.domtree.dominates(acc, cur) {
                cur
            } else {
                acc
            }
        })
        .unwrap_or(ctx.domtree.root())
}

fn schedule_late(ctx: &ScheduleCtx<'_>, early_schedule: &ByNodeSchedule) -> ByNodeSchedule {
    let depth_map = get_cfg_depth_map(ctx);
    let mut late_visited_nodes = EntitySet::new();
    let mut late_schedule = ctx.pinned_nodes.clone();
    let mut output_postorder = PostOrderContext::new(
        UnpinnedDataSuccs::new(
            ctx.graph,
            ctx.live_node_info.live_nodes(),
            &ctx.pinned_nodes,
        ),
        [],
    );

    for &domtree_node in &ctx.domtree_preorder {
        let cfg_node = ctx.domtree.get_cfg_node(domtree_node);
        output_postorder.reset([cfg_node]);
        while let Some(data_succ) = output_postorder.next(&mut late_visited_nodes) {
            schedule_node_late(
                ctx,
                &depth_map,
                early_schedule,
                &mut late_schedule,
                data_succ,
            );
        }

        for phi in get_attached_phis(ctx.graph, cfg_node) {
            if ctx.live_node_info.live_nodes().contains(phi) {
                output_postorder.reset([phi]);
                while let Some(data_succ) = output_postorder.next(&mut late_visited_nodes) {
                    schedule_node_late(
                        ctx,
                        &depth_map,
                        early_schedule,
                        &mut late_schedule,
                        data_succ,
                    );
                }
            }
        }
    }

    for &root in ctx.live_node_info.roots() {
        if late_schedule[root].is_none() {
            schedule_node_late(ctx, &depth_map, early_schedule, &mut late_schedule, root);
        }
    }

    late_schedule
}

fn schedule_node_late(
    ctx: &ScheduleCtx<'_>,
    depth_map: &CfgDepthMap,
    early_schedule: &ByNodeSchedule,
    late_schedule: &mut ByNodeSchedule,
    node: Node,
) {
    debug_assert!(ctx.pinned_nodes[node].is_none());
    debug_assert!(late_schedule[node].is_none());

    let early_loc = early_schedule[node].unwrap();
    let mut late_loc = late_schedule_location(ctx, depth_map, late_schedule, node);

    debug_assert!(ctx.domtree.dominates(early_loc, late_loc));

    // Select a node between `early_loc` and `late_loc` on the dominator
    // tree that has minimal loop depth, favoring deeper nodes (those closer to `late_loc`).
    let mut best_loc = late_loc;
    while late_loc != early_loc {
        if depth_map[late_loc].loop_depth < depth_map[best_loc].loop_depth {
            best_loc = late_loc;
        }
        late_loc = ctx.domtree.idom(late_loc).unwrap();
    }

    late_schedule[node] = best_loc.into();
}

fn late_schedule_location(
    ctx: &ScheduleCtx<'_>,
    depth_map: &CfgDepthMap,
    schedule: &ByNodeSchedule,
    node: Node,
) -> DomTreeNode {
    data_succs(ctx.graph, ctx.live_node_info.live_nodes(), node)
        .filter_map(|(succ, input_idx)| {
            if ctx.graph.node_kind(node) != &NodeKind::Phi {
                // Plain data nodes using the value should always have a schedule if they are live.
                Some(schedule[succ].expect("data flow cycle"))
            } else {
                let region = ctx.graph.value_def(ctx.graph.node_inputs(node)[0]).0;

                // Note: the corresponding region input might be dead even when the phi is live;
                // this just means that this instance doesn't count as a use and can be skipped.
                ctx.domtree.get_tree_node(
                    ctx.graph
                        .value_def(ctx.graph.node_inputs(region)[input_idx as usize])
                        .0,
                )
            }
        })
        .reduce(|acc, cur| domtree_lca(ctx.domtree, depth_map, acc, cur))
        .expect("live non-pinned node has no data uses")
}

#[derive(Clone, Copy, Default)]
struct CfgDepthData {
    domtree_depth: u32,
    loop_depth: u32,
}

type CfgDepthMap = SecondaryMap<DomTreeNode, CfgDepthData>;

fn get_cfg_depth_map(ctx: &ScheduleCtx<'_>) -> CfgDepthMap {
    let mut depth_map = CfgDepthMap::new();

    for &domtree_node in &ctx.domtree_preorder {
        // TODO: `loop_depth` is linear in the loop depth, which is probably okay in practice, but
        // can cause worst-case quadratic behavior.
        let loop_depth = ctx
            .loop_forest
            .containing_loop(domtree_node)
            .map_or(0, |containing_loop| {
                ctx.loop_forest.loop_depth(containing_loop)
            });
        let domtree_depth = ctx
            .domtree
            .idom(domtree_node)
            .map_or(0, |idom| depth_map[idom].domtree_depth + 1);

        depth_map[domtree_node] = CfgDepthData {
            domtree_depth,
            loop_depth,
        };
    }

    depth_map
}

fn domtree_lca(
    domtree: &DomTree,
    depth_map: &CfgDepthMap,
    mut a: DomTreeNode,
    mut b: DomTreeNode,
) -> DomTreeNode {
    while depth_map[a].domtree_depth > depth_map[b].domtree_depth {
        // Note: `a` must have an immediate dominator here because its depth is nonzero.
        a = domtree.idom(a).unwrap();
    }

    while depth_map[b].domtree_depth > depth_map[a].domtree_depth {
        // Note: `b` must have an immediate dominator here because its depth is nonzero.
        b = domtree.idom(b).unwrap();
    }

    debug_assert!(depth_map[a].domtree_depth == depth_map[b].domtree_depth);
    while a != b {
        a = domtree.idom(a).unwrap();
        b = domtree.idom(b).unwrap();
    }

    a
}

fn data_preds(graph: &ValGraph, node: Node) -> impl Iterator<Item = Node> + '_ {
    graph
        .node_inputs(node)
        .into_iter()
        .filter(move |&input| graph.value_kind(input).is_value())
        .map(move |input| graph.value_def(input).0)
}

fn data_succs<'a>(
    graph: &'a ValGraph,
    live_nodes: &'a EntitySet<Node>,
    node: Node,
) -> impl Iterator<Item = (Node, u32)> + 'a {
    graph
        .node_outputs(node)
        .into_iter()
        .filter(move |&output| graph.value_kind(output).is_value())
        .flat_map(|output| graph.value_uses(output))
        .filter(move |&(succ, _)| live_nodes.contains(succ))
}

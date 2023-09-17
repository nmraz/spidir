use cranelift_entity::{EntityList, ListPool, SecondaryMap};
use ir::{
    domtree::{DomTree, TreeNode as DomTreeNode},
    loop_forest::LoopForest,
    node::NodeKind,
    schedule::{schedule_early, schedule_late, ByNodeSchedule, ScheduleCtx},
    valgraph::{Node, ValGraph},
    valwalk::{dataflow_preds, dataflow_succs},
};

pub struct Schedule {
    schedule: SecondaryMap<DomTreeNode, EntityList<Node>>,
    node_list_pool: ListPool<Node>,
}

impl Schedule {
    pub fn compute(graph: &ValGraph, domtree: &DomTree, loop_forest: &LoopForest) -> Self {
        let ctx = ScheduleCtx::prepare(graph, domtree);
        let mut final_schedule = Self {
            schedule: SecondaryMap::new(),
            node_list_pool: ListPool::new(),
        };

        let early_schedule = schedule_early(&ctx, |schedule, node| {
            early_schedule_location(&ctx, schedule, node)
        });

        let depth_map = get_cfg_depth_map(&ctx, loop_forest);
        schedule_late(&ctx, |late_schedule, node| {
            let loc =
                best_schedule_location(&ctx, &depth_map, &early_schedule, late_schedule, node);
            final_schedule.push_node(loc, node);
            loc
        });

        // Push the phi nodes for every region on last (which means they will actually show up
        // first, because the attached node lists are reversed).
        for &domtree_node in ctx.domtree_preorder() {
            for phi in ctx.get_attached_phis(domtree.get_cfg_node(domtree_node)) {
                final_schedule.push_node(domtree_node, phi);
            }
        }

        final_schedule
    }

    pub fn attached_nodes_rev(&self, domtree_node: DomTreeNode) -> &[Node] {
        self.schedule
            .get(domtree_node)
            .map_or(&[], |attached_nodes| {
                attached_nodes.as_slice(&self.node_list_pool)
            })
    }

    fn push_node(&mut self, loc: DomTreeNode, node: Node) {
        self.schedule[loc].push(node, &mut self.node_list_pool);
    }
}

fn early_schedule_location(
    ctx: &ScheduleCtx<'_>,
    schedule: &ByNodeSchedule,
    node: Node,
) -> DomTreeNode {
    dataflow_preds(ctx.graph(), node)
        .map(|pred| schedule[pred].expect("data flow cycle or dead input"))
        .reduce(|acc, cur| {
            // Find the deepest input schedule location in the dominator tree, assuming they are
            // totally ordered by dominance.
            if ctx.domtree().dominates(acc, cur) {
                cur
            } else {
                acc
            }
        })
        .unwrap_or(ctx.domtree().root())
}

fn best_schedule_location(
    ctx: &ScheduleCtx<'_>,
    depth_map: &CfgDepthMap,
    early_schedule: &ByNodeSchedule,
    late_schedule: &ByNodeSchedule,
    node: Node,
) -> DomTreeNode {
    let early_loc = early_schedule[node].unwrap();
    let mut late_loc = late_schedule_location(ctx, depth_map, late_schedule, node);

    debug_assert!(ctx.domtree().dominates(early_loc, late_loc));

    // Select a node between `early_loc` and `late_loc` on the dominator
    // tree that has minimal loop depth, favoring deeper nodes (those closer to `late_loc`).
    let mut best_loc = late_loc;
    while late_loc != early_loc {
        if depth_map[late_loc].loop_depth < depth_map[best_loc].loop_depth {
            best_loc = late_loc;
        }
        late_loc = ctx.domtree().idom(late_loc).unwrap();
    }

    best_loc
}

fn late_schedule_location(
    ctx: &ScheduleCtx<'_>,
    depth_map: &CfgDepthMap,
    schedule: &ByNodeSchedule,
    node: Node,
) -> DomTreeNode {
    let graph = ctx.graph();
    let domtree = ctx.domtree();

    dataflow_succs(ctx.graph(), ctx.live_nodes(), node)
        .filter_map(|(succ, input_idx)| {
            if graph.node_kind(node) != &NodeKind::Phi {
                // Plain data nodes using the value should always have a schedule if they are live.
                Some(schedule[succ].expect("data flow cycle"))
            } else {
                let region = graph.value_def(graph.node_inputs(node)[0]).0;

                // Note: the corresponding region input might be dead even when the phi is live;
                // this just means that this instance doesn't count as a use and can be skipped.
                domtree.get_tree_node(
                    graph
                        .value_def(graph.node_inputs(region)[input_idx as usize])
                        .0,
                )
            }
        })
        .reduce(|acc, cur| domtree_lca(domtree, depth_map, acc, cur))
        .expect("live non-pinned node has no data uses")
}

#[derive(Clone, Copy, Default)]
struct CfgDepthData {
    domtree_depth: u32,
    loop_depth: u32,
}

type CfgDepthMap = SecondaryMap<DomTreeNode, CfgDepthData>;

fn get_cfg_depth_map(ctx: &ScheduleCtx<'_>, loop_forest: &LoopForest) -> CfgDepthMap {
    let mut depth_map = CfgDepthMap::new();

    for &domtree_node in ctx.domtree_preorder() {
        // TODO: `loop_depth` is linear in the loop depth, which is probably okay in practice, but
        // can cause worst-case quadratic behavior.
        let loop_depth = loop_forest
            .containing_loop(domtree_node)
            .map_or(0, |containing_loop| loop_forest.loop_depth(containing_loop));
        let domtree_depth = ctx
            .domtree()
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

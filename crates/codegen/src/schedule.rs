use cranelift_entity::{EntityList, ListPool, SecondaryMap};
use ir::{
    domtree::{DomTree, TreeNode as DomTreeNode},
    loop_forest::LoopForest,
    node::NodeKind,
    schedule::{schedule_early, schedule_late, ByNodeSchedule, ScheduleCtx},
    valgraph::{Node, ValGraph},
    valwalk::{dataflow_preds, dataflow_succs},
};
use log::{log_enabled, trace};

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

        if log_enabled!(log::Level::Trace) {
            for (pinned, loc) in ctx.pinned_nodes().iter() {
                if let Some(loc) = loc.expand() {
                    trace!("pinned: graph {} -> CFG {}", pinned.as_u32(), loc.as_u32());
                }
            }
        }

        let early_schedule = schedule_early(&ctx, |schedule, node| {
            trace!("early: graph {}", node.as_u32());
            let loc = early_schedule_location(&ctx, schedule, node);
            trace!("early: graph {} -> CFG {}", node.as_u32(), loc.as_u32());
            loc
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
        .map(|pred| {
            let pred_loc = schedule[pred].expect("data flow cycle or dead input");
            trace!(
                "    pred: graph {} (CFG {})",
                pred.as_u32(),
                pred_loc.as_u32()
            );
            pred_loc
        })
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
    trace!("late: graph {}", node.as_u32());
    let mut late_loc = late_schedule_location(ctx, depth_map, late_schedule, node);
    trace!("late: graph {} -> CFG {}", node.as_u32(), late_loc.as_u32());

    let early_loc = early_schedule[node].unwrap();
    trace!(
        "place: graph {} in CFG range ({}, {})",
        node.as_u32(),
        early_loc.as_u32(),
        late_loc.as_u32()
    );

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

    // Make sure we don't land on nodes that don't have exactly one control output, since we can't
    // actually "place" things there.
    while !depth_map[best_loc].single_ctrl_output {
        best_loc = ctx
            .domtree()
            .idom(best_loc)
            .expect("entry node should have single control output");
    }

    debug_assert!(ctx.domtree().dominates(early_loc, best_loc));

    trace!(
        "place: graph {} -> CFG {}",
        node.as_u32(),
        best_loc.as_u32()
    );

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
            if let Some(succ_domtree_node) = domtree.get_tree_node(succ) {
                // If this successor is a CFG node, we explicitly want to be scheduled before it and
                // not under it.
                let loc = ctx
                    .domtree()
                    .idom(succ_domtree_node)
                    .expect("dominator tree root was a data successor");
                trace!(
                    "    succ: pinned graph {} (idom CFG {})",
                    succ.as_u32(),
                    loc.as_u32()
                );
                Some(loc)
            } else if graph.node_kind(succ) == &NodeKind::Phi {
                let region = graph.value_def(graph.node_inputs(succ)[0]).0;

                // Note: the corresponding region input might be dead even when the phi is live;
                // this just means that this instance doesn't count as a use and can be skipped.
                let input_idx = input_idx as usize - 1;
                let loc =
                    domtree.get_tree_node(graph.value_def(graph.node_inputs(region)[input_idx]).0);

                if let Some(loc) = loc {
                    trace!(
                        "    succ: phi {} input {} (CFG {})",
                        node.as_u32(),
                        input_idx,
                        loc.as_u32()
                    );
                }

                loc
            } else {
                // Plain data nodes using the value should always have a schedule if they are live.
                // We want to be scheduled in the same CFG node as they are; the fact that we are
                // doing a postorder traversal will ensure that we get appended to the reverse list
                // after them.
                let loc = schedule[succ].expect("data flow cycle");
                trace!("    succ: graph {} (CFG {})", succ.as_u32(), loc.as_u32());
                Some(loc)
            }
        })
        .reduce(|acc, cur| domtree_lca(domtree, depth_map, acc, cur))
        .expect("live non-pinned node has no data uses")
}

#[derive(Clone, Copy, Default)]
struct CfgDepthData {
    domtree_depth: u32,
    loop_depth: u32,
    single_ctrl_output: bool,
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
            single_ctrl_output: has_single_ctrl_output(
                ctx.graph(),
                ctx.domtree().get_cfg_node(domtree_node),
            ),
        };
    }

    depth_map
}

fn has_single_ctrl_output(graph: &ValGraph, node: Node) -> bool {
    let mut ctrl_outputs = graph
        .node_outputs(node)
        .into_iter()
        .filter(|&output| graph.value_kind(output).is_control());
    ctrl_outputs.next().is_some() && ctrl_outputs.next().is_none()
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

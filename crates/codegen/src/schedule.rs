use cranelift_entity::{
    packed_option::PackedOption, EntityList, EntitySet, ListPool, SecondaryMap,
};
use graphwalk::{GraphRef, PostOrderContext};
use ir::{
    loops::LoopForest,
    module::Module,
    node::NodeKind,
    schedule::{schedule_early, schedule_late, ScheduleContext},
    valgraph::{Node, ValGraph},
    valwalk::{dataflow_preds, dataflow_succs, raw_def_use_succs, LiveNodeInfo},
};
use log::trace;

mod display;

pub use display::Display;

use crate::cfg::{Block, BlockCfg, BlockDomTree, CfgContext, FunctionBlockMap};

/// The maximum path length up the dominator tree we are willing to hoist nodes, to avoid quadratic
/// behavior.
const MAX_HOIST_DEPTH: usize = 50;

#[derive(Clone, Copy, Default)]
struct BlockData {
    phis: EntityList<Node>,
    scheduled_nodes_rev: EntityList<Node>,
}

pub struct Schedule {
    block_data: SecondaryMap<Block, BlockData>,
    node_list_pool: ListPool<Node>,
}

impl Schedule {
    pub fn compute(
        graph: &ValGraph,
        valgraph_cfg_preorder: &[Node],
        cfg_ctx: &CfgContext,
        block_map: &FunctionBlockMap,
    ) -> Self {
        let entry = valgraph_cfg_preorder[0];
        let live_node_info = LiveNodeInfo::compute(graph, entry);

        let ctx = ScheduleContext::new(graph, &live_node_info, valgraph_cfg_preorder);
        let depth_map = get_cfg_depth_map(&cfg_ctx.domtree, &cfg_ctx.loop_forest);

        let mut schedule = Self {
            block_data: SecondaryMap::new(),
            node_list_pool: ListPool::new(),
        };

        let mut scheduler = Scheduler {
            cfg_ctx,
            block_map,
            depth_map: &depth_map,
            blocks_by_node: SecondaryMap::new(),
            block_terminators: SecondaryMap::new(),
            schedule: &mut schedule,
        };

        scheduler.pin_nodes(&ctx);

        let mut scratch_postorder = PostOrderContext::new();
        schedule_early(&ctx, &mut scratch_postorder, |ctx, node| {
            scheduler.schedule_early(ctx, node);
        });
        schedule_late(&ctx, &mut scratch_postorder, |ctx, node| {
            scheduler.schedule_late(ctx, node);
        });
        scheduler.schedule_intra_block(graph, &mut scratch_postorder);

        schedule
    }

    pub fn scheduled_nodes_rev(&self, block: Block) -> &[Node] {
        self.block_data[block]
            .scheduled_nodes_rev
            .as_slice(&self.node_list_pool)
    }

    pub fn block_phis(&self, block: Block) -> &[Node] {
        self.block_data[block].phis.as_slice(&self.node_list_pool)
    }

    pub fn display<'a>(
        &'a self,
        module: &'a Module,
        graph: &'a ValGraph,
        cfg: &'a BlockCfg,
        block_order: &'a [Block],
    ) -> Display<'a> {
        Display {
            module,
            graph,
            cfg,
            schedule: self,
            block_order,
        }
    }
}

struct SingleBlockSuccs<'a> {
    graph: &'a ValGraph,
    blocks_by_node: &'a SecondaryMap<Node, PackedOption<Block>>,
    block: Block,
}

impl GraphRef for SingleBlockSuccs<'_> {
    type Node = Node;

    fn successors(&self, node: Node, mut f: impl FnMut(Node)) {
        raw_def_use_succs(self.graph, node)
            .filter(|&(succ, use_idx)| {
                // Take only outputs from the right block, and skip skip any cycle back edges coming
                // from control/phi nodes. We also skip terminators because they are manually placed
                // last in the block.
                self.blocks_by_node[succ].expand() == Some(self.block)
                    && !should_skip_block_edge(self.graph, succ, use_idx)
            })
            .for_each(|(node, _use_idx)| f(node));
    }
}

fn should_skip_block_edge(graph: &ValGraph, succ: Node, use_idx: u32) -> bool {
    match graph.node_kind(succ) {
        // Data inputs to phi nodes from the same block are always back edges.
        NodeKind::Phi => use_idx != 0,
        // Control inputs to the block header region are always back edges.
        NodeKind::Region => true,
        // Otherwise, skip just the block's terminator, because we tack it on manually at the end.
        kind => kind.is_terminator(),
    }
}

struct Scheduler<'a> {
    cfg_ctx: &'a CfgContext,
    block_map: &'a FunctionBlockMap,
    depth_map: &'a CfgDepthMap,
    blocks_by_node: SecondaryMap<Node, PackedOption<Block>>,
    block_terminators: SecondaryMap<Block, PackedOption<Node>>,
    schedule: &'a mut Schedule,
}

impl<'a> Scheduler<'a> {
    fn pin_nodes(&mut self, ctx: &ScheduleContext<'_>) {
        for &cfg_node in ctx.cfg_preorder() {
            let block = self
                .block_map
                .containing_block(cfg_node)
                .expect("live CFG node not in block CFG");

            match ctx.graph().node_kind(cfg_node) {
                terminator_kind if terminator_kind.is_terminator() => {
                    // Track terminators separately so we know to place them last during intra-block
                    // scheduling.
                    trace!("terminator: node {}", cfg_node.as_u32());
                    self.assign_block(block, cfg_node);
                    self.block_terminators[block] = cfg_node.into();
                }
                NodeKind::Region => {
                    // Exclude region nodes from the final schedule, since their purpose was just to
                    // delineate block boundaries.
                    // This won't break any scheduling assumptions made later because regions don't have
                    // any data outputs.

                    for phi in ctx.get_attached_phis(cfg_node) {
                        trace!("phi: node {} -> {block}", phi.as_u32());
                        self.assign_block(block, phi);
                        self.schedule.block_data[block]
                            .phis
                            .push(phi, &mut self.schedule.node_list_pool);
                    }
                }
                _ => {
                    // "Normal" control nodes just get pinned in place.
                    trace!("pinned: node {} -> {block}", cfg_node.as_u32());
                    self.assign_and_append(block, cfg_node);
                }
            }
        }
    }

    fn schedule_intra_block(
        &mut self,
        graph: &ValGraph,
        scratch_postorder: &mut PostOrderContext<Node>,
    ) {
        let mut per_block_visited = EntitySet::new();

        // Get a reverse-topological sort of all nodes placed into each block.
        for block in self.domtree().preorder() {
            let block = self.domtree().get_cfg_node(block);
            let succs = SingleBlockSuccs {
                graph,
                blocks_by_node: &self.blocks_by_node,
                block,
            };

            // At this point, the scheduled node list for each block is partitioned into (almost
            // all) control nodes at the front, followed by data-only nodes at the back. Still
            // missing is the block terminator (if one exists), which will have been recorded in
            // `pin_nodes` but not yet inserted because we want to make sure it comes last.

            let nodes = &mut self.schedule.block_data[block].scheduled_nodes_rev;

            // Note: this postorder will not visit the terminator.
            scratch_postorder.reset(
                nodes
                    .as_slice(&self.schedule.node_list_pool)
                    .iter()
                    .copied(),
            );
            nodes.clear(&mut self.schedule.node_list_pool);

            trace!("sorting {block}");

            // Place the terminator first in the reverse list (if it exists).
            if let Some(terminator) = self.block_terminators[block].expand() {
                trace!("    term {}", terminator.as_u32());
                nodes.push(terminator, &mut self.schedule.node_list_pool);
            }

            // Topologically sort the rest of the nodes.
            while let Some(node) = scratch_postorder.next(&succs, &mut per_block_visited) {
                trace!("    node {}", node.as_u32());
                nodes.push(node, &mut self.schedule.node_list_pool);
            }
        }
    }

    fn schedule_early(&mut self, ctx: &ScheduleContext<'_>, node: Node) {
        assert!(self.blocks_by_node[node].is_none());
        self.assign_block(self.early_schedule_location(ctx, node), node);
    }

    fn schedule_late(&mut self, ctx: &ScheduleContext<'_>, node: Node) {
        let early_loc = self.blocks_by_node[node].expect("node should have been scheduled early");
        let late_loc = self.late_schedule_location(ctx, node);
        let loc = self.best_schedule_location(node, early_loc, late_loc);
        self.assign_and_append(loc, node);
    }

    fn best_schedule_location(&self, node: Node, early_loc: Block, late_loc: Block) -> Block {
        trace!(
            "place: node {} in {early_loc} [domtree depth {}, loop depth {}] .. {late_loc} [domtree depth {}, loop depth {}]",
            node.as_u32(),
            self.depth_map[early_loc].domtree_depth,
            self.depth_map[early_loc].loop_depth,
            self.depth_map[late_loc].domtree_depth,
            self.depth_map[late_loc].loop_depth,
        );

        debug_assert!(self.domtree().cfg_dominates(early_loc, late_loc));

        // Select a node between `early_loc` and `late_loc` on the dominator tree that has minimal
        // loop depth, favoring deeper nodes (those closer to `late_loc`).
        let mut cur_loc = late_loc;
        let mut best_loc = cur_loc;
        let mut i = 0;

        loop {
            if i > MAX_HOIST_DEPTH {
                trace!("place: reached maximum hoist depth {MAX_HOIST_DEPTH}, stopping search");
                break;
            }

            if self.depth_map[cur_loc].loop_depth < self.depth_map[best_loc].loop_depth {
                trace!(
                    "place: hoist to {cur_loc} [domtree depth {}, loop depth {}]",
                    self.depth_map[cur_loc].domtree_depth,
                    self.depth_map[cur_loc].loop_depth
                );
                best_loc = cur_loc;
            }

            if cur_loc == early_loc {
                break;
            }

            cur_loc = self.domtree().cfg_idom(cur_loc).unwrap();
            i += 1;
        }

        debug_assert!(self.domtree().cfg_dominates(early_loc, best_loc));

        trace!("place: node {} -> {best_loc}", node.as_u32());
        best_loc
    }

    fn early_schedule_location(&self, ctx: &ScheduleContext<'_>, node: Node) -> Block {
        trace!("early: node {}", node.as_u32());
        let loc = dataflow_preds(ctx.graph(), node)
            .map(|pred| {
                let pred_loc = self.blocks_by_node[pred].expect("data flow cycle or dead input");
                trace!("    pred: node {} ({pred_loc})", pred.as_u32());
                pred_loc
            })
            .reduce(|acc, cur| {
                // Find the deepest input schedule location in the dominator tree, assuming they are
                // totally ordered by dominance.
                if self.domtree().cfg_dominates(acc, cur) {
                    cur
                } else {
                    acc
                }
            })
            .unwrap_or(self.domtree().get_cfg_node(self.domtree().root()));
        trace!("early: node {} -> {loc}", node.as_u32());
        loc
    }

    fn late_schedule_location(&self, ctx: &ScheduleContext<'_>, node: Node) -> Block {
        let graph = ctx.graph();

        trace!("late: node {}", node.as_u32());

        let loc = dataflow_succs(ctx.graph(), ctx.live_nodes(), node)
            .filter_map(|(succ, input_idx)| {
                if matches!(graph.node_kind(succ), NodeKind::Phi) {
                    // Phi nodes are special, because they "use" their inputs in the corresponding
                    // input blocks and not in the blocks housing them.

                    let region = graph.value_def(graph.node_inputs(succ)[0]).0;

                    // Note: the corresponding region input might be dead even when the phi is live;
                    // this just means that this instance doesn't count as a use and can be skipped.
                    let input_idx = input_idx as usize - 1;
                    let ctrl_input = graph.node_inputs(region)[input_idx];

                    // Find the CFG predecessor corresponding to `ctrl_input`, which might be a
                    // split critical edge. We don't just index into the region's `block_preds`
                    // directly with `input_idx` because `block_preds` will not include any dead
                    // inputs.
                    let loc = self.block_map.cfg_pred_block(graph, ctrl_input);

                    if let Some(loc) = loc {
                        debug_assert!(self
                            .cfg()
                            .block_preds(self.block_map.containing_block(region).unwrap())
                            .contains(&loc));
                        trace!(
                            "    succ: phi {} input {} ({loc})",
                            node.as_u32(),
                            input_idx,
                        );
                    }

                    loc
                } else {
                    // Other nodes using the value should always be scheduled in the same block as
                    // the value computation.
                    let loc = self.blocks_by_node[succ].expect("data flow cycle");
                    trace!("    succ: node {} ({loc})", succ.as_u32());
                    Some(loc)
                }
            })
            .reduce(|acc, cur| domtree_lca(self.domtree(), self.depth_map, acc, cur))
            .expect("live non-pinned node has no data uses");

        trace!("late: node {} -> {loc}", node.as_u32());
        loc
    }

    fn assign_and_append(&mut self, block: Block, node: Node) {
        self.assign_block(block, node);
        self.schedule.block_data[block]
            .scheduled_nodes_rev
            .push(node, &mut self.schedule.node_list_pool);
    }

    fn assign_block(&mut self, block: Block, node: Node) {
        self.blocks_by_node[node] = block.into();
    }

    fn cfg(&self) -> &'a BlockCfg {
        &self.cfg_ctx.cfg
    }

    fn domtree(&self) -> &'a BlockDomTree {
        &self.cfg_ctx.domtree
    }
}

#[derive(Clone, Copy, Default)]
struct CfgDepthData {
    domtree_depth: u32,
    loop_depth: u32,
}

type CfgDepthMap = SecondaryMap<Block, CfgDepthData>;

fn get_cfg_depth_map(domtree: &BlockDomTree, loop_forest: &LoopForest) -> CfgDepthMap {
    let mut depth_map = CfgDepthMap::new();

    for domtree_node in domtree.preorder() {
        // We make use of two facts here:
        // 1. Loop headers dominate all nodes in the loop (including subloops).
        // 2. We are walking down the dominator tree in preorder.
        // This means that whenever we are walking through a loop, at least its parent's header will
        // have been traversed and have correct depth information.
        let cur_loop = loop_forest.containing_loop(domtree_node);
        let loop_depth = match cur_loop {
            Some(cur_loop) => {
                let parent_loop = loop_forest.loop_parent(cur_loop);
                let parent_depth = parent_loop.map_or(0, |parent_loop| {
                    depth_map[domtree.get_cfg_node(loop_forest.loop_header(parent_loop))].loop_depth
                });
                parent_depth + 1
            }
            None => 0,
        };

        let domtree_depth = domtree.idom(domtree_node).map_or(0, |idom| {
            let idom = domtree.get_cfg_node(idom);
            depth_map[idom].domtree_depth + 1
        });

        let block = domtree.get_cfg_node(domtree_node);
        depth_map[block] = CfgDepthData {
            domtree_depth,
            loop_depth,
        };
    }

    depth_map
}

fn domtree_lca(
    domtree: &BlockDomTree,
    depth_map: &CfgDepthMap,
    mut a: Block,
    mut b: Block,
) -> Block {
    while depth_map[a].domtree_depth > depth_map[b].domtree_depth {
        // Note: `a` must have an immediate dominator here because its depth is nonzero.
        a = domtree.cfg_idom(a).unwrap();
    }

    while depth_map[b].domtree_depth > depth_map[a].domtree_depth {
        // Note: `b` must have an immediate dominator here because its depth is nonzero.
        b = domtree.cfg_idom(b).unwrap();
    }

    debug_assert!(depth_map[a].domtree_depth == depth_map[b].domtree_depth);
    while a != b {
        a = domtree.cfg_idom(a).unwrap();
        b = domtree.cfg_idom(b).unwrap();
    }

    a
}

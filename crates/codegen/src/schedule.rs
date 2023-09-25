use cranelift_entity::{
    packed_option::PackedOption, EntityList, EntitySet, ListPool, SecondaryMap,
};
use graphwalk::{GraphRef, PostOrderContext};
use ir::{
    cfg::{Block, BlockCfg, BlockDomTree},
    loops::LoopForest,
    node::NodeKind,
    schedule::{pin_nodes, schedule_early, schedule_late, PinNodes, ScheduleCtx},
    valgraph::{Node, ValGraph},
    valwalk::{dataflow_preds, dataflow_succs, raw_def_use_succs, LiveNodeInfo},
};
use log::trace;

pub struct Schedule {
    blocks_by_node: SecondaryMap<Node, PackedOption<Block>>,
    nodes_by_block: SecondaryMap<Block, EntityList<Node>>,
    block_phi_inputs: SecondaryMap<Block, EntityList<Node>>,
    node_list_pool: ListPool<Node>,
}

impl Schedule {
    pub fn compute(graph: &ValGraph, valgraph_cfg_preorder: &[Node], block_cfg: &BlockCfg) -> Self {
        let entry = valgraph_cfg_preorder[0];
        let live_node_info = LiveNodeInfo::compute(graph, entry);

        let domtree = BlockDomTree::compute(block_cfg, block_cfg.containing_block(entry).unwrap());
        let loop_forest = LoopForest::compute(block_cfg, &domtree);

        let ctx = ScheduleCtx::new(graph, &live_node_info, valgraph_cfg_preorder);
        let depth_map = get_cfg_depth_map(&domtree, &loop_forest);

        let mut schedule = Self {
            blocks_by_node: SecondaryMap::new(),
            nodes_by_block: SecondaryMap::new(),
            block_phi_inputs: SecondaryMap::new(),
            node_list_pool: ListPool::new(),
        };

        let mut scheduler = Scheduler {
            block_cfg,
            domtree: &domtree,
            depth_map: &depth_map,
            schedule: &mut schedule,
        };

        pin_nodes(&ctx, &mut scheduler);

        let mut scratch_postorder = PostOrderContext::new();
        schedule_early(&ctx, &mut scratch_postorder, |ctx, node| {
            scheduler.schedule_early(ctx, node);
        });

        schedule_late(&ctx, &mut scratch_postorder, |ctx, node| {
            scheduler.schedule_late(ctx, node);
        });

        schedule.schedule_intra_block(graph, &domtree, &mut scratch_postorder);
        schedule
    }

    pub fn scheduled_nodes_rev(&self, block: Block) -> &[Node] {
        self.nodes_by_block[block].as_slice(&self.node_list_pool)
    }

    pub fn block_phi_inputs(&self, block: Block) -> &[Node] {
        self.block_phi_inputs[block].as_slice(&self.node_list_pool)
    }

    fn schedule_intra_block(
        &mut self,
        graph: &ValGraph,
        domtree: &BlockDomTree,
        scratch_postorder: &mut PostOrderContext<Node>,
    ) {
        let mut per_block_visited = EntitySet::new();

        // Get a reverse-topological sort of all nodes placed into each block.
        for block in domtree.preorder() {
            let block = domtree.get_cfg_node(block);
            let succs = SingleBlockSuccs {
                graph,
                blocks_by_node: &self.blocks_by_node,
                block,
            };

            let nodes = &mut self.nodes_by_block[block];
            scratch_postorder.reset(nodes.as_slice(&self.node_list_pool).iter().copied());
            nodes.clear(&mut self.node_list_pool);

            trace!("sorting {block}");
            while let Some(node) = scratch_postorder.next(&succs, &mut per_block_visited) {
                if matches!(graph.node_kind(node), NodeKind::Phi) {
                    // We handle phi nodes specially.
                    continue;
                }
                trace!("    node {}", node.as_u32());
                nodes.push(node, &mut self.node_list_pool);
            }
        }
    }

    fn push_block_node(&mut self, block: Block, node: Node) {
        self.nodes_by_block[block].push(node, &mut self.node_list_pool);
    }

    fn push_block_phi(&mut self, block: Block, phi: Node) {
        self.block_phi_inputs[block].push(phi, &mut self.node_list_pool);
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
                // from control/phi nodes.
                self.blocks_by_node[succ].expand() == Some(self.block)
                    && !is_block_backedge(self.graph, succ, use_idx)
            })
            .for_each(|(node, _use_idx)| f(node));
    }
}

fn is_block_backedge(graph: &ValGraph, succ: Node, use_idx: u32) -> bool {
    match graph.node_kind(succ) {
        // Data inputs to phi nodes from the same block are always back edges.
        NodeKind::Phi => use_idx != 0,
        // Control inputs to the block header region are always back edges.
        NodeKind::Region => true,
        _ => false,
    }
}

struct Scheduler<'a> {
    block_cfg: &'a BlockCfg,
    domtree: &'a BlockDomTree,
    depth_map: &'a CfgDepthMap,
    schedule: &'a mut Schedule,
}

impl PinNodes for Scheduler<'_> {
    type Block = Block;

    fn control_node_block(&self, node: Node) -> Block {
        self.block_cfg
            .containing_block(node)
            .expect("live CFG node not in block CFG")
    }

    fn pin(&mut self, ctx: &ScheduleCtx<'_>, node: Node, block: Block) {
        assert!(self.schedule.blocks_by_node[node].is_none());
        self.schedule.blocks_by_node[node] = block.into();

        if matches!(ctx.graph().node_kind(node), NodeKind::Phi) {
            trace!("phi: node {} -> {block}", node.as_u32());
            self.schedule.push_block_phi(block, node);
        } else {
            trace!("pinned: node {} -> {block}", node.as_u32());
            self.schedule.push_block_node(block, node);
        }
    }
}

impl Scheduler<'_> {
    fn schedule_early(&mut self, ctx: &ScheduleCtx<'_>, node: Node) {
        assert!(self.schedule.blocks_by_node[node].is_none());
        self.schedule.blocks_by_node[node] = self.early_schedule_location(ctx, node).into();
    }

    fn schedule_late(&mut self, ctx: &ScheduleCtx<'_>, node: Node) {
        let early_loc =
            self.schedule.blocks_by_node[node].expect("node should have been scheduled early");
        let late_loc = self.late_schedule_location(ctx, node);
        let loc = self.best_schedule_location(node, early_loc, late_loc);
        self.schedule.blocks_by_node[node] = loc.into();
        self.schedule.push_block_node(loc, node);
    }

    fn best_schedule_location(&self, node: Node, early_loc: Block, mut late_loc: Block) -> Block {
        trace!(
            "place: node {} in {early_loc} [domtree depth {}, loop depth {}] .. {late_loc} [domtree depth {}, loop depth {}]",
            node.as_u32(),
            self.depth_map[early_loc].domtree_depth,
            self.depth_map[early_loc].loop_depth,
            self.depth_map[late_loc].domtree_depth,
            self.depth_map[late_loc].loop_depth,
        );

        debug_assert!(self.domtree.cfg_dominates(early_loc, late_loc));

        // Select a node between `early_loc` and `late_loc` on the dominator
        // tree that has minimal loop depth, favoring deeper nodes (those closer to `late_loc`).
        let mut best_loc = late_loc;
        while late_loc != early_loc {
            if self.depth_map[late_loc].loop_depth < self.depth_map[best_loc].loop_depth {
                trace!(
                    "place: hoist to {late_loc} [domtree depth {}, loop depth {}]",
                    self.depth_map[late_loc].domtree_depth,
                    self.depth_map[late_loc].loop_depth
                );
                best_loc = late_loc;
            }
            late_loc = self.domtree.cfg_idom(late_loc).unwrap();
        }

        debug_assert!(self.domtree.cfg_dominates(early_loc, best_loc));

        trace!("place: node {} -> {best_loc}", node.as_u32());
        best_loc
    }

    fn early_schedule_location(&self, ctx: &ScheduleCtx<'_>, node: Node) -> Block {
        trace!("early: node {}", node.as_u32());
        let loc = dataflow_preds(ctx.graph(), node)
            .map(|pred| {
                let pred_loc =
                    self.schedule.blocks_by_node[pred].expect("data flow cycle or dead input");
                trace!("    pred: node {} ({pred_loc})", pred.as_u32());
                pred_loc
            })
            .reduce(|acc, cur| {
                // Find the deepest input schedule location in the dominator tree, assuming they are
                // totally ordered by dominance.
                if self.domtree.cfg_dominates(acc, cur) {
                    cur
                } else {
                    acc
                }
            })
            .unwrap_or(self.domtree.get_cfg_node(self.domtree.root()));
        trace!("early: node {} -> {loc}", node.as_u32());
        loc
    }

    fn late_schedule_location(&self, ctx: &ScheduleCtx<'_>, node: Node) -> Block {
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
                    let loc = self
                        .block_cfg
                        .containing_block(graph.value_def(graph.node_inputs(region)[input_idx]).0);

                    if let Some(loc) = loc {
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
                    let loc = self.schedule.blocks_by_node[succ].expect("data flow cycle");
                    trace!("    succ: node {} ({loc})", succ.as_u32());
                    Some(loc)
                }
            })
            .reduce(|acc, cur| domtree_lca(self.domtree, self.depth_map, acc, cur))
            .expect("live non-pinned node has no data uses");

        trace!("late: node {} -> {loc}", node.as_u32());
        loc
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

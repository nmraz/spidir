use core::{cmp::Ordering, ops::ControlFlow};

use alloc::{collections::BinaryHeap, vec::Vec};

use cranelift_entity::{
    EntityList, ListPool, SecondaryMap,
    packed_option::{PackedOption, ReservedValue},
};
use dominators::depth_map::DepthMap;
use entity_set::DenseEntitySet;
use fx_utils::{FxHashMap, FxHashSet};
use graphwalk::{GraphRef, PostOrderContext};
use ir::{
    function::FunctionBody,
    module::Module,
    node::NodeKind,
    schedule::{ScheduleContext, schedule_early, schedule_late},
    valgraph::{DepValue, Node, ValGraph},
    valwalk::{
        CfgPreorderInfo, GraphWalkInfo, dataflow_inputs, dataflow_preds, dataflow_succs,
        def_use_preds, raw_dataflow_succs, raw_def_use_succs,
    },
};
use log::trace;

mod display;

pub use display::Display;
use smallvec::SmallVec;

use crate::cfg::{Block, BlockCfg, BlockDomTree, CfgContext, FunctionBlockMap};

/// The maximum path length up the dominator tree we are willing to hoist nodes, to avoid quadratic
/// behavior.
const MAX_HOIST_DEPTH: usize = 50;

#[derive(Clone, Copy, Default)]
struct BlockData {
    phis: EntityList<Node>,
    scheduled_nodes: EntityList<Node>,
}

pub struct Schedule {
    block_data: SecondaryMap<Block, BlockData>,
    node_list_pool: ListPool<Node>,
}

impl Schedule {
    pub fn compute(
        graph: &ValGraph,
        valgraph_cfg_preorder: &CfgPreorderInfo,
        cfg_ctx: &CfgContext,
        block_map: &FunctionBlockMap,
    ) -> Self {
        let entry = valgraph_cfg_preorder.preorder[0];
        let walk_info = GraphWalkInfo::compute_full(graph, entry);

        let ctx = ScheduleContext::new(graph, &walk_info, &valgraph_cfg_preorder.preorder);

        let mut schedule = Self {
            block_data: SecondaryMap::new(),
            node_list_pool: ListPool::new(),
        };

        let mut scheduler = Scheduler {
            cfg_ctx,
            block_map,
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

    pub fn scheduled_nodes(&self, block: Block) -> &[Node] {
        self.block_data[block]
            .scheduled_nodes
            .as_slice(&self.node_list_pool)
    }

    pub fn block_phis(&self, block: Block) -> &[Node] {
        self.block_data[block].phis.as_slice(&self.node_list_pool)
    }

    pub fn display<'a>(
        &'a self,
        module: &'a Module,
        body: &'a FunctionBody,
        cfg: &'a BlockCfg,
        block_order: &'a [Block],
    ) -> Display<'a> {
        Display {
            module,
            body,
            cfg,
            schedule: self,
            block_order,
        }
    }
}

type BlocksByNode = SecondaryMap<Node, PackedOption<Block>>;

struct Scheduler<'a> {
    cfg_ctx: &'a CfgContext,
    block_map: &'a FunctionBlockMap,
    blocks_by_node: BlocksByNode,
    block_terminators: SecondaryMap<Block, PackedOption<Node>>,
    schedule: &'a mut Schedule,
}

impl<'a> Scheduler<'a> {
    fn pin_nodes(&mut self, ctx: &ScheduleContext<'_>) {
        for &cfg_node in ctx.cfg_preorder {
            let block = self
                .block_map
                .containing_block(cfg_node)
                .expect("live CFG node not in block CFG");

            match ctx.graph.node_kind(cfg_node) {
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
        let mut block_scheduler =
            BlockScheduler::new(graph, &self.blocks_by_node, scratch_postorder);

        for &block in &self.cfg_ctx.block_order {
            trace!("scheduling {block}");

            let nodes = &mut self.schedule.block_data[block].scheduled_nodes;
            let orig_len = nodes.len(&self.schedule.node_list_pool);

            // Set up our local scheduler for this block.
            block_scheduler.reset(block, nodes.as_slice(&self.schedule.node_list_pool));

            // Schedule all interior nodes in the block.
            nodes.clear(&mut self.schedule.node_list_pool);
            block_scheduler.schedule(|node| {
                nodes.push(node, &mut self.schedule.node_list_pool);
            });

            debug_assert_eq!(nodes.len(&self.schedule.node_list_pool), orig_len);

            // Place the terminator last (if it exists).
            if let Some(terminator) = self.block_terminators[block].expand() {
                trace!("    term: node {}", terminator.as_u32());
                nodes.push(terminator, &mut self.schedule.node_list_pool);
            }
        }
    }

    fn schedule_early(&mut self, ctx: &ScheduleContext<'_>, node: Node) {
        assert!(self.blocks_by_node[node].is_none());
        self.assign_block(self.early_schedule_location(ctx, node), node);
    }

    fn schedule_late(&mut self, ctx: &ScheduleContext<'_>, node: Node) {
        let early_loc = self.blocks_by_node[node].expect("node should have been scheduled early");

        // We might not have a late schedule location at all if all uses are dead phi inputs - just
        // don't schedule in that case.
        if let Some(late_loc) = self.late_schedule_location(ctx, node) {
            let loc = self.best_schedule_location(node, early_loc, late_loc);
            self.assign_and_append(loc, node);
        }
    }

    fn best_schedule_location(&self, node: Node, early_loc: Block, late_loc: Block) -> Block {
        let domtree = self.domtree();
        let depth_map = self.depth_map();

        let early_tree_loc = domtree.get_tree_node(early_loc).unwrap();
        let late_tree_loc = domtree.get_tree_node(late_loc).unwrap();
        trace!(
            "place: node {} in {early_loc} [domtree depth {}, loop depth {}] .. {late_loc} [domtree depth {}, loop depth {}]",
            node.as_u32(),
            depth_map.domtree_depth(early_tree_loc),
            depth_map.loop_depth(early_tree_loc),
            depth_map.domtree_depth(late_tree_loc),
            depth_map.loop_depth(late_tree_loc),
        );

        debug_assert!(domtree.dominates(early_tree_loc, late_tree_loc));

        // Select a node between `early_loc` and `late_loc` on the dominator tree that has minimal
        // loop depth, favoring deeper nodes (those closer to `late_loc`).
        let mut cur_tree_loc = late_tree_loc;
        let mut best_tree_loc = cur_tree_loc;
        let mut i = 0;

        while cur_tree_loc != early_tree_loc {
            cur_tree_loc = domtree.idom(cur_tree_loc).unwrap();
            i += 1;

            if i > MAX_HOIST_DEPTH {
                trace!("place: reached maximum hoist depth {MAX_HOIST_DEPTH}, stopping search");
                break;
            }

            if depth_map.loop_depth(cur_tree_loc) < depth_map.loop_depth(best_tree_loc) {
                trace!(
                    "place: hoist to {} [domtree depth {}, loop depth {}]",
                    domtree.get_cfg_node(cur_tree_loc),
                    depth_map.domtree_depth(cur_tree_loc),
                    depth_map.loop_depth(cur_tree_loc),
                );
                best_tree_loc = cur_tree_loc;
            }
        }

        debug_assert!(domtree.dominates(early_tree_loc, best_tree_loc));

        let best_loc = domtree.get_cfg_node(best_tree_loc);
        trace!("place: node {} -> {best_loc}", node.as_u32());
        best_loc
    }

    fn early_schedule_location(&self, ctx: &ScheduleContext<'_>, node: Node) -> Block {
        trace!("early: node {}", node.as_u32());
        let loc = dataflow_preds(ctx.graph, node)
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

    fn late_schedule_location(&self, ctx: &ScheduleContext<'_>, node: Node) -> Option<Block> {
        let graph = ctx.graph;

        trace!("late: node {}", node.as_u32());

        let loc = dataflow_succs(ctx.graph, ctx.live_nodes(), node)
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
                        debug_assert!(
                            self.cfg()
                                .block_preds(self.block_map.containing_block(region).unwrap())
                                .contains(&loc)
                        );
                        trace!(
                            "    succ: phi {} input {} ({loc})",
                            succ.as_u32(),
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
            // The node might not have any live uses if all uses are dead phi inputs.
            .reduce(|acc, cur| self.domtree_lca(acc, cur))?;

        trace!("late: node {} -> {loc}", node.as_u32());
        Some(loc)
    }

    fn assign_and_append(&mut self, block: Block, node: Node) {
        self.assign_block(block, node);
        self.schedule.block_data[block]
            .scheduled_nodes
            .push(node, &mut self.schedule.node_list_pool);
    }

    fn assign_block(&mut self, block: Block, node: Node) {
        self.blocks_by_node[node] = block.into();
    }

    fn domtree_lca(&self, a: Block, b: Block) -> Block {
        let domtree = self.domtree();
        domtree.get_cfg_node(self.depth_map().domtree_lca(
            domtree,
            domtree.get_tree_node(a).unwrap(),
            domtree.get_tree_node(b).unwrap(),
        ))
    }

    fn cfg(&self) -> &'a BlockCfg {
        &self.cfg_ctx.cfg
    }

    fn domtree(&self) -> &'a BlockDomTree {
        &self.cfg_ctx.domtree
    }

    fn depth_map(&self) -> &'a DepthMap {
        &self.cfg_ctx.depth_map
    }
}

#[derive(Clone, Copy, Eq)]
struct ReadyNode {
    node: Node,
    prio: u64,
}

impl PartialOrd for ReadyNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ReadyNode {
    fn cmp(&self, other: &Self) -> Ordering {
        self.prio.cmp(&other.prio)
    }
}

impl PartialEq for ReadyNode {
    fn eq(&self, other: &Self) -> bool {
        self.prio == other.prio
    }
}

/// Holds auxiliary data for block-local scheduling of nodes.
struct BlockNodeData {
    /// The number of block-local predecessors of this node that have not yet been scheduled.
    unscheduled_preds: u32,
    /// The length of the longest data dependency chain in the block headed by this node.
    cp_length: u32,
    /// The number of deduplicated data inputs for which this node is the last use point in the
    /// block.
    last_use_count: u32,
    /// A deduplicated list of all data inputs to this node.
    unique_inputs: EntityList<DepValue>,
}

#[derive(Default)]
struct BlockValueData {
    users: EntityList<Node>,
    outstanding_uses: u32,
}

struct BlockScheduler<'a> {
    graph: &'a ValGraph,
    blocks_by_node: &'a BlocksByNode,
    scratch_postorder: &'a mut PostOrderContext<Node>,
    scheduled: DenseEntitySet<Node>,
    block: Block,
    block_postorder: Vec<Node>,
    block_node_data: FxHashMap<Node, BlockNodeData>,
    block_value_data: FxHashMap<DepValue, BlockValueData>,
    unique_input_pool: ListPool<DepValue>,
    value_use_pool: ListPool<Node>,
    ready_nodes: BinaryHeap<ReadyNode>,
}

impl<'a> BlockScheduler<'a> {
    fn new(
        graph: &'a ValGraph,
        blocks_by_node: &'a BlocksByNode,
        scratch_postorder: &'a mut PostOrderContext<Node>,
    ) -> Self {
        Self {
            graph,
            blocks_by_node,
            scratch_postorder,
            scheduled: DenseEntitySet::new(),
            block: Block::reserved_value(),
            block_postorder: Vec::new(),
            block_node_data: FxHashMap::default(),
            block_value_data: FxHashMap::default(),
            unique_input_pool: ListPool::new(),
            value_use_pool: ListPool::new(),
            ready_nodes: BinaryHeap::new(),
        }
    }

    fn reset(&mut self, block: Block, nodes: &[Node]) {
        self.block = block;
        self.compute_postorder(nodes);
        self.prepare_node_data();
    }

    fn schedule(&mut self, mut f: impl FnMut(Node)) {
        self.scheduled.clear();

        let mut updated_last_users = SmallVec::<[Node; 4]>::new();
        while let Some(ready_node) = self.ready_nodes.pop() {
            let node = ready_node.node;
            if self.scheduled.contains(node) {
                // This can happen if we have re-enqueued a node because its last-use-count has
                // increased: we'll see the previous entries with the lower last-use-counts after
                // the node has already been placed.
                continue;
            }

            trace!(
                "    place: node {} [luc {}, cp {}]",
                node.as_u32(),
                self.block_node_data[&node].last_use_count,
                self.block_node_data[&node].cp_length,
            );

            self.scheduled.insert(node);
            f(node);

            updated_last_users.clear();
            self.gather_updated_last_users(node, &mut updated_last_users);
            for node in updated_last_users.drain(..) {
                self.enqueue_if_ready(node);
            }

            self.enqeue_ready_succs(node);
        }
    }

    fn enqeue_ready_succs(&mut self, node: Node) {
        for (succ, _use_idx) in raw_def_use_succs(self.graph, node) {
            if !self.is_block_interior_node(succ) {
                continue;
            }

            let succ_data = self.block_node_data.get_mut(&succ).unwrap();
            debug_assert!(succ_data.unscheduled_preds > 0);
            debug_assert!(!self.scheduled.contains(succ));
            succ_data.unscheduled_preds -= 1;
            self.enqueue_if_ready(succ);
        }
    }

    fn gather_updated_last_users(
        &mut self,
        node: Node,
        updated_last_users: &mut SmallVec<[Node; 4]>,
    ) {
        for &input in self.block_node_data[&node]
            .unique_inputs
            .as_slice(&self.unique_input_pool)
        {
            // This input should always have an existing outstanding use count.
            let value_data = self.block_value_data.get_mut(&input).unwrap();
            debug_assert!(value_data.outstanding_uses > 0);
            value_data.outstanding_uses -= 1;
            if value_data.outstanding_uses == 1 {
                // The remaining user of this value is now the last in the block, so bump its
                // last use count and report it to the caller.

                // This search happens at most once per value during scheduling of the block (and
                // only walks edges leading into the block), so the total complexity here is still
                // linear.
                let last_user = *value_data
                    .users
                    .as_slice(&self.value_use_pool)
                    .iter()
                    .find(|&&user| !self.scheduled.contains(user))
                    .expect("value should have outstanding user");

                // Don't even bother with nodes that have already been scheduled. This can
                // happen if `last_user` actually uses a value defined outside the block.
                if !self.scheduled.contains(last_user) {
                    self.block_node_data
                        .get_mut(&last_user)
                        .unwrap()
                        .last_use_count += 1;
                    // Note: this node may be pushed multiple times if several of its inputs are
                    // last users.
                    updated_last_users.push(last_user);
                }
            }
        }
    }

    fn compute_postorder(&mut self, nodes: &[Node]) {
        self.scratch_postorder.reset(nodes.iter().copied());

        // Temporarily use `scheduled` here to track which nodes have already been visited by the
        // postorder.
        self.scheduled.clear();

        let block_graph = self.block_graph();

        self.block_postorder.clear();
        while let Some(node) = self
            .scratch_postorder
            .next(&block_graph, &mut self.scheduled)
        {
            self.block_postorder.push(node);
        }
    }

    fn prepare_node_data(&mut self) {
        self.block_node_data.clear();
        self.unique_input_pool.clear();
        self.block_value_data.clear();

        let mut recorded_inputs = FxHashSet::default();

        // Walk the block in postorder here so we have critical path lengths for successors before
        // reaching the nodes themselves.
        for i in 0..self.block_postorder.len() {
            let node = self.block_postorder[i];

            let cp_length = self.compute_node_cp_length(node);
            let unscheduled_preds = self.count_unscheduled_preds(node);

            self.block_node_data.insert(
                node,
                BlockNodeData {
                    unscheduled_preds,
                    cp_length,
                    last_use_count: 0,
                    unique_inputs: EntityList::new(),
                },
            );
            let node_data = self.block_node_data.get_mut(&node).unwrap();

            // Record this node's deduplicated input list.
            recorded_inputs.clear();
            for input in dataflow_inputs(self.graph, node) {
                if recorded_inputs.contains(&input) {
                    continue;
                }
                recorded_inputs.insert(input);
                node_data
                    .unique_inputs
                    .push(input, &mut self.unique_input_pool);
            }

            // Record outstanding uses for all incoming values.
            for &input in node_data.unique_inputs.as_slice(&self.unique_input_pool) {
                let value_data = self.block_value_data.entry(input).or_default();
                value_data.users.push(node, &mut self.value_use_pool);
                value_data.outstanding_uses += 1;
            }
        }

        // Now that everything has been set up and outstanding uses have been counted, bump last use
        // counts and enqueue nodes.
        for i in 0..self.block_postorder.len() {
            let node = self.block_postorder[i];
            let node_data = self.block_node_data.get_mut(&node).unwrap();
            for &input in node_data.unique_inputs.as_slice(&self.unique_input_pool) {
                if self.block_value_data[&input].outstanding_uses == 1 {
                    // This node is the only use of the input in the block, bump its last use count
                    // now.
                    node_data.last_use_count += 1;
                }
            }
            self.enqueue_if_ready(node);
        }
    }

    fn enqueue_if_ready(&mut self, node: Node) {
        if self.block_node_data[&node].unscheduled_preds == 0 {
            trace!("    ready: node {}", node.as_u32());
            self.ready_nodes.push(ReadyNode {
                node,
                prio: self.node_prio(node),
            })
        }
    }

    fn count_unscheduled_preds(&self, node: Node) -> u32 {
        let preds =
            def_use_preds(self.graph, node).filter(|&pred| self.is_block_interior_node(pred));
        preds.count() as u32
    }

    fn compute_node_cp_length(&self, node: Node) -> u32 {
        let block_dataflow_succs = raw_dataflow_succs(self.graph, node)
            .map(|(succ, _use_idx)| succ)
            .filter(|&succ| self.is_block_interior_node(succ));

        let exit_cp_length = block_dataflow_succs
            .map(|succ| {
                // Note: we expect node data to have been computed for all successors in the block.
                self.block_node_data[&succ].cp_length
            })
            .max();

        // If we have any users in the block, we lengthen the critical path by 1. Otherwise, all
        // paths exiting the node have length 0.
        exit_cp_length.map_or(0, |exit_cp_length| exit_cp_length + 1)
    }

    fn node_prio(&self, node: Node) -> u64 {
        let node_data = &self.block_node_data[&node];
        // Prefer last use count, and fall back to CP length when tied.
        ((node_data.last_use_count as u64) << 32) | node_data.cp_length as u64
    }

    fn is_block_interior_node(&self, node: Node) -> bool {
        is_block_interior_node(self.graph, self.blocks_by_node, self.block, node)
    }

    fn block_graph(&self) -> BlockSubgraph<'a> {
        BlockSubgraph {
            graph: self.graph,
            blocks_by_node: self.blocks_by_node,
            block: self.block,
        }
    }
}

struct BlockSubgraph<'a> {
    graph: &'a ValGraph,
    blocks_by_node: &'a BlocksByNode,
    block: Block,
}

impl GraphRef for BlockSubgraph<'_> {
    type Node = Node;

    fn try_successors(
        &self,
        node: Node,
        mut f: impl FnMut(Node) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        raw_def_use_succs(self.graph, node)
            .filter(|&(succ, _use_idx)| {
                is_block_interior_node(self.graph, self.blocks_by_node, self.block, succ)
            })
            .try_for_each(|(node, _use_idx)| f(node))
    }
}

fn is_block_interior_node(
    graph: &ValGraph,
    blocks_by_node: &BlocksByNode,
    block: Block,
    node: Node,
) -> bool {
    blocks_by_node[node].expand() == Some(block)
        && is_block_interior_node_kind(graph.node_kind(node))
}

fn is_block_interior_node_kind(kind: &NodeKind) -> bool {
    !kind.is_terminator() && !matches!(kind, NodeKind::Phi | NodeKind::Region)
}

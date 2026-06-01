use core::cmp::Ordering;

use alloc::collections::BinaryHeap;

use cranelift_entity::{
    EntityList, ListPool, SecondaryMap,
    packed_option::{PackedOption, ReservedValue},
};
use entity_utils::{list::dedup_entity_list, set::DenseEntitySet};
use fx_utils::FxHashMap;
use graphwalk::dfs::PostOrderContext;
use hashbrown::hash_map::Entry;
use ir::{
    function::FunctionBody,
    module::ModuleMetadata,
    node::NodeKind,
    schedule::{ScheduleContext, schedule_early, schedule_late},
    valgraph::{DepValue, Node, ValGraph},
    valwalk::{
        CfgPreorderInfo, DefUseSuccs, GraphWalkInfo, dataflow_inputs, dataflow_outputs,
        dataflow_preds, dataflow_succs, def_use_preds, raw_def_use_succs,
    },
};
use log::trace;

mod display;

pub use display::Display;
use smallvec::SmallVec;

use crate::cfg::{
    Block, BlockCfg, BlockDepthMap, BlockDomTree, BlockPostDomTree, CfgContext, FunctionBlockMap,
};

pub struct Schedule {
    block_data: SecondaryMap<Block, BlockScheduleData>,
    node_list_pool: ListPool<Node>,
}

impl Schedule {
    pub fn compute(
        graph: &ValGraph,
        valgraph_cfg_preorder: &CfgPreorderInfo,
        cfg_ctx: &CfgContext,
        block_map: &FunctionBlockMap,
    ) -> Self {
        let walk_info = GraphWalkInfo::compute_cfg_live(graph, valgraph_cfg_preorder);
        let ctx = ScheduleContext::new(graph, &walk_info, &valgraph_cfg_preorder.preorder);

        let mut schedule = Self {
            block_data: SecondaryMap::new(),
            node_list_pool: ListPool::new(),
        };

        let mut scheduler = Scheduler {
            cfg_ctx,
            block_map,
            node_data: SecondaryMap::new(),
            block_data: SecondaryMap::new(),
            tethered_values: DenseEntitySet::new(),
            indvar_update_nodes: DenseEntitySet::new(),
            schedule: &mut schedule,
        };

        let mut scratch_postorder = PostOrderContext::new();

        scheduler.prepare_node_data(&ctx, &mut scratch_postorder);

        scheduler.pin_nodes(&ctx);

        schedule_early(&ctx, &mut scratch_postorder, |ctx, node| {
            scheduler.schedule_early(ctx, node);
        });
        schedule_late(&ctx, &mut scratch_postorder, |ctx, node| {
            scheduler.schedule_late(ctx, node);
        });
        scheduler.schedule_intra_block(graph);

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
        module_metadata: &'a ModuleMetadata,
        body: &'a FunctionBody,
        cfg: &'a BlockCfg,
        block_order: &'a [Block],
    ) -> Display<'a> {
        Display {
            module_metadata,
            body,
            cfg,
            schedule: self,
            block_order,
        }
    }
}

#[derive(Clone, Copy, Default)]
struct BlockScheduleData {
    phis: EntityList<Node>,
    scheduled_nodes: EntityList<Node>,
}

#[derive(Clone, Copy, Default)]
struct BlockSchedulerData {
    max_cp_length: u32,
    terminator: PackedOption<Node>,
}

#[derive(Clone, Copy, Default)]
struct NodeSchedulerData {
    cp_length: u32,
    block: PackedOption<Block>,
}

struct Scheduler<'a> {
    cfg_ctx: &'a CfgContext,
    block_map: &'a FunctionBlockMap,
    node_data: SecondaryMap<Node, NodeSchedulerData>,
    block_data: SecondaryMap<Block, BlockSchedulerData>,
    tethered_values: DenseEntitySet<DepValue>,
    indvar_update_nodes: DenseEntitySet<Node>,
    schedule: &'a mut Schedule,
}

/// The maximum path length up the dominator tree we are willing to hoist nodes, to avoid quadratic
/// behavior.
const MAX_HOIST_DEPTH: u32 = 50;

/// The maximum number of levels down the dominator tree we're willing to sink operations guaranteed
/// to have a net-negative live range delta.
const MAX_LRG_TERMINATOR_SINK_DEPTH: u32 = 3;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RegPressureHoistMode {
    None,
    Always,
    ControlDependent,
}

impl<'a> Scheduler<'a> {
    fn prepare_node_data(
        &mut self,
        ctx: &ScheduleContext<'_>,
        scratch_postorder: &mut PostOrderContext<Node>,
    ) {
        scratch_postorder.reset(ctx.walk_info.roots.iter().copied());

        let mut visited = DenseEntitySet::new();
        let graph = DefUseSuccs::new(ctx.graph, ctx.live_nodes());

        // Walk nodes in postorder so we can compute critical path lengths. As we visit each node,
        // we also determine whether it is a loop induction variable update.

        while let Some(node) = scratch_postorder.next(graph, &mut visited) {
            let cp_length = dataflow_succs(ctx.graph, ctx.live_nodes(), node)
                // Note: this value will already have been computed for all nodes dominated by this
                // one because we are walking in postorder. For dataflow cycles involving phi nodes,
                // unvisited successors will be ignored because the CP lengths default to 0.
                .map(|(succ, _)| self.node_data[succ].cp_length)
                .max()
                // If we have any users, we lengthen the critical path by 1. Otherwise, all paths
                // exiting the node have length 0.
                .map_or(0, |succ_cp_length| succ_cp_length + 1);
            self.node_data[node].cp_length = cp_length;

            if self.is_loop_indvar_update(ctx, node) {
                self.indvar_update_nodes.insert(node);
            }
        }
    }

    fn pin_nodes(&mut self, ctx: &ScheduleContext<'_>) {
        for &cfg_node in ctx.cfg_preorder {
            self.mark_node_tethered(ctx.graph, cfg_node);

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
                    self.block_data[block].terminator = cfg_node.into();
                }
                NodeKind::Region => {
                    // Exclude region nodes from the final schedule, since their purpose was just to
                    // delineate block boundaries.
                    // This won't break any scheduling assumptions made later because regions don't have
                    // any data outputs.

                    for phi in ctx.get_attached_phis(cfg_node) {
                        trace!("phi: node {} -> {block}", phi.as_u32());
                        self.mark_node_tethered(ctx.graph, phi);
                        self.assign_block(block, phi);
                        self.bump_max_cp_length(block, phi);
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

    fn schedule_intra_block(&mut self, graph: &ValGraph) {
        let mut block_scheduler =
            BlockScheduler::new(graph, &self.node_data, &self.indvar_update_nodes);

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
            if let Some(terminator) = self.block_data[block].terminator.expand() {
                trace!("    term: node {}", terminator.as_u32());
                nodes.push(terminator, &mut self.schedule.node_list_pool);
            }
        }
    }

    fn schedule_early(&mut self, ctx: &ScheduleContext<'_>, node: Node) {
        assert!(self.node_data[node].block.is_none());

        // Compute whether this node is tethered now that we know about all its predecessors.
        if dataflow_inputs(ctx.graph, node).any(|input| self.tethered_values.contains(input)) {
            self.mark_node_tethered(ctx.graph, node);
        }

        self.assign_block(self.early_schedule_location(ctx, node), node);
    }

    fn schedule_late(&mut self, ctx: &ScheduleContext<'_>, node: Node) {
        let early_loc = self.node_data[node]
            .block
            .expect("node should have been scheduled early");

        // We might not have a late schedule location at all if all uses are dead phi inputs - just
        // don't schedule in that case.
        if let Some(late_loc) = self.late_schedule_location(ctx, node, early_loc) {
            let loc = self.best_schedule_location(ctx.graph, node, early_loc, late_loc);
            trace!("place: node {} -> {loc}", node.as_u32());
            self.assign_and_append(loc, node);
        }
    }

    fn best_schedule_location(
        &self,
        graph: &ValGraph,
        node: Node,
        early_loc: Block,
        late_loc: Block,
    ) -> Block {
        // Avoid the heavy work below if the node is "locked" into a single block anyway.
        if early_loc == late_loc {
            return early_loc;
        }

        let domtree = self.domtree();

        debug_assert!(domtree.cfg_dominates(early_loc, late_loc));

        trace!("place: node {} in {early_loc}..{late_loc}", node.as_u32());

        let hoist_mode = self.reg_pressure_hoist_mode(graph, node, early_loc, late_loc);

        // Select a node between `early_loc` and `late_loc` on the dominator tree that has minimal
        // loop depth, favoring deeper nodes (those closer to `late_loc`).
        let mut cur_loc = late_loc;
        let mut best_loc = cur_loc;
        let mut i = 0;

        while cur_loc != early_loc {
            i += 1;
            if i > MAX_HOIST_DEPTH {
                trace!("    place: reached maximum hoist depth {MAX_HOIST_DEPTH}, stopping search");
                break;
            }

            cur_loc = domtree.cfg_idom(cur_loc).unwrap();

            if self.is_better_schedule_location(node, hoist_mode, best_loc, cur_loc) {
                trace!("    place: hoist to {cur_loc}");
                best_loc = cur_loc;
            }
        }

        debug_assert!(domtree.cfg_dominates(early_loc, best_loc));

        best_loc
    }

    fn is_better_schedule_location(
        &self,
        node: Node,
        hoist_mode: RegPressureHoistMode,
        best_loc: Block,
        cand_loc: Block,
    ) -> bool {
        let domtree = self.domtree();
        let postdomtree = self.postdomtree();
        let depth_map = self.depth_map();

        let best_tree_loc = domtree.get_tree_node(best_loc).unwrap();
        let cand_tree_loc = domtree.get_tree_node(cand_loc).unwrap();

        let cand_loop_depth = depth_map.loop_depth(cand_tree_loc);
        let best_loop_depth = depth_map.loop_depth(best_tree_loc);

        // Loop depth is our strongest signal and overrides everything else. This includes the
        // control dependence check below, because we want LICM to be willing to hoist things out
        // of more control-dependent blocks in loop bodies.
        match cand_loop_depth.cmp(&best_loop_depth) {
            Ordering::Less => return true,
            Ordering::Greater => return false,
            Ordering::Equal => {
                // Move on...
            }
        }

        // Don't put things in split loop backedges when we don't need to, because doing so can
        // "undo" loop rotation and introduce extra branches.
        if self.is_split_loop_backedge(best_loc) {
            return true;
        }

        // If we've been instructed to hoist aggressively for register pressure, hoist even into a
        // less-control dependent location as long as we wouldn't be hoisting into a loop. Note that
        // this may execute the node speculatively.
        if hoist_mode == RegPressureHoistMode::Always {
            return true;
        }

        // Don't hoist loop induction variable updates, because they may cause copies to be inserted
        // on split loop backedges if they aren't the last use of the previous induction variable
        // value.
        if self.indvar_update_nodes.contains(node) {
            return false;
        }

        // At this point, make sure we never hoist into a less control-dependent block so things
        // aren't speculated too eagerly.
        if !postdomtree.cfg_postdominates(best_loc, cand_loc) {
            return false;
        }

        // Hoist the node if doing so would reduce register pressure or if its critical path doesn't
        // fit in the current block.
        hoist_mode == RegPressureHoistMode::ControlDependent
            || self.node_data[node].cp_length > self.block_data[best_loc].max_cp_length
    }

    fn reg_pressure_hoist_mode(
        &self,
        graph: &ValGraph,
        node: Node,
        early_loc: Block,
        late_loc: Block,
    ) -> RegPressureHoistMode {
        // If placing this node will always terminate tethered live ranges, prefer to hoist it
        // (potentially even speculatively) if its input live ranges would otherwise be stretched
        // too far.
        if self.max_tethered_lrg_delta(graph, node) < 0 {
            let domtree = self.domtree();
            let depth_map = self.depth_map();

            let early_tree_loc = domtree.get_tree_node(early_loc).unwrap();
            let late_tree_loc = domtree.get_tree_node(late_loc).unwrap();

            // If not hoisting the instruction would end up making live ranges too long, prefer
            // to hoist even into a less control-dependent location.
            if depth_map.domtree_depth(late_tree_loc)
                > depth_map.domtree_depth(early_tree_loc) + MAX_LRG_TERMINATOR_SINK_DEPTH
            {
                return RegPressureHoistMode::Always;
            }

            // If the live range would be small enough regardless, prefer to hoist into regions that
            // aren't less control-dependent.
            return RegPressureHoistMode::ControlDependent;
        }

        RegPressureHoistMode::None
    }

    fn early_schedule_location(&self, ctx: &ScheduleContext<'_>, node: Node) -> Block {
        trace!("early: node {}", node.as_u32());
        let loc = dataflow_preds(ctx.graph, node)
            .map(|pred| {
                let pred_loc = self.node_data[pred]
                    .block
                    .expect("data flow cycle or dead input");
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

    fn late_schedule_location(
        &self,
        ctx: &ScheduleContext<'_>,
        node: Node,
        early_loc: Block,
    ) -> Option<Block> {
        let graph = ctx.graph;

        trace!("late: node {}", node.as_u32());

        let mut succ_locs =
            dataflow_succs(ctx.graph, ctx.live_nodes(), node).filter_map(|(succ, input_idx)| {
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
                    let loc = self.node_data[succ].block.expect("data flow cycle");
                    trace!("    succ: node {} ({loc})", succ.as_u32());
                    Some(loc)
                }
            });

        let loc = 'lca: {
            // The node might not have any live uses if all uses are dead phi inputs.
            let mut lca_loc = succ_locs.next()?;

            // If we ever hit the early location, we have no reason to keep inspecting predecessors
            // and climbing the dominator tree. Note that this is strictly an optimization and
            // should not affect the computed location.
            if lca_loc == early_loc {
                break 'lca lca_loc;
            }

            for loc in succ_locs {
                lca_loc = self.domtree_lca(lca_loc, loc);

                // Once more, we can stop looking if we ever hit the early location.
                if lca_loc == early_loc {
                    break 'lca lca_loc;
                }
            }

            lca_loc
        };

        trace!("late: node {} -> {loc}", node.as_u32());
        Some(loc)
    }

    fn assign_and_append(&mut self, block: Block, node: Node) {
        self.assign_block(block, node);
        self.bump_max_cp_length(block, node);
        let block_data = &mut self.schedule.block_data[block];
        block_data
            .scheduled_nodes
            .push(node, &mut self.schedule.node_list_pool);
    }

    fn assign_block(&mut self, block: Block, node: Node) {
        self.node_data[node].block = block.into();
    }

    fn bump_max_cp_length(&mut self, block: Block, node: Node) {
        let node_cp_length = self.node_data[node].cp_length;
        if node_cp_length > self.block_data[block].max_cp_length {
            self.block_data[block].max_cp_length = node_cp_length;
        }
    }

    fn mark_node_tethered(&mut self, graph: &ValGraph, node: Node) {
        for output in dataflow_outputs(graph, node) {
            self.tethered_values.insert(output);
        }
    }

    fn is_split_loop_backedge(&self, block: Block) -> bool {
        let cfg = self.cfg();

        // Is this a split critical edge? Split critical edges have one predecessor, one successor
        // and no pinned nodes.

        if cfg.block_preds(block).len() != 1 {
            return false;
        }

        let &[succ] = cfg.block_succs(block) else {
            return false;
        };

        if !self.schedule.block_data[block].scheduled_nodes.is_empty() {
            return false;
        }

        // Is this split edge a natural loop backedge (does it flow to a dominator)?
        self.domtree().cfg_dominates(succ, block)
    }

    fn max_tethered_lrg_delta(&self, graph: &ValGraph, node: Node) -> i32 {
        let added = dataflow_outputs(graph, node).count() as i32;

        // This under-estimates the number of terminated live ranges, but is simple and does not
        // depend on the node's placement.
        let terminated = dataflow_inputs(graph, node)
            .filter(|&input| self.tethered_values.contains(input) && graph.has_one_use(input))
            .count() as i32;

        added - terminated
    }

    fn is_loop_indvar_update(&self, ctx: &ScheduleContext<'_>, node: Node) -> bool {
        let graph = ctx.graph;
        let domtree = self.domtree();
        let block_map = self.block_map;

        dataflow_succs(graph, ctx.live_nodes(), node).any(|(succ, idx)| {
            if matches!(graph.node_kind(succ), NodeKind::Phi) {
                // Consider this node an induction variable update if it feeds into a phi along a
                // natural loop backedge.

                let region = graph.value_def(graph.node_inputs(succ)[0]).0;
                let cfg_pred = graph
                    .value_def(graph.node_inputs(region)[idx as usize - 1])
                    .0;

                // We know that `region` be live (and thus exist in the block map) because its
                // attached phi is live.
                let region = block_map.containing_block(region).unwrap();

                // By contrast, the CFG predecessor corresponding to this phi input may be dead, so
                // be careful here.
                if let Some(cfg_pred) = block_map.containing_block(cfg_pred) {
                    // Is the edge `cfg_pred -> region` a natural loop backedge?
                    return domtree.cfg_dominates(region, cfg_pred);
                }
            }

            false
        })
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

    fn postdomtree(&self) -> &'a BlockPostDomTree {
        &self.cfg_ctx.postdomtree
    }

    fn depth_map(&self) -> &'a BlockDepthMap {
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
    /// The number of live ranges forward execution of this instruction is expected to add. May be
    /// negative if the instruction terminates more live ranges than it creates.
    live_range_delta: i32,
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
    node_data: &'a SecondaryMap<Node, NodeSchedulerData>,
    indvar_update_nodes: &'a DenseEntitySet<Node>,
    scheduled: DenseEntitySet<Node>,
    block: Block,
    block_node_data: FxHashMap<Node, BlockNodeData>,
    block_value_data: FxHashMap<DepValue, BlockValueData>,
    unique_input_pool: ListPool<DepValue>,
    value_use_pool: ListPool<Node>,
    ready_nodes: BinaryHeap<ReadyNode>,
}

impl<'a> BlockScheduler<'a> {
    fn new(
        graph: &'a ValGraph,
        node_data: &'a SecondaryMap<Node, NodeSchedulerData>,
        indvar_update_nodes: &'a DenseEntitySet<Node>,
    ) -> Self {
        Self {
            graph,
            node_data,
            indvar_update_nodes,
            scheduled: DenseEntitySet::new(),
            block: Block::reserved_value(),
            block_node_data: FxHashMap::default(),
            block_value_data: FxHashMap::default(),
            unique_input_pool: ListPool::new(),
            value_use_pool: ListPool::new(),
            ready_nodes: BinaryHeap::new(),
        }
    }

    fn reset(&mut self, block: Block, nodes: &[Node]) {
        self.block_node_data.clear();
        self.unique_input_pool.clear();
        self.block_value_data.clear();

        self.block = block;

        // Start by recording every node's inputs and counting outstanding uses.
        for &node in nodes {
            let unscheduled_preds = self.count_unscheduled_preds(node);

            let node_data = match self.block_node_data.entry(node) {
                Entry::Vacant(entry) => entry.insert(BlockNodeData {
                    unscheduled_preds,
                    live_range_delta: 0,
                    unique_inputs: EntityList::from_iter(
                        dataflow_inputs(self.graph, node),
                        &mut self.unique_input_pool,
                    ),
                }),
                Entry::Occupied(_) => panic!("node data already prepared"),
            };

            dedup_entity_list(&mut node_data.unique_inputs, &mut self.unique_input_pool);

            // Record outstanding uses for all incoming values.
            for &input in node_data.unique_inputs.as_slice(&self.unique_input_pool) {
                let value_data = self.block_value_data.entry(input).or_default();
                value_data.users.push(node, &mut self.value_use_pool);
                value_data.outstanding_uses += 1;
            }
        }

        // Now that all outstanding uses across the block have been counted, compute live range
        // deltas and enqueue nodes.
        for &node in nodes {
            let node_data = self.block_node_data.get_mut(&node).unwrap();

            node_data.live_range_delta = dataflow_outputs(self.graph, node).count() as i32;

            for &input in node_data.unique_inputs.as_slice(&self.unique_input_pool) {
                if self.block_value_data[&input].outstanding_uses == 1 {
                    // This node is the only use of the input in the block, so it terminates the
                    // input's live range.
                    node_data.live_range_delta -= 1;
                }
            }
            self.enqueue_if_ready(node);
        }
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
                "    place: node {} [{}, lrg delta {}, cp {}]",
                node.as_u32(),
                if self.indvar_update_nodes.contains(node) {
                    "indvar"
                } else {
                    "non-indvar"
                },
                self.block_node_data[&node].live_range_delta,
                self.node_data[node].cp_length,
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
                // The remaining user of this value is now the last in the block, so adjust its live
                // range delta and report it to the caller.

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
                        .live_range_delta -= 1;
                    // Note: this node may be pushed multiple times if several of its inputs are
                    // last users.
                    updated_last_users.push(last_user);
                }
            }
        }
    }

    fn enqueue_if_ready(&mut self, node: Node) {
        if self.block_node_data[&node].unscheduled_preds == 0 {
            debug_assert!(!self.scheduled.contains(node));
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

    fn node_prio(&self, node: Node) -> u64 {
        // We want lower `live_range_delta` values to have higher priority, so we need an unsigned
        // value that increases when `live_range_delta` decreases. Accomplish this by centering
        // `-live_range_delta` around the middle of the 31-bit unsigned range.
        let biased_delta =
            (1u32 << 30).wrapping_sub(self.block_node_data[&node].live_range_delta as u32);

        let is_indvar_update = self.indvar_update_nodes.contains(node);

        // Prefer to place non-induction-variable-updates first (to avoid introducing copies on
        // split backedges), then prefer reducing live ranges, and finally fall back to CP length
        // when tied.
        ((!is_indvar_update as u64) << 63)
            | ((biased_delta as u64) << 32)
            | self.node_data[node].cp_length as u64
    }

    fn is_block_interior_node(&self, node: Node) -> bool {
        self.node_data[node].block.expand() == Some(self.block)
            && is_block_interior_node_kind(self.graph.node_kind(node))
    }
}

fn is_block_interior_node_kind(kind: &NodeKind) -> bool {
    !kind.is_terminator() && !matches!(kind, NodeKind::Phi | NodeKind::Region)
}

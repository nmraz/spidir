use core::{cell::RefCell, ops::ControlFlow};

use alloc::vec::Vec;

use dominators::loops::LoopForest;
use graphwalk::{entity_postorder, GraphRef};
use log::trace;
use smallvec::SmallVec;

use crate::cfg::{Block, BlockCfg, BlockDomTree};

pub fn compute_block_order(
    cfg: &BlockCfg,
    domtree: &BlockDomTree,
    loop_forest: &LoopForest,
) -> Vec<Block> {
    let entry = domtree.get_cfg_node(domtree.root());

    let grouped_cfg = LoopGroupedCfg {
        cfg,
        domtree,
        loop_forest,
        other_loop_succ_cache: RefCell::new(SmallVec::new()),
    };

    // Take the RPO, but cluster loops together.
    let mut order: Vec<_> = entity_postorder(grouped_cfg, [entry]).collect();
    order.reverse();
    order
}

struct LoopGroupedCfg<'a> {
    cfg: &'a BlockCfg,
    domtree: &'a BlockDomTree,
    loop_forest: &'a LoopForest,
    other_loop_succ_cache: RefCell<SmallVec<[Block; 4]>>,
}

impl GraphRef for LoopGroupedCfg<'_> {
    type Node = Block;

    fn successors(
        &self,
        node: Self::Node,
        mut f: impl FnMut(Self::Node) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        // Guiding principle: try to cause successors of node that are not nested in its loop to be
        // visited by the DFS *after* nodes that are not nested in the loop, so that the loop nodes
        // will end up earlier in the RPO.

        let tree_node = self
            .domtree
            .get_tree_node(node)
            .expect("reachable CFG node not in domtree");
        let containing_loop = self.loop_forest.containing_loop(tree_node);

        trace!("examining succs for {node}:");

        let mut other_loop = self.other_loop_succ_cache.borrow_mut();
        other_loop.clear();

        // Report successors in two passes: during the first (run online as successors are being
        // traversed), successors belonging to the same or nested loops are reported.

        for &succ in self.cfg.block_succs(node) {
            let tree_succ = self.domtree.get_tree_node(succ).unwrap();
            let succ_loop = self.loop_forest.containing_loop(tree_succ);

            trace!("  {succ}:");

            if succ_loop == containing_loop {
                trace!("    same loop");
                f(succ)?;
                continue;
            }

            let Some(succ_loop) = succ_loop else {
                trace!("    exiting outermost loop");
                other_loop.push(succ);
                continue;
            };

            // Note: The loop header dominates all nodes in the loop, so it is impossible to enter
            // a child loop without passing through its header.
            if tree_succ == self.loop_forest.loop_header(succ_loop) {
                trace!("    entering child loop");
                f(succ)?;
            } else {
                trace!("    sibling or parent loop");
                other_loop.push(succ);
            }
        }

        other_loop.iter().copied().try_for_each(f)
    }
}

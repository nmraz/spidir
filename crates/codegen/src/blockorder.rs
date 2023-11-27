use alloc::vec::Vec;

use dominators::loops::LoopForest;
use graphwalk::{entity_postorder, GraphRef};
use log::trace;

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
}

impl GraphRef for LoopGroupedCfg<'_> {
    type Node = Block;

    fn successors(&self, node: Self::Node, mut f: impl FnMut(Self::Node)) {
        // Guiding principle: try to cause successors of node that are not nested in its loop to be
        // visited by the DFS *after* nodes that are not nested in the loop, so that the loop nodes
        // will end up earlier in the RPO.

        let tree_node = self
            .domtree
            .get_tree_node(node)
            .expect("reachable CFG node not in domtree");
        let containing_loop = self.loop_forest.containing_loop(tree_node);

        trace!("examining succs for {node}:");

        let mut same_loop = Vec::new();
        let mut other_loop = Vec::new();

        for &succ in self.cfg.block_succs(node) {
            let tree_succ = self.domtree.get_tree_node(succ).unwrap();
            let succ_loop = self.loop_forest.containing_loop(tree_succ);

            trace!("  {succ}:");

            if succ_loop == containing_loop {
                trace!("    same loop");
                same_loop.push(succ);
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
                same_loop.push(succ);
            } else {
                trace!("    sibling or parent loop");
                other_loop.push(succ);
            }
        }

        // Take successors from the same/child loops first, which will guarantee that they will
        // be visited *later* by the DFS, ultimately causing them show up earlier in the RPO.
        same_loop.iter().copied().for_each(&mut f);
        other_loop.iter().copied().for_each(f);
    }
}

use core::iter;

use alloc::borrow::ToOwned;

use cranelift_entity::{entity_impl, packed_option::PackedOption, PrimaryMap, SecondaryMap};
use smallvec::SmallVec;

use crate::{
    domtree::{DomTree, TreeNode},
    valgraph::ValGraph,
    valwalk::cfg_preds,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Loop(u32);
entity_impl!(Loop, "loop");

struct LoopData {
    parent: PackedOption<Loop>,
    header: TreeNode,
}

pub struct LoopForest {
    loops: PrimaryMap<Loop, LoopData>,
    containing_loops: SecondaryMap<TreeNode, PackedOption<Loop>>,
}

impl LoopForest {
    pub fn compute(graph: &ValGraph, domtree: &DomTree) -> Self {
        let mut forest = Self {
            loops: PrimaryMap::new(),
            containing_loops: SecondaryMap::new(),
        };

        let mut latches = SmallVec::<[TreeNode; 4]>::new();

        for tree_node in domtree.postorder() {
            // Detect if `tree_node` is a loop header by checking if it has a CFG
            // predecessor that it dominates.
            let cfg_node = domtree.get_cfg_node(tree_node);

            latches.clear();
            for pred in cfg_preds(graph, cfg_node) {
                let Some(pred_tree_node) = domtree.get_tree_node(pred) else {
                    // Skip any unreachable nodes.
                    continue;
                };

                if domtree.dominates(tree_node, pred_tree_node) {
                    latches.push(pred_tree_node);
                }
            }

            if !latches.is_empty() {
                // We've found backedges to a dominator, so it must be a loop header.
                let new_loop = forest.create_loop(tree_node);

                // Discover all the nodes making up the loop. We take advantage of the fact
                // that:
                // 1. outer loop headers always dominate inner loop headers
                // 2. we are discovering loops by their headers
                // 3. we are traversing the dominator tree in postorder
                // to conclude that no ancestors of `new_loop` have yet been found, making
                // this function legal to call.
                forest.discover_loop(graph, domtree, new_loop, &latches);
            }
        }

        forest
    }

    pub fn loop_header(&self, loop_node: Loop) -> TreeNode {
        self.loops[loop_node].header
    }

    pub fn loop_parent(&self, loop_node: Loop) -> Option<Loop> {
        self.loops[loop_node].parent.expand()
    }

    pub fn containing_loop(&self, node: TreeNode) -> Option<Loop> {
        self.containing_loops
            .get(node)
            .and_then(|loop_node| loop_node.expand())
    }

    pub fn root_loop(&self, loop_node: Loop) -> Loop {
        iter::successors(Some(loop_node), |&loop_node| self.loop_parent(loop_node))
            .last()
            .unwrap()
    }

    /// Discovers the maximal loop containing `latches` and populates `loop_node` accordingly,
    /// assuming no ancestors of the loop have yet been recorded in the forest.
    fn discover_loop(
        &mut self,
        graph: &ValGraph,
        domtree: &DomTree,
        new_loop: Loop,
        latches: &[TreeNode],
    ) {
        let header = self.loops[new_loop].header;
        let mut stack = latches.to_owned();
        while let Some(node) = stack.pop() {
            if let Some(subloop) = self.containing_loop(node) {
                // This node belongs to an existing loop, and by the function precondition we know
                // it must be a (non-strict) descendant of ours.
                let root_subloop = self.root_loop(subloop);
                if root_subloop == new_loop {
                    // We already know this node is in a subloop of ours, nothing more to do.
                    continue;
                }

                self.loops[root_subloop].parent = new_loop.into();

                // Keep walking at the subloop's header to discover remaining nodes.
                for pred in cfg_preds(graph, domtree.get_cfg_node(self.loop_header(root_subloop))) {
                    if let Some(tree_pred) = domtree.get_tree_node(pred) {
                        debug_assert!(domtree.dominates(header, tree_pred));

                        // Don't bother walking back to the header since nothing interesting would
                        // come of it anyway. Note that this is strictly an optimization, as walking
                        // the header would fall into a benign "existing loop" case.
                        if tree_pred != header {
                            stack.push(tree_pred);
                        }
                    }
                }
            } else {
                // This node hasn't been discovered yet, so it must be a part of our loop as the
                // header dominates all nodes we discover here.
                self.containing_loops[node] = new_loop.into();
                for pred in cfg_preds(graph, domtree.get_cfg_node(node)) {
                    if let Some(tree_pred) = domtree.get_tree_node(pred) {
                        debug_assert!(domtree.dominates(header, tree_pred));

                        // Don't bother walking back to the header since nothing interesting would
                        // come of it anyway. Note that this is strictly an optimization, as walking
                        // the header would fall into a benign "existing loop" case.
                        if tree_pred != header {
                            stack.push(tree_pred);
                        }
                    }
                }
            }
        }
    }

    fn create_loop(&mut self, header: TreeNode) -> Loop {
        let loop_node = self.loops.push(LoopData {
            parent: None.into(),
            header,
        });
        self.containing_loops[header] = loop_node.into();
        loop_node
    }
}

use alloc::{borrow::ToOwned, vec::Vec};
use core::{iter, ops::ControlFlow};

use cranelift_entity::{
    EntityRef, PrimaryMap, SecondaryMap, entity_impl, packed_option::PackedOption,
};
use graphwalk::PredGraphRef;
use smallvec::SmallVec;

use crate::{
    IntoCfg,
    domtree::{DomTree, DomTreeNode},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Loop(u32);
entity_impl!(Loop, "loop");

struct LoopData {
    parent: PackedOption<Loop>,
    header: DomTreeNode,
}

pub struct LoopForest {
    loops: PrimaryMap<Loop, LoopData>,
    containing_loops: SecondaryMap<DomTreeNode, PackedOption<Loop>>,
}

impl LoopForest {
    pub fn compute<N: EntityRef>(graph: impl IntoCfg<Node = N>, domtree: &DomTree<N>) -> Self {
        let graph = graph.into_cfg();

        let mut forest = Self {
            loops: PrimaryMap::new(),
            containing_loops: SecondaryMap::new(),
        };

        let mut latches = SmallVec::<[DomTreeNode; 4]>::new();

        for tree_node in domtree.postorder() {
            // Detect if `tree_node` is a loop header by checking if it has a CFG
            // predecessor that it dominates.
            let cfg_node = domtree.get_cfg_node(tree_node);

            latches.clear();
            graph.predecessors(cfg_node, |pred| {
                let Some(pred_tree_node) = domtree.get_tree_node(pred) else {
                    // Skip any unreachable nodes.
                    return;
                };

                if domtree.dominates(tree_node, pred_tree_node) {
                    latches.push(pred_tree_node);
                }
            });

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
                forest.discover_loop(&graph, domtree, new_loop, &latches);
            }
        }

        forest
    }

    #[inline]
    pub fn loop_header(&self, loop_node: Loop) -> DomTreeNode {
        self.loops[loop_node].header
    }

    #[inline]
    pub fn loop_parent(&self, loop_node: Loop) -> Option<Loop> {
        self.loops[loop_node].parent.expand()
    }

    #[inline]
    pub fn loop_ancestors(&self, loop_node: Loop) -> impl Iterator<Item = Loop> + '_ {
        iter::successors(Some(loop_node), |&loop_node| self.loop_parent(loop_node))
    }

    pub fn loop_depth(&self, loop_node: Loop) -> u32 {
        self.loop_ancestors(loop_node).count() as u32
    }

    pub fn root_loop(&self, loop_node: Loop) -> Loop {
        self.loop_ancestors(loop_node).last().unwrap()
    }

    #[inline]
    pub fn containing_loop(&self, node: DomTreeNode) -> Option<Loop> {
        self.containing_loops[node].expand()
    }

    pub fn is_latch<N: EntityRef>(
        &self,
        graph: impl IntoCfg<Node = N>,
        domtree: &DomTree<N>,
        loop_node: Loop,
        node: DomTreeNode,
    ) -> bool {
        let header = self.loop_header(loop_node);
        if !domtree.dominates(header, node) {
            return false;
        }

        let node = domtree.get_cfg_node(node);

        graph
            .into_cfg()
            .try_predecessors(domtree.get_cfg_node(header), |pred| {
                if pred == node {
                    ControlFlow::Break(())
                } else {
                    ControlFlow::Continue(())
                }
            })
            .is_break()
    }

    /// Discovers the maximal loop containing `latches` and populates `loop_node` accordingly,
    /// assuming no ancestors of the loop have yet been recorded in the forest.
    fn discover_loop<N: EntityRef>(
        &mut self,
        graph: &impl PredGraphRef<Node = N>,
        domtree: &DomTree<N>,
        new_loop: Loop,
        latches: &[DomTreeNode],
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

                // Keep walking at the subloop's header to discover remaining nodes in our outer
                // loop.
                push_loop_preds(
                    graph,
                    domtree,
                    self.loop_header(root_subloop),
                    header,
                    &mut stack,
                );
            } else {
                // This node hasn't been discovered yet, so it must be a part of our loop as the
                // header dominates all nodes we discover here.
                self.containing_loops[node] = new_loop.into();
                push_loop_preds(graph, domtree, node, header, &mut stack);
            }
        }
    }

    fn create_loop(&mut self, header: DomTreeNode) -> Loop {
        let loop_node = self.loops.push(LoopData {
            parent: None.into(),
            header,
        });
        self.containing_loops[header] = loop_node.into();
        loop_node
    }
}

fn push_loop_preds<N: EntityRef>(
    graph: &impl PredGraphRef<Node = N>,
    domtree: &DomTree<N>,
    node: DomTreeNode,
    header: DomTreeNode,
    stack: &mut Vec<DomTreeNode>,
) {
    graph.predecessors(domtree.get_cfg_node(node), |pred| {
        if let Some(tree_pred) = domtree.get_tree_node(pred) {
            debug_assert!(domtree.dominates(header, tree_pred));

            // Don't bother walking back to the header since nothing interesting would
            // come of it anyway. Note that this is strictly an optimization, as walking
            // the header would fall into a benign "existing loop" case.
            if tree_pred != header {
                stack.push(tree_pred);
            }
        }
    });
}

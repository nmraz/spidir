use cranelift_entity::{entity_impl, packed_option::PackedOption, PrimaryMap, SecondaryMap};
use dominators::domtree::DomTree;
use graphwalk::{GraphRef, PredGraphRef};
use log::trace;
use smallvec::SmallVec;

use crate::{
    node::NodeKind,
    valgraph::{DepValue, Node, ValGraph},
    valwalk::{cfg_outputs, cfg_preds, cfg_succs},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Block(u32);
entity_impl!(Block, "block");

struct BlockData {
    header: Node,
    terminator: Node,
}

pub type BlockDomTree = DomTree<Block>;

pub struct BlockCfg {
    blocks: PrimaryMap<Block, BlockData>,
    // If this is too sparse, we may want to consider a hashmap instead.
    blocks_by_node: SecondaryMap<Node, PackedOption<Block>>,
}

impl BlockCfg {
    pub fn compute(graph: &ValGraph, cfg_preorder: impl Iterator<Item = Node>) -> Self {
        let mut ctrl_outputs = SmallVec::<[DepValue; 4]>::new();
        let mut cfg = Self {
            blocks: PrimaryMap::new(),
            blocks_by_node: SecondaryMap::new(),
        };

        let mut cur_block = None;
        for node in cfg_preorder {
            let block = match cur_block {
                Some(cur_block) => cur_block,
                None => {
                    let block = cfg.create_block(node);
                    cur_block = Some(block);
                    block
                }
            };

            cfg.blocks_by_node[node] = block.into();

            ctrl_outputs.clear();
            ctrl_outputs.extend(cfg_outputs(graph, node));
            if should_terminate_block(graph, &ctrl_outputs) {
                cfg.terminate_block(block, node);
                cur_block = None;
            }
        }

        cfg
    }

    #[inline]
    pub fn containing_block(&self, node: Node) -> Option<Block> {
        self.blocks_by_node[node].expand()
    }

    #[inline]
    pub fn block_header(&self, block: Block) -> Node {
        self.blocks[block].header
    }

    #[inline]
    pub fn block_terminator(&self, block: Block) -> Node {
        self.blocks[block].terminator
    }

    pub fn succs<'a>(
        &'a self,
        graph: &'a ValGraph,
        block: Block,
    ) -> impl Iterator<Item = Block> + 'a {
        cfg_succs(graph, self.block_terminator(block))
            .map(move |succ| self.containing_block(succ).unwrap())
    }

    pub fn preds<'a>(
        &'a self,
        graph: &'a ValGraph,
        block: Block,
    ) -> impl Iterator<Item = Block> + 'a {
        // Ignore any predecessors that don't belong to any block, which can happen if there are
        // dead control nodes.
        cfg_preds(graph, self.block_header(block)).filter_map(|pred| self.containing_block(pred))
    }

    #[inline]
    pub fn graph_ref<'a>(&'a self, graph: &'a ValGraph) -> BlockCfgGraphRef<'a> {
        BlockCfgGraphRef { graph, cfg: self }
    }

    fn create_block(&mut self, header: Node) -> Block {
        let block = self.blocks.push(BlockData {
            header,
            terminator: header,
        });
        trace!("discovered {block} with header {}", header.as_u32());
        block
    }

    fn terminate_block(&mut self, block: Block, terminator: Node) {
        trace!("terminating {block} with node {}", terminator.as_u32());
        self.blocks[block].terminator = terminator;
    }
}

#[derive(Clone, Copy)]
pub struct BlockCfgGraphRef<'a> {
    graph: &'a ValGraph,
    cfg: &'a BlockCfg,
}

impl GraphRef for BlockCfgGraphRef<'_> {
    type Node = Block;

    fn successors(&self, node: Self::Node, f: impl FnMut(Self::Node)) {
        self.cfg.succs(self.graph, node).for_each(f);
    }
}

impl PredGraphRef for BlockCfgGraphRef<'_> {
    fn predecessors(&self, node: Self::Node, f: impl FnMut(Self::Node)) {
        self.cfg.preds(self.graph, node).for_each(f);
    }
}

fn should_terminate_block(graph: &ValGraph, ctrl_outputs: &[DepValue]) -> bool {
    if ctrl_outputs.len() != 1 {
        return true;
    }

    // We have a single output at this point.
    let succ = graph.value_uses(ctrl_outputs[0]).next().unwrap().0;

    // Create a new block for all region nodes (even if they have a single input), so that we don't
    // have to deal with phi nodes in the "middle" of blocks later.
    matches!(graph.node_kind(succ), NodeKind::Region)
}

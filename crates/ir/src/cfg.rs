use cranelift_entity::{
    entity_impl, packed_option::PackedOption, EntityList, ListPool, PrimaryMap, SecondaryMap,
};
use dominators::domtree::DomTree;
use graphwalk::{GraphRef, PredGraphRef};
use log::trace;
use smallvec::SmallVec;

use crate::{
    node::NodeKind,
    valgraph::{DepValue, Node, ValGraph},
    valwalk::cfg_outputs,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Block(u32);
entity_impl!(Block, "block");

#[derive(Default, Clone, Copy)]
struct BlockLinks {
    preds: EntityList<Block>,
    succs: EntityList<Block>,
}

pub type BlockDomTree = DomTree<Block>;

pub struct BlockCfg {
    blocks: PrimaryMap<Block, BlockLinks>,
    // If this is too sparse, we may want to consider a hashmap instead.
    blocks_by_node: SecondaryMap<Node, PackedOption<Block>>,
    block_link_pool: ListPool<Block>,
}

impl BlockCfg {
    pub fn compute(graph: &ValGraph, cfg_preorder: impl Iterator<Item = Node>) -> Self {
        let mut ctrl_outputs = SmallVec::<[DepValue; 4]>::new();
        let mut cfg = Self {
            blocks: PrimaryMap::new(),
            blocks_by_node: SecondaryMap::new(),
            block_link_pool: ListPool::new(),
        };

        let mut cur_block: Option<Block> = None;
        for node in cfg_preorder {
            let block = match cur_block {
                Some(cur_block) => {
                    cfg.blocks_by_node[node] = cur_block.into();
                    cur_block
                }
                None => {
                    let block = cfg.get_headed_block(node);
                    cur_block = Some(block);
                    block
                }
            };

            ctrl_outputs.clear();
            ctrl_outputs.extend(cfg_outputs(graph, node));

            if should_terminate_block(graph, &ctrl_outputs) {
                cfg.terminate_block(block, node);
                for &output in &ctrl_outputs {
                    let succ = cfg.get_headed_block(graph.value_uses(output).next().unwrap().0);
                    cfg.add_block_edge(block, succ);
                }
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
    pub fn block_succs(&self, block: Block) -> &[Block] {
        self.blocks[block].succs.as_slice(&self.block_link_pool)
    }

    #[inline]
    pub fn block_preds(&self, block: Block) -> &[Block] {
        self.blocks[block].preds.as_slice(&self.block_link_pool)
    }

    fn add_block_edge(&mut self, pred: Block, succ: Block) {
        self.blocks[pred]
            .succs
            .push(succ, &mut self.block_link_pool);
        self.blocks[succ]
            .preds
            .push(pred, &mut self.block_link_pool);
    }

    fn get_headed_block(&mut self, header: Node) -> Block {
        if let Some(block) = self.containing_block(header) {
            return block;
        }

        let block = self.blocks.push(BlockLinks::default());
        self.blocks_by_node[header] = block.into();
        trace!("discovered {block} with header {}", header.as_u32());
        block
    }

    fn terminate_block(&mut self, block: Block, terminator: Node) {
        trace!("terminating {block} with node {}", terminator.as_u32());
    }
}

impl GraphRef for &'_ BlockCfg {
    type Node = Block;

    fn successors(&self, node: Block, f: impl FnMut(Block)) {
        self.block_succs(node).iter().copied().for_each(f);
    }
}

impl PredGraphRef for &'_ BlockCfg {
    fn predecessors(&self, node: Block, f: impl FnMut(Block)) {
        self.block_preds(node).iter().copied().for_each(f);
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

use alloc::vec::Vec;
use cranelift_entity::{
    entity_impl, packed_option::PackedOption, EntityList, ListPool, PrimaryMap, SecondaryMap,
};
use dominators::{domtree::DomTree, loops::LoopForest};
use fx_utils::FxHashMap;
use graphwalk::{GraphRef, PredGraphRef};
use ir::{
    node::NodeKind,
    valgraph::{DepValue, Node, ValGraph},
    valwalk::{cfg_inputs, cfg_outputs},
};
use log::trace;
use smallvec::SmallVec;

use crate::blockorder::compute_block_order;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Block(u32);
entity_impl!(Block, "block");

#[derive(Default, Clone, Copy)]
struct BlockLinks {
    preds: EntityList<Block>,
    succs: EntityList<Block>,
}

pub type BlockDomTree = DomTree<Block>;

#[derive(Default, Clone)]
pub struct BlockCfg {
    blocks: PrimaryMap<Block, BlockLinks>,
    block_link_pool: ListPool<Block>,
}

impl BlockCfg {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn block_succs(&self, block: Block) -> &[Block] {
        self.blocks[block].succs.as_slice(&self.block_link_pool)
    }

    #[inline]
    pub fn block_preds(&self, block: Block) -> &[Block] {
        self.blocks[block].preds.as_slice(&self.block_link_pool)
    }

    #[inline]
    pub fn create_block(&mut self) -> Block {
        self.blocks.push(BlockLinks::default())
    }

    pub fn add_block_edge(&mut self, pred: Block, succ: Block) {
        self.blocks[pred]
            .succs
            .push(succ, &mut self.block_link_pool);
        self.blocks[succ]
            .preds
            .push(pred, &mut self.block_link_pool);
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

// Dummy entity for predecessor indices, just so we can store them in a `ListPool`.
#[derive(Clone, Copy, PartialEq, Eq)]
struct PredIndex(u32);
entity_impl!(PredIndex);

pub struct FunctionBlockMap {
    // If this is too sparse, we may want to consider a hashmap instead.
    blocks_by_node: SecondaryMap<Node, PackedOption<Block>>,
    block_valgraph_pred_indices: SecondaryMap<Block, EntityList<PredIndex>>,
    pred_index_pool: ListPool<PredIndex>,
    critical_edges: FxHashMap<DepValue, Block>,
}

impl FunctionBlockMap {
    #[inline]
    pub fn containing_block(&self, node: Node) -> Option<Block> {
        self.blocks_by_node[node].expand()
    }

    pub fn critical_edge_block(&self, edge: DepValue) -> Option<Block> {
        self.critical_edges.get(&edge).copied()
    }

    pub fn cfg_pred_block(&self, graph: &ValGraph, edge: DepValue) -> Option<Block> {
        self.critical_edge_block(edge)
            .or_else(|| self.containing_block(graph.value_def(edge).0))
    }

    pub fn valgraph_pred_index(&self, block: Block, pred: usize) -> Option<u32> {
        let pred_indices = self.block_valgraph_pred_indices[block].as_slice(&self.pred_index_pool);
        pred_indices.get(pred).map(|&idx| idx.0)
    }
}

pub fn compute_block_cfg(graph: &ValGraph, cfg_preorder: &[Node]) -> (BlockCfg, FunctionBlockMap) {
    let mut ctrl_outputs = SmallVec::<[DepValue; 4]>::new();
    let mut cfg = BlockCfg::new();
    let mut block_map = FunctionBlockMap {
        blocks_by_node: SecondaryMap::new(),
        block_valgraph_pred_indices: SecondaryMap::new(),
        pred_index_pool: ListPool::new(),
        critical_edges: FxHashMap::default(),
    };

    let mut cur_block: Option<Block> = None;
    for &node in cfg_preorder {
        let block = match cur_block {
            Some(cur_block) => {
                block_map.blocks_by_node[node] = cur_block.into();
                cur_block
            }
            None => {
                let block = get_headed_block(&mut cfg, &mut block_map, node);
                cur_block = Some(block);
                block
            }
        };

        ctrl_outputs.clear();
        ctrl_outputs.extend(cfg_outputs(graph, node));

        if should_terminate_block(graph, &ctrl_outputs) {
            trace!("terminating {block} with node {}", node.as_u32());

            // Note: it is important that successors added here appear in the same order as the
            // original valgraph successors, because they are intrinsic to the meanings of
            // instructions. Predecessors, on the other hand, will not necessarily have their order
            // preserved by the preorder walk; the original indices will be saved along with each
            // predecessor to assist conversion of phi nodes later.
            for &output in &ctrl_outputs {
                let mut pred_block = block;
                let (succ, succ_pred_index) = graph.value_uses(output).next().unwrap();

                if ctrl_outputs.len() > 1 && !has_single_cfg_input(graph, succ) {
                    let split_block = cfg.create_block();

                    // Note: we're not adding anything to `block_valgraph_pred_indices` here because
                    // this block doesn't actually correspond to anything in the valgraph.
                    cfg.add_block_edge(block, split_block);
                    block_map.critical_edges.insert(output, split_block);

                    pred_block = split_block;
                    trace!(
                        "split critical edge {} -> {} with {split_block}",
                        node.as_u32(),
                        succ.as_u32()
                    );
                }

                let succ = get_headed_block(&mut cfg, &mut block_map, succ);

                // As we add this edge to `succ`, remember the index of the corresponding
                // predecessor in the valgraph so phi nodes can be converted to block params later.
                //
                // Correctness here depends on `succ`'s predecessor list and
                // `block_valgraph_pred_indices` being updated in lockstep.
                cfg.add_block_edge(pred_block, succ);
                block_map.block_valgraph_pred_indices[succ].push(
                    PredIndex::from_u32(succ_pred_index),
                    &mut block_map.pred_index_pool,
                );
            }
            cur_block = None;
        }
    }
    (cfg, block_map)
}

pub struct CfgContext {
    pub cfg: BlockCfg,
    pub block_order: Vec<Block>,
    pub block_map: FunctionBlockMap,
    pub domtree: BlockDomTree,
    pub loop_forest: LoopForest,
}

impl CfgContext {
    pub fn compute(graph: &ValGraph, cfg_preorder: &[Node]) -> Self {
        let (cfg, block_map) = compute_block_cfg(graph, cfg_preorder);

        let domtree =
            BlockDomTree::compute(&cfg, block_map.containing_block(cfg_preorder[0]).unwrap());
        let loop_forest = LoopForest::compute(&cfg, &domtree);

        let block_order = compute_block_order(&cfg, &domtree, &loop_forest);

        CfgContext {
            cfg,
            block_order,
            block_map,
            domtree,
            loop_forest,
        }
    }
}

fn get_headed_block(cfg: &mut BlockCfg, block_map: &mut FunctionBlockMap, header: Node) -> Block {
    if let Some(block) = block_map.containing_block(header) {
        return block;
    }

    let block = cfg.create_block();
    block_map.blocks_by_node[header] = block.into();
    trace!("discovered {block} with header {}", header.as_u32());
    block
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

fn has_single_cfg_input(graph: &ValGraph, node: Node) -> bool {
    let mut cfg_inputs = cfg_inputs(graph, node);
    cfg_inputs.next().is_some() && cfg_inputs.next().is_none()
}

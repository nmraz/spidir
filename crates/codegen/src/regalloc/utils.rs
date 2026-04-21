use crate::{
    cfg::{Block, CfgContext},
    lir::{Instr, Lir},
    machine::MachineRegalloc,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChangeStatus {
    Unchanged,
    Changed,
}

impl ChangeStatus {
    pub fn is_changed(&self) -> bool {
        matches!(self, Self::Changed)
    }
}

pub fn get_weight_at_instr<M: MachineRegalloc>(
    lir: &Lir<M>,
    cfg_ctx: &CfgContext,
    instr: Instr,
) -> f32 {
    let block = cfg_ctx.block_order[lir.instr_block_index(instr)];
    get_block_weight(cfg_ctx, block)
}

pub fn get_block_weight(cfg_ctx: &CfgContext, block: Block) -> f32 {
    let loop_depth = cfg_ctx
        .depth_map
        .loop_depth(cfg_ctx.domtree.get_tree_node(block).unwrap());
    1000f32 * ((loop_depth + 1) as f32)
}

use crate::{
    cfg::CfgContext,
    lir::{Instr, Lir},
    machine::MachineCore,
};

pub fn get_instr_weight<M: MachineCore>(lir: &Lir<M>, cfg_ctx: &CfgContext, instr: Instr) -> f32 {
    let block = lir.instr_block(instr);
    let loop_depth = cfg_ctx
        .depth_map
        .loop_depth(cfg_ctx.domtree.get_tree_node(block).unwrap());
    1000f32 * ((loop_depth + 1) as f32)
}

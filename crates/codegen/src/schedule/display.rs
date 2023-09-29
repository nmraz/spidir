use core::fmt;

use graphwalk::entity_preorder;
use ir::{
    cfg::{Block, BlockCfg},
    module::Module,
    valgraph::ValGraph,
    write::display_node,
};
use itertools::Itertools;

use super::Schedule;

pub struct Display<'a> {
    pub(super) module: &'a Module,
    pub(super) graph: &'a ValGraph,
    pub(super) cfg: &'a BlockCfg,
    pub(super) schedule: &'a Schedule,
    pub(super) entry: Block,
}

impl fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for block in entity_preorder(self.cfg, [self.entry]) {
            writeln!(f, "{block}:")?;
            for &attached_node in self.schedule.scheduled_nodes_rev(block).iter().rev() {
                writeln!(
                    f,
                    "    {}",
                    display_node(self.module, self.graph, attached_node)
                )?;
            }
            let succs = self.cfg.block_succs(block);
            if !succs.is_empty() {
                writeln!(f, "=> {}", succs.iter().format(", "))?;
            }
        }

        Ok(())
    }
}

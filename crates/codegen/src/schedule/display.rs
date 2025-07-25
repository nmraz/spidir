use core::fmt;

use ir::{function::FunctionBody, module::ModuleMetadata, write::display_node};
use itertools::Itertools;

use crate::cfg::{Block, BlockCfg};

use super::Schedule;

pub struct Display<'a> {
    pub(super) module_metadata: &'a ModuleMetadata,
    pub(super) body: &'a FunctionBody,
    pub(super) cfg: &'a BlockCfg,
    pub(super) schedule: &'a Schedule,
    pub(super) block_order: &'a [Block],
}

impl fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for &block in self.block_order {
            writeln!(f, "{block}:")?;
            for &phi in self.schedule.block_phis(block) {
                writeln!(
                    f,
                    "    {}",
                    display_node(self.module_metadata, self.body, phi)
                )?;
            }
            for &node in self.schedule.scheduled_nodes(block) {
                writeln!(
                    f,
                    "    {}",
                    display_node(self.module_metadata, self.body, node)
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

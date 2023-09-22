use std::fmt::Write;

use anyhow::Result;
use ir::{cfg::BlockCfg, module::Module};
use itertools::Itertools;

use crate::utils::write_graph_with_trailing_comments;

use super::{update_per_func_output, TestProvider, Updater};

pub struct CfgProvider;
impl TestProvider for CfgProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            let cfg = BlockCfg::compute(&func.graph, func.entry);
            write_graph_with_trailing_comments(&mut output, module, func, |s, node| {
                if let Some(block) = cfg.containing_block(node) {
                    if node == cfg.block_header(block) {
                        write!(s, "head {block}").unwrap();
                    } else if node == cfg.block_terminator(block) {
                        write!(s, "term {block}").unwrap();
                        let succs: Vec<_> = cfg.succs(&func.graph, block).collect();
                        if !succs.is_empty() {
                            write!(s, " -> {}", succs.iter().format(", ")).unwrap();
                        }
                    } else {
                        write!(s, "body {block}").unwrap();
                    }
                } else {
                    write!(s, "x").unwrap();
                }
                Ok(())
            })?;
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, output_str: &str) -> Result<()> {
        update_per_func_output(updater, output_str)
    }
}

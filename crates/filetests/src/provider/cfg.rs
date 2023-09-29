use std::fmt::Write;

use anyhow::Result;
use codegen::cfg::BlockCfg;
use cranelift_entity::EntitySet;
use ir::{module::Module, valwalk::cfg_preorder};
use itertools::Itertools;

use crate::utils::write_graph_with_trailing_comments;

use super::{update_per_func_output, TestProvider, Updater};

pub struct CfgProvider;
impl TestProvider for CfgProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            let cfg = BlockCfg::compute(&func.graph, cfg_preorder(&func.graph, func.entry));
            let mut seen_blocks = EntitySet::new();
            write_graph_with_trailing_comments(&mut output, module, func, |s, node| {
                if let Some(block) = cfg.containing_block(node) {
                    write!(s, "{block}; ").unwrap();

                    if !seen_blocks.contains(block) {
                        let preds = cfg.block_preds(block);
                        if !preds.is_empty() {
                            write!(s, "preds {}; ", preds.iter().format(", ")).unwrap();
                        }
                        let succs = cfg.block_succs(block);
                        if !succs.is_empty() {
                            write!(s, "succs {}; ", succs.iter().format(", ")).unwrap();
                        }
                        seen_blocks.insert(block);
                    }
                }
                write!(s, "x").unwrap();
            });
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, output_str: &str) -> Result<()> {
        update_per_func_output(updater, output_str)
    }
}

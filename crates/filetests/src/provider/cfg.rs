use std::fmt::Write;

use anyhow::Result;
use codegen::cfg::compute_block_cfg;
use cranelift_entity::EntitySet;
use ir::{module::Module, valwalk::cfg_preorder};
use itertools::Itertools;

use crate::utils::write_body_with_trailing_comments;

use super::{update_per_func_output, TestProvider, Updater};

pub struct CfgProvider;
impl TestProvider for CfgProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            let body = &func.body;
            let cfg_preorder: Vec<_> = cfg_preorder(&body.graph, body.entry).collect();
            let (cfg, block_map) = compute_block_cfg(&body.graph, &cfg_preorder);
            let mut seen_blocks = EntitySet::new();
            write_body_with_trailing_comments(&mut output, module, func, |s, node| {
                if let Some(block) = block_map.containing_block(node) {
                    write!(s, "{block}; ").unwrap();

                    if !seen_blocks.contains(block) {
                        let preds = cfg.block_preds(block);
                        if !preds.is_empty() {
                            write!(
                                s,
                                "preds {}; ",
                                preds.iter().format_with(", ", |&pred, f| {
                                    f(&pred)?;
                                    if let Some(valgraph_pred_index) =
                                        block_map.valgraph_pred_index(block, pred)
                                    {
                                        f(&format_args!(" (#{valgraph_pred_index})"))?;
                                    }
                                    Ok(())
                                })
                            )
                            .unwrap();
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

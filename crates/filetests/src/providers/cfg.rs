use std::fmt::Write;

use anyhow::Result;
use codegen::cfg::compute_block_cfg;
use entity_set::DenseEntitySet;
use ir::module::Module;
use itertools::Itertools;

use crate::{
    provider::{SimpleTestProvider, Updater, update_per_func_output},
    utils::write_body_with_trailing_comments,
};

pub struct CfgProvider;
impl SimpleTestProvider for CfgProvider {
    fn output_for(&self, module: Module) -> Result<String> {
        let mut output = String::new();

        for func in module.metadata.functions().keys() {
            let func = module.borrow_function(func);

            let body = func.body();
            let cfg_preorder = body.compute_cfg_preorder_info();
            let (cfg, block_map) = compute_block_cfg(&body.graph, &cfg_preorder.preorder);
            let mut seen_blocks = DenseEntitySet::new();
            write_body_with_trailing_comments(&mut output, &module.metadata, func, |s, node| {
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

    fn update(&self, updater: &mut Updater<'_>, output_str: &str) -> Result<()> {
        update_per_func_output(updater, output_str)
    }
}

use std::fmt::Write;

use anyhow::Result;
use codegen::schedule::Schedule;
use cranelift_entity::EntitySet;
use filecheck::Value;
use fx_utils::FxHashMap;
use graphwalk::PreOrder;
use ir::{cfg::BlockCfg, module::Module, valwalk::cfg_preorder, write::display_node};

use crate::{regexes::VAL_REGEX, utils::generalize_value_names};

use super::{update_per_func_output, TestProvider, Updater};

pub struct ScheduleProvider;
impl TestProvider for ScheduleProvider {
    fn env(&self) -> FxHashMap<String, Value<'_>> {
        [("val".to_owned(), Value::Regex(VAL_REGEX.into()))]
            .into_iter()
            .collect()
    }

    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            writeln!(output, "function `{}`:", func.name).unwrap();

            let graph = &func.graph;
            let cfg_preorder: Vec<_> = cfg_preorder(graph, func.entry).collect();
            let block_cfg = BlockCfg::compute(graph, cfg_preorder.iter().copied());
            let schedule = Schedule::compute(graph, &cfg_preorder, &block_cfg);

            for block in
                PreOrder::<_, EntitySet<_>>::new(&block_cfg, block_cfg.containing_block(func.entry))
            {
                writeln!(output, "{block}:").unwrap();
                for &attached_node in schedule.scheduled_nodes_rev(block).iter().rev() {
                    writeln!(output, "    {}", display_node(module, graph, attached_node)).unwrap();
                }
            }
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, module: &Module, output_str: &str) -> Result<()> {
        let output_str = generalize_value_names(module, output_str)?;
        update_per_func_output(updater, &output_str)
    }
}

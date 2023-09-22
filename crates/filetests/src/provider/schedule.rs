use std::fmt::Write;

use anyhow::Result;
use codegen::schedule::Schedule;
use filecheck::Value;
use fx_utils::FxHashMap;
use ir::{domtree, loop_forest::LoopForest, module::Module, write::display_node};

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
            let domtree = domtree::compute(graph, func.entry);
            let loop_forest = LoopForest::compute(graph, &domtree);
            let schedule = Schedule::compute(graph, &domtree, &loop_forest);

            let cfg_postorder: Vec<_> = domtree.postorder().collect();
            for &domtree_node in cfg_postorder.iter().rev() {
                let cfg_node = domtree.get_cfg_node(domtree_node);
                writeln!(output, "{}", display_node(module, graph, cfg_node)).unwrap();
                for &attached_node in schedule.attached_nodes_rev(domtree_node).iter().rev() {
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

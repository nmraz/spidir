use std::fmt::Write;

use anyhow::Result;
use codegen::api::schedule_graph;
use filecheck::Value;
use fx_utils::FxHashMap;
use ir::module::Module;

use crate::{
    provider::{update_per_func_output, TestProvider, Updater},
    regexes::VAL_REGEX,
    utils::generalize_per_function_value_names,
};

pub struct ScheduleProvider;
impl TestProvider for ScheduleProvider {
    type AuxOutput = Module;

    fn env(&self) -> FxHashMap<String, Value<'_>> {
        [("val".to_owned(), Value::Regex(VAL_REGEX.into()))]
            .into_iter()
            .collect()
    }

    fn output_for(&self, module: Module) -> Result<(String, Module)> {
        let mut output = String::new();

        for func in module.functions.values() {
            let body = &func.body;

            writeln!(output, "function `{}`:", func.metadata.name).unwrap();

            let (cfg_ctx, _, schedule) = schedule_graph(&body.graph, body.entry);

            write!(
                output,
                "{}",
                schedule.display(&module, body, &cfg_ctx.cfg, &cfg_ctx.block_order)
            )
            .unwrap();
        }

        Ok((output, module))
    }

    fn update(&self, updater: &mut Updater<'_>, output_str: &str, module: Module) -> Result<()> {
        let output_str = generalize_per_function_value_names(&module, output_str)?;
        update_per_func_output(updater, &output_str)
    }
}

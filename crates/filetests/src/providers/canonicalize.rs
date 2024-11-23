use anyhow::Result;
use filecheck::Value;

use fx_utils::FxHashMap;
use ir::module::Module;
use opt::canonicalize::canonicalize;

use crate::{
    provider::{update_transformed_module_output, TestProvider, Updater},
    regexes::VAL_REGEX,
    utils::verify_module_with_err,
};

pub struct CanonicalizeProvider;

impl TestProvider for CanonicalizeProvider {
    type AuxOutput = Module;

    fn env(&self) -> FxHashMap<String, Value<'_>> {
        [("val".to_owned(), Value::Regex(VAL_REGEX.into()))]
            .into_iter()
            .collect()
    }

    fn output_for(&self, mut module: Module) -> Result<(String, Module)> {
        for func in module.functions.values_mut() {
            canonicalize(&mut func.body, &mut func.node_cache);
        }
        verify_module_with_err(&module, "transformed module invalid")?;
        Ok((module.to_string(), module))
    }

    fn update(&self, updater: &mut Updater<'_>, output_str: &str, module: Module) -> Result<()> {
        update_transformed_module_output(updater, &module, output_str)
    }
}

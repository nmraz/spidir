use anyhow::Result;
use filecheck::Value;

use fx_utils::FxHashMap;
use ir::module::Module;
use opt::canonicalize::canonicalize;

use crate::{
    provider::{TestProvider, Updater, update_transformed_module_output},
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
        for (func, body) in module.function_bodies.iter_mut() {
            canonicalize(
                &module.metadata,
                body,
                &mut module.function_node_caches[func],
            );
        }
        verify_module_with_err(&module, "transformed module invalid")?;
        Ok((module.to_string(), module))
    }

    fn update(&self, updater: &mut Updater<'_>, output_str: &str, module: Module) -> Result<()> {
        update_transformed_module_output(updater, &module, output_str)
    }
}

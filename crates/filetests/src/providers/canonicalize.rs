use anyhow::Result;
use filecheck::Value;
use fx_utils::FxHashMap;
use ir::{canonicalize::canonicalize, module::Module};

use crate::{
    provider::{update_transformed_module_output, SimpleTestProvider, Updater},
    regexes::VAL_REGEX,
};

pub struct CanonicalizeProvider;

impl SimpleTestProvider for CanonicalizeProvider {
    fn env(&self) -> FxHashMap<String, Value<'_>> {
        [("val".to_owned(), Value::Regex(VAL_REGEX.into()))]
            .into_iter()
            .collect()
    }

    fn output_for(&self, mut module: Module) -> Result<String> {
        for func in module.functions.values_mut() {
            canonicalize(&mut func.body);
        }
        Ok(module.to_string())
    }

    fn update(&self, updater: &mut Updater<'_>, output_str: &str) -> Result<()> {
        update_transformed_module_output(updater, output_str)
    }
}

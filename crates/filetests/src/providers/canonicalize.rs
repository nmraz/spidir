use anyhow::Result;
use filecheck::Value;
use fx_utils::FxHashMap;
use ir::{canonicalize::canonicalize, module::Module};

use crate::{
    provider::{update_transformed_module_output, TestProvider, Updater},
    regexes::VAL_REGEX,
};

pub struct CanonicalizeProvider;

impl TestProvider for CanonicalizeProvider {
    fn env(&self) -> FxHashMap<String, Value<'_>> {
        [("val".to_owned(), Value::Regex(VAL_REGEX.into()))]
            .into_iter()
            .collect()
    }

    fn output_for(&self, module: &Module) -> Result<String> {
        let mut module = module.clone();
        for func in module.functions.values_mut() {
            canonicalize(&mut func.body);
        }
        Ok(module.to_string())
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, output_str: &str) -> Result<()> {
        update_transformed_module_output(updater, output_str)
    }
}

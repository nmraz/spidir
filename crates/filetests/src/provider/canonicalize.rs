use anyhow::Result;
use ir::{canonicalize::canonicalize, module::Module};

use super::{update_transformed_module_output, TestProvider, Updater};

pub struct CanonicalizeProvider;

impl TestProvider for CanonicalizeProvider {
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

use anyhow::{Result, bail};
use filecheck::Value;

use fx_utils::FxHashMap;
use ir::module::Module;
use opt::ModulePipeline;

use crate::{
    provider::{TestProvider, Updater, update_transformed_module_output},
    regexes::VAL_REGEX,
    utils::verify_module_with_err,
};

pub struct OptProvider(ModulePipeline);

impl OptProvider {
    pub fn from_params(params: &[&str]) -> Result<Self> {
        match params {
            [] => Ok(Self(opt::default_pipeline())),
            &[desc] => Ok(Self(opt::pipeline_from_desc(desc)?)),
            _ => bail!("invalid parameter count for opt provider"),
        }
    }
}

impl TestProvider for OptProvider {
    type AuxOutput = Module;

    fn env(&self) -> FxHashMap<String, Value<'_>> {
        [("val".to_owned(), Value::Regex(VAL_REGEX.into()))]
            .into_iter()
            .collect()
    }

    fn output_for(&self, mut module: Module) -> Result<(String, Module)> {
        self.0.run(&mut module);
        verify_module_with_err(&module, "transformed module invalid")?;
        Ok((module.to_string(), module))
    }

    fn update(&self, updater: &mut Updater<'_>, output_str: &str, module: Module) -> Result<()> {
        update_transformed_module_output(updater, &module, output_str)
    }
}

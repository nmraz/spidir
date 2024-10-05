use core::fmt::Write;

use anyhow::{anyhow, Result};
use codegen::{api::lower_func, target::x64::X64Machine};
use ir::module::Module;

use crate::{
    provider::{update_per_func_output, TestProvider, Updater},
    utils::sanitize_raw_output,
};

pub struct IselProvider {
    machine: X64Machine,
}

impl IselProvider {
    pub fn new(machine: X64Machine) -> Self {
        Self { machine }
    }
}

impl TestProvider for IselProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            writeln!(output, "function `{}`:", func.metadata.name).unwrap();

            let (cfg_ctx, lir) = lower_func(module, func, &self.machine).map_err(|e| {
                anyhow!(
                    "isel failed for `{}`: {}",
                    func.metadata.name,
                    e.display(module, &func.body)
                )
            })?;

            write!(
                output,
                "{}",
                lir.display(&cfg_ctx.cfg, &cfg_ctx.block_order)
            )
            .unwrap();
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, output_str: &str) -> Result<()> {
        update_per_func_output(updater, &sanitize_raw_output(output_str))
    }
}

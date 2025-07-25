use core::fmt::Write;

use anyhow::{Result, anyhow};
use codegen::{api::lower_func, target::x64::X64Machine};
use ir::module::Module;

use crate::{
    provider::{SimpleTestProvider, Updater, update_per_func_output},
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

impl SimpleTestProvider for IselProvider {
    fn output_for(&self, module: Module) -> Result<String> {
        let mut output = String::new();

        for func in module.iter_function_borrows() {
            writeln!(output, "function `{}`:", func.metadata.name).unwrap();

            let (cfg_ctx, lir) =
                lower_func(&module.metadata, func, &self.machine).map_err(|e| {
                    anyhow!(
                        "isel failed for `{}`: {}",
                        func.metadata.name,
                        e.display(&module.metadata, func.body)
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

    fn update(&self, updater: &mut Updater<'_>, output_str: &str) -> Result<()> {
        update_per_func_output(updater, &sanitize_raw_output(output_str))
    }
}

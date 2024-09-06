use core::fmt::Write;

use anyhow::{anyhow, Result};
use codegen::{api::lower_func, target::x86_64::X64Machine};
use ir::{module::Module, write::display_node};

use crate::utils::sanitize_raw_output;

use super::{update_per_func_output, TestProvider, Updater};

pub struct IselProvider;

impl TestProvider for IselProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            writeln!(output, "function `{}`:", func.name).unwrap();

            let (cfg_ctx, lir) = lower_func(module, func, &X64Machine).map_err(|err| {
                anyhow!(
                    "failed to select `{}`: `{}`",
                    func.name,
                    display_node(module, &func.graph, err.node)
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

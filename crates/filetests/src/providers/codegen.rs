use std::fmt::Write;

use anyhow::{anyhow, Result};
use codegen::{
    api::{codegen_func, CodegenOpts},
    target::x64::X64Machine,
};
use codegen_test_tools::disasm::disasm_code;
use ir::module::Module;

use crate::{
    provider::{update_per_func_output, TestProvider, Updater},
    utils::sanitize_raw_output,
};

pub struct CodegenProvider {
    machine: X64Machine,
}

impl CodegenProvider {
    pub fn new(machine: X64Machine) -> Self {
        Self { machine }
    }
}

impl TestProvider for CodegenProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            writeln!(output, "function `{}`:", func.metadata.name).unwrap();

            let code = codegen_func(module, func, &self.machine, &CodegenOpts::default())
                .map_err(|err| anyhow!("{}", err.display(module, func)))?;
            disasm_code(module, &code, 0, &mut output)?;
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, output_str: &str) -> Result<()> {
        update_per_func_output(updater, &sanitize_raw_output(output_str))
    }
}

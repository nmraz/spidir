use std::fmt::Write;

use anyhow::{Result, anyhow};
use codegen::{
    api::{CodegenOpts, codegen_func},
    target::x64::X64Machine,
};
use codegen_test_tools::disasm::disasm_code;
use ir::module::Module;

use crate::{
    provider::{SimpleTestProvider, Updater, update_per_func_output},
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

impl SimpleTestProvider for CodegenProvider {
    fn output_for(&self, module: Module) -> Result<String> {
        let mut output = String::new();

        for func in module.metadata.functions().keys() {
            let func = module.borrow_function(func);

            writeln!(output, "function `{}`:", func.metadata.name).unwrap();

            let code = codegen_func(
                &module.metadata,
                func,
                &self.machine,
                &CodegenOpts::default(),
            )
            .map_err(|err| anyhow!("{}", err.display(&module.metadata, func)))?;
            disasm_code(&module, &code, 0, &mut output)?;
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, output_str: &str) -> Result<()> {
        update_per_func_output(updater, &sanitize_raw_output(output_str))
    }
}

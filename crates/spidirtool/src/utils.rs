use std::{fs, path::Path};

use anyhow::{anyhow, bail, Context, Result};
use ir::{
    module::{Function, FunctionData, Module},
    verify::verify_module,
};
use parser::parse_module;

pub fn read_and_verify_module(input_file: &Path) -> Result<Module> {
    let module = read_module(input_file)?;
    if let Err(errors) = verify_module(&module) {
        for error in errors {
            eprintln!("error: {}", error.display_with_context(&module));
        }
        bail!("module contained errors")
    };
    Ok(module)
}

pub fn read_module(input_file: &Path) -> Result<Module> {
    let input_data = fs::read_to_string(input_file).context("failed to read input file")?;
    let module = parse_module(&input_data).context("error parsing module")?;
    Ok(module)
}

pub fn function_by_name<'a>(
    module: &'a Module,
    name: &str,
) -> Result<(Function, &'a FunctionData)> {
    module
        .functions
        .iter()
        .find(|(_func, data)| data.name == name)
        .ok_or_else(|| anyhow!("function `{}` not found in module", name))
}

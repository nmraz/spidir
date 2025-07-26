use std::{fs, path::Path};

use anyhow::{Context, Result, anyhow, bail};
use ir::{
    function::FunctionBorrow,
    module::{Function, Module},
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
) -> Result<(Function, FunctionBorrow<'a>)> {
    let (func, _) = module
        .metadata
        .functions()
        .iter()
        .find(|(_func, metadata)| metadata.name == name)
        .ok_or_else(|| anyhow!("function `{}` not found in module", name))?;
    Ok((func, module.borrow_function(func)))
}

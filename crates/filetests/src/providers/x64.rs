use anyhow::{bail, Result};
use codegen::target::x64::{CodeModel, X64Machine, X64MachineConfig};

pub fn create_x64_machine(params: &[&str]) -> Result<X64Machine> {
    match params {
        [] => Ok(X64Machine::default()),
        [code_model] => {
            let code_model = parse_code_model(code_model)?;
            Ok(X64Machine::new(X64MachineConfig {
                internal_code_model: code_model,
                extern_code_model: code_model,
            }))
        }
        [internal_code_model, extern_code_model] => Ok(X64Machine::new(X64MachineConfig {
            internal_code_model: parse_code_model(internal_code_model)?,
            extern_code_model: parse_code_model(extern_code_model)?,
        })),
        _ => bail!("invalid parameter count for x64 backend provider"),
    }
}

fn parse_code_model(code_model: &str) -> Result<CodeModel> {
    match code_model {
        "small-pic" => Ok(CodeModel::SmallPic),
        "large-abs" => Ok(CodeModel::LargeAbs),
        _ => bail!("unknown code model '{code_model}'"),
    }
}

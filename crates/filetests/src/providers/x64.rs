use anyhow::{Ok, Result, bail, ensure};
use codegen::target::x64::{CodeModel, X64CpuFeatures, X64Machine, X64MachineConfig};

pub fn create_x64_machine(params: &[&str]) -> Result<X64Machine> {
    match params {
        [] => Ok(X64Machine::default()),
        [code_models] => {
            let (internal_code_model, extern_code_model) = parse_code_models(code_models)?;
            Ok(X64Machine::new(X64MachineConfig {
                internal_code_model,
                extern_code_model,
                ..Default::default()
            }))
        }
        [code_models, cpu_features] => {
            let (internal_code_model, extern_code_model) = parse_code_models(code_models)?;
            let cpu_features = parse_cpu_features(cpu_features)?;
            Ok(X64Machine::new(X64MachineConfig {
                internal_code_model,
                extern_code_model,
                cpu_features,
            }))
        }
        _ => bail!("invalid parameter count for x64 backend provider"),
    }
}

fn parse_code_models(code_models: &str) -> Result<(CodeModel, CodeModel)> {
    if code_models.starts_with('(') {
        ensure!(code_models.ends_with(')'), "invalid code model description");
        let code_models = code_models[1..code_models.len() - 1]
            .split(',')
            .collect::<Vec<_>>();
        let &[internal_code_model, extern_code_model] = &code_models[..] else {
            bail!("expected two code models in parenthesized description")
        };
        return Ok((
            parse_code_model(internal_code_model.trim())?,
            parse_code_model(extern_code_model.trim())?,
        ));
    }

    let code_model = parse_code_model(code_models)?;
    Ok((code_model, code_model))
}

fn parse_code_model(code_model: &str) -> Result<CodeModel> {
    match code_model {
        "small-pic" => Ok(CodeModel::SmallPic),
        "large-abs" => Ok(CodeModel::LargeAbs),
        _ => bail!("unknown code model '{code_model}'"),
    }
}

fn parse_cpu_features(cpu_features: &str) -> Result<X64CpuFeatures> {
    ensure!(
        cpu_features.starts_with('{') && cpu_features.ends_with('}'),
        "invalid CPU feature description"
    );

    let mut features = X64CpuFeatures::default();

    let cpu_features = &cpu_features[1..cpu_features.len() - 1];
    for feature in cpu_features.split(',') {
        let feature = feature.trim();
        match feature {
            "popcnt" => {
                features.popcnt = true;
            }
            _ => bail!("unknown CPU feature '{feature}'"),
        }
    }

    Ok(features)
}

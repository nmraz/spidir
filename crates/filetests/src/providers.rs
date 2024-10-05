use anyhow::{bail, ensure, Result};
use isel_regalloc::IselRegallocProvider;

use crate::provider::TestProvider;

use self::{
    canonicalize::CanonicalizeProvider,
    cfg::CfgProvider,
    codegen::CodegenProvider,
    domtree::DomTreeProvider,
    graphviz::GraphvizTestProvider,
    isel::IselProvider,
    loops::LoopForestProvider,
    schedule::ScheduleProvider,
    verify::{VerifyErrProvider, VerifyOkProvider},
    x64::create_x64_machine,
};

mod canonicalize;
mod cfg;
mod codegen;
mod domtree;
mod graphviz;
mod isel;
mod isel_regalloc;
mod loops;
mod schedule;
mod verify;
mod x64;

pub fn select_test_provider(run_command: &str) -> Result<Box<dyn TestProvider>> {
    let (command, params) = parse_run_command(run_command)?;

    match command {
        "cfg" => Ok(Box::new(CfgProvider)),
        "domtree" => Ok(Box::new(DomTreeProvider)),
        "graphviz" => Ok(Box::new(GraphvizTestProvider::new(&params)?)),
        "isel" => Ok(Box::new(IselProvider::new(create_x64_machine(&params)?))),
        "isel-regalloc" => Ok(Box::new(IselRegallocProvider::from_params(&params)?)),
        "canonicalize" => Ok(Box::new(CanonicalizeProvider)),
        "codegen" => Ok(Box::new(CodegenProvider::new(create_x64_machine(&params)?))),
        "loop-forest" => Ok(Box::new(LoopForestProvider)),
        "schedule" => Ok(Box::new(ScheduleProvider)),
        "verify-err" => Ok(Box::new(VerifyErrProvider)),
        "verify-ok" => Ok(Box::new(VerifyOkProvider)),
        _ => bail!("unknown run command '{run_command}'"),
    }
}

fn parse_run_command(run_command: &str) -> Result<(&str, Vec<&str>)> {
    let Some(base_end) = run_command.find('[') else {
        // Simple case: no parameters.
        return Ok((run_command, vec![]));
    };

    let (command, params) = run_command.split_at(base_end);
    ensure!(
        params.find(']') == Some(params.len() - 1),
        "invalid provider parameter string"
    );
    let params = params[1..params.len() - 1]
        .split(',')
        .map(|param| param.trim())
        .collect();

    Ok((command, params))
}

use anyhow::{Result, bail, ensure};
use isel_regalloc::IselRegallocProvider;

use crate::{provider::DynTestProvider, providers::opt::OptProvider};

use self::{
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

mod cfg;
mod codegen;
mod domtree;
mod graphviz;
mod isel;
mod isel_regalloc;
mod loops;
mod opt;
mod schedule;
mod verify;
mod x64;

pub fn select_test_provider(run_command: &str) -> Result<Box<dyn DynTestProvider>> {
    let (command, params) = parse_run_command(run_command)?;

    match command {
        "cfg" => Ok(Box::new(CfgProvider)),
        "domtree" => Ok(Box::new(DomTreeProvider)),
        "graphviz" => Ok(Box::new(GraphvizTestProvider::new(&params)?)),
        "isel" => Ok(Box::new(IselProvider::new(create_x64_machine(&params)?))),
        "isel-regalloc" => Ok(Box::new(IselRegallocProvider::from_params(&params)?)),
        "opt" => Ok(Box::new(OptProvider::from_params(&params)?)),
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
    ensure!(params.ends_with(']'), "invalid provider parameter string");
    Ok((command, parse_params(&params[1..params.len() - 1])))
}

fn parse_params(param_str: &str) -> Vec<&str> {
    let mut params = Vec::new();

    // Use simple depth counters to avoid splitting on commas in nested parenthesizations. We don't
    // actually check that the nesting is balanced or well-formed.

    let mut depth_round: usize = 0;
    let mut depth_square: usize = 0;
    let mut depth_curly: usize = 0;

    let mut cur_param_start = 0;

    for (i, c) in param_str.char_indices() {
        match c {
            '(' => depth_round += 1,
            ')' => depth_round = depth_round.saturating_sub(1),
            '[' => depth_square += 1,
            ']' => depth_square = depth_square.saturating_sub(1),
            '{' => depth_curly += 1,
            '}' => depth_curly = depth_curly.saturating_sub(1),
            ',' if depth_round == 0 && depth_square == 0 && depth_curly == 0 => {
                params.push(param_str[cur_param_start..i].trim());
                cur_param_start = i + 1;
            }
            _ => {}
        }
    }

    // Handle the last parameter, if one exists.
    if !param_str.is_empty() {
        params.push(param_str[cur_param_start..].trim());
    }

    params
}

use core::iter;

use anyhow::{anyhow, bail, Context, Ok, Result};
use filecheck::{Checker, CheckerBuilder, NO_VARIABLES};
use ir::module::Module;
use parser::parse_module;

use provider::{select_test_provider, TestProvider};
use utils::find_comment_start;

use crate::{
    provider::Updater,
    utils::{find_run_line, parse_run_line},
};

#[macro_use]
mod utils;
mod provider;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpdateMode {
    Never,
    IfFailed,
    Always,
}

#[derive(Debug, Clone)]
pub enum TestOutcome {
    Ok,
    Update(String),
}

pub fn select_test_provider_from_input(input: &str) -> Result<Box<dyn TestProvider>> {
    let (_, run_command) =
        find_run_line(input.lines()).ok_or_else(|| anyhow!("no run line in test file"))?;
    select_test_provider(run_command)
}

pub fn run_test(
    provider: &dyn TestProvider,
    input: &str,
    update_mode: UpdateMode,
) -> Result<TestOutcome> {
    let module = parse_module(input).context("failed to parse module")?;
    let output = provider.output_for(&module)?;

    match update_mode {
        UpdateMode::Never => {
            run_test_checks(input, &output, true)?;
            Ok(TestOutcome::Ok)
        }
        UpdateMode::IfFailed => {
            if run_test_checks(input, &output, false).is_err() {
                return Ok(TestOutcome::Update(compute_update(
                    provider, input, &module, &output,
                )?));
            }
            Ok(TestOutcome::Ok)
        }
        UpdateMode::Always => Ok(TestOutcome::Update(compute_update(
            provider, input, &module, &output,
        )?)),
    }
}

fn run_test_checks(input: &str, output: &str, explain: bool) -> Result<()> {
    let checker = build_checker(input)?;
    let ok = checker
        .check(output, NO_VARIABLES)
        .context("filecheck failed")?;

    if !ok {
        if explain {
            let (_, explanation) = checker
                .explain(output, NO_VARIABLES)
                .context("filecheck explain failed")?;
            bail!("filecheck failed:\n{explanation}");
        } else {
            bail!("filecheck failed");
        }
    }

    Ok(())
}

fn compute_update(
    provider: &dyn TestProvider,
    input: &str,
    module: &Module,
    output: &str,
) -> Result<String> {
    let lines = get_non_directive_lines(input);
    let mut updater = Updater::new(&lines);
    updater
        .advance_to_after(|line| parse_run_line(line).is_some())
        .context("failed to find run line")?;
    provider
        .update(&mut updater, module, output)
        .context("failed to update file directives")?;
    Ok(updater.output())
}

fn build_checker(input: &str) -> Result<Checker> {
    let mut builder = CheckerBuilder::new();
    for (i, line) in input.lines().enumerate() {
        if let Some(comment_start) = find_comment_start(line) {
            let comment = line[comment_start + 1..].trim();
            builder
                .directive(comment)
                .with_context(|| format!("invalid filecheck directive on line {i}"))?;
        }
    }
    let checker = builder.finish();
    if checker.is_empty() {
        bail!("no filecheck directives found in input file");
    }
    Ok(checker)
}

fn get_non_directive_lines(input: &str) -> Vec<&str> {
    let mut other_lines = Vec::new();
    let mut builder = CheckerBuilder::new();
    let mut empty_line_run = 0;
    let mut post_directive_only = false;

    for line in input.lines() {
        if line.trim().is_empty() {
            if !post_directive_only {
                // Record any empty lines that don't follow a directive-only line so we can add them
                // back in later.
                empty_line_run += 1;
            }
            continue;
        }

        if let Some(comment_start) = find_comment_start(line) {
            let comment = line[comment_start + 1..].trim();

            let was_directive = builder.directive(comment).unwrap_or(true);

            let pre_line = if was_directive {
                line[..comment_start].trim()
            } else {
                line
            };

            if pre_line.is_empty() {
                // This was a directive-only line, so delete any empty lines that came before it.
                post_directive_only = was_directive;
            } else {
                other_lines.extend(iter::repeat("").take(empty_line_run));
                other_lines.push(pre_line);
                post_directive_only = false;
            }
        } else {
            other_lines.extend(iter::repeat("").take(empty_line_run));
            other_lines.push(line);
            post_directive_only = false;
        }

        empty_line_run = 0;
    }

    other_lines
}

#[cfg(test)]
mod tests;

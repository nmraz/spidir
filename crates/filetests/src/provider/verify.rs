use std::{fmt::Write, sync::OnceLock};

use anyhow::{anyhow, bail, Result};
use cranelift_entity::SecondaryMap;
use filecheck::Value;
use fx_utils::FxHashMap;
use ir::{
    module::Module,
    verify::{verify_module, ModuleVerifierError},
    write::display_node,
};
use itertools::Itertools;
use regex::Regex;

use crate::{regexes::VAL_REGEX, utils::parse_output_func_heading};

use super::{TestProvider, Updater};

pub struct VerifyOkProvider;
impl TestProvider for VerifyOkProvider {
    fn expects_valid_module(&self) -> bool {
        false
    }

    fn output_for(&self, module: &Module) -> Result<String> {
        verify_module(module).map_err(|errs| {
            let errs = errs
                .iter()
                .format_with("\n", |err, f| f(&err.display_with_context(module)));
            anyhow!("verification failed:\n{errs}")
        })?;
        Ok("".to_owned())
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, _output_str: &str) -> Result<()> {
        updater.blank_line();
        updater.directive(0, "check", "$()");
        Ok(())
    }
}

pub struct VerifyErrProvider;
impl TestProvider for VerifyErrProvider {
    fn expects_valid_module(&self) -> bool {
        false
    }

    fn env(&self) -> FxHashMap<String, Value<'_>> {
        [("val".to_owned(), Value::Regex(VAL_REGEX.into()))]
            .into_iter()
            .collect()
    }

    fn output_for(&self, module: &Module) -> Result<String> {
        let Err(errors) = verify_module(module) else {
            bail!("module contained no verifier errors");
        };

        let mut global_errors = Vec::new();
        let mut errors_by_function: SecondaryMap<_, Vec<_>> = SecondaryMap::new();

        for error in errors {
            match error {
                ModuleVerifierError::Func { function, error } => {
                    errors_by_function[function].push(error);
                }
                error => global_errors.push(error),
            }
        }

        let mut output = String::new();
        writeln!(output, "global:").unwrap();
        if !global_errors.is_empty() {
            for error in global_errors {
                writeln!(output, "{}", error.display(module)).unwrap();
            }
        }
        writeln!(output).unwrap();

        for (func, func_data) in &module.functions {
            writeln!(output, "function `{}`:", func_data.metadata.name).unwrap();
            if let Some(errors) = errors_by_function.get(func) {
                let body = &func_data.body;
                let graph = &body.graph;
                for error in errors {
                    writeln!(
                        output,
                        "`{}`: {}",
                        display_node(module, body, error.node(graph)),
                        error.display(graph)
                    )
                    .unwrap();
                }
            }
            writeln!(output).unwrap();
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, output_str: &str) -> Result<()> {
        let mut output_lines = output_str.lines();
        assert!(output_lines.next().unwrap() == "global:");

        let val_regex = regex!(VAL_REGEX);

        let mut cur_func = None;
        let mut output_run = Vec::new();

        for output_line in output_lines {
            if output_line.is_empty() {
                continue;
            }

            if let Some(new_func) = parse_output_func_heading(output_line) {
                // Add the lines we've gathered up to this point before moving on to the new
                // function.
                add_error_run(updater, cur_func, &mut output_run);
                updater.advance_to_function(new_func)?;
                cur_func = Some(new_func);
            } else {
                output_run.push(val_regex.replace_all(output_line, "$$val").into_owned());
            }
        }

        // Add in the line run for the last function.
        add_error_run(updater, cur_func, &mut output_run);

        Ok(())
    }
}

fn add_error_run(updater: &mut Updater<'_>, cur_func: Option<&str>, output_run: &mut Vec<String>) {
    if output_run.is_empty() {
        // If there aren't any errors to add, don't even insert the heading.
        return;
    }

    let indent = if cur_func.is_some() { 4 } else { 0 };

    if let Some(cur_func) = cur_func {
        updater.directive(4, "check", &format!("function `{cur_func}`:"));
    } else {
        updater.blank_line();
        updater.directive(0, "check", "global:");
    }

    for line in output_run.drain(..) {
        updater.directive(indent, "unordered", &line);
    }

    updater.blank_line();
}

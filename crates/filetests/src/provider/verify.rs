use std::{fmt::Write, sync::OnceLock};

use anyhow::{anyhow, bail, Result};
use cranelift_entity::SecondaryMap;
use ir::{
    module::Module,
    valgraph::{Node, ValGraph},
    verify::{verify_module, ModuleVerifierError},
    write::write_node,
};
use itertools::Itertools;
use regex::Regex;

use super::{TestProvider, Updater};

pub struct VerifyOkProvider;
impl TestProvider for VerifyOkProvider {
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
        updater.blank_line();
        Ok(())
    }
}

pub struct VerifyErrProvider;
impl TestProvider for VerifyErrProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let Err(errors) = verify_module(module) else {
            bail!("module contained no verifier errors");
        };

        let mut global_errors = Vec::new();
        let mut errors_by_function: SecondaryMap<_, Vec<_>> = SecondaryMap::new();

        for error in errors {
            match error {
                ModuleVerifierError::Graph { function, error } => {
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
            writeln!(output, "function `{}`:", func_data.name).unwrap();
            if let Some(errors) = errors_by_function.get(func) {
                let graph = &func_data.graph;
                for error in errors {
                    writeln!(
                        output,
                        "`{}`: {}",
                        display_node(module, graph, error.node(graph)),
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
        updater.directive(0, "regex", r"val=%\d+");

        let mut output_lines = output_str.lines();
        assert!(output_lines.next().unwrap() == "global:");

        let func_regex = regex!(r#"^function `(.+)`:"#);
        let val_regex = regex!(r"%\d+");

        let mut in_func = false;
        let mut output_run = Vec::new();

        for output_line in output_lines {
            if output_line.is_empty() {
                continue;
            }

            if let Some(new_func) = func_regex.captures(output_line) {
                // Add the lines we've gathered up to this point before moving on to the new
                // function.
                if !in_func && !output_run.is_empty() {
                    // There were global errors, add the `global:` label.
                    updater.blank_line();
                    updater.directive(0, "check", "global:");
                }
                add_line_run(updater, in_func, &mut output_run);

                let name = &new_func[1];
                updater.advance_to_after(|line| line.contains(&format!("func @{name}")))?;
                updater.directive(4, "check", &format!("function `{name}`:"));
                in_func = true;
            } else {
                output_run.push(val_regex.replace_all(output_line, "$$val").into_owned());
            }
        }

        // Add in the line run for the last function.
        add_line_run(updater, in_func, &mut output_run);

        Ok(())
    }
}

fn add_line_run(updater: &mut Updater<'_>, in_func: bool, output_run: &mut Vec<String>) {
    let indent = if in_func { 4 } else { 0 };
    for line in output_run.drain(..) {
        updater.directive(indent, "unordered", &line);
    }
    updater.blank_line();
}

fn display_node(module: &Module, graph: &ValGraph, node: Node) -> String {
    let mut s = String::new();
    write_node(&mut s, module, graph, node).unwrap();
    s
}

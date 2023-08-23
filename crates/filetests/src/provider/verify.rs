use std::{fmt::Write, sync::OnceLock};

use cranelift_entity::SecondaryMap;
use ir::{
    module::Module,
    valgraph::{Node, ValGraph},
    verify::{verify_module, ModuleVerifierError},
    write::write_node,
};
use regex::Regex;

use crate::utils::{find_run_line, insert_lines_after};

use super::TestProvider;

pub struct VerifyTest;
impl TestProvider for VerifyTest {
    fn output_for(&self, module: &Module) -> String {
        let Err(errors) = verify_module(module) else {
            return String::new();
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

        output
    }

    fn update(&self, _module: &Module, input_lines: &mut Vec<String>, output_str: &str) {
        let (run_line_pos, _) = find_run_line(input_lines.iter().map(|a| a.as_str())).unwrap();

        let prefix_lines = [r"# regex: val=%\d+", "", "# check: global:"];
        insert_lines_after(input_lines, run_line_pos, prefix_lines);

        let mut output_lines = output_str.lines();
        assert!(output_lines.next().unwrap() == "global:");

        let func_regex = regex!(r#"^function `(.+)`:"#);
        let val_regex = regex!(r"%\d+");

        let mut input_line = run_line_pos + prefix_lines.len();
        let mut in_func = false;
        let mut output_run: Vec<String> = Vec::new();

        for output_line in output_lines {
            if output_line.is_empty() {
                continue;
            }

            if let Some(new_func) = func_regex.captures(output_line) {
                insert_lines_after(input_lines, input_line, output_run.iter().map(|s| &**s));
                insert_lines_after(input_lines, input_line + output_run.len(), [""]);
                output_run.clear();

                let name = &new_func[1];
                input_line += input_lines[input_line..]
                    .iter()
                    .position(|line| line.contains(&format!("func @{name}(")))
                    .expect("function not found in source");

                insert_lines_after(
                    input_lines,
                    input_line,
                    [&*format!("    # check: function `{name}`:")],
                );
                input_line += 1;
                in_func = true;
            } else {
                output_run.push(format!(
                    "{:1$}# nextln: {2}",
                    "",
                    if in_func { 4 } else { 0 },
                    val_regex.replace_all(output_line, "$$val")
                ));
            }
        }

        insert_lines_after(input_lines, input_line, output_run.iter().map(|s| &**s));
        insert_lines_after(input_lines, input_line + output_run.len(), [""]);
    }
}

fn display_node(module: &Module, graph: &ValGraph, node: Node) -> String {
    let mut s = String::new();
    write_node(&mut s, module, graph, node).unwrap();
    s
}

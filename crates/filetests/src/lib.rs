use core::iter;
use std::{fmt::Write, sync::OnceLock};

use cranelift_entity::SecondaryMap;
use filecheck::{Checker, CheckerBuilder};
use ir::{
    module::Module,
    valgraph::{Node, ValGraph},
    verify::{verify_module, ModuleVerifierError},
    write::write_node,
};
use parser::parse_module;
use regex::Regex;

macro_rules! regex {
    ($val:literal) => {{
        static REGEX: OnceLock<Regex> = OnceLock::new();
        REGEX.get_or_init(|| Regex::new($val).unwrap())
    }};
}

pub trait TestProducer {
    fn output_for(&self, module: &Module) -> String;
    fn update(&self, module: &Module, input_lines: &mut Vec<String>, output_str: &str);
}

pub struct VerifyTest;
impl TestProducer for VerifyTest {
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

fn insert_lines_after<'a>(
    lines: &mut Vec<String>,
    i: usize,
    new_lines: impl IntoIterator<Item = &'a str>,
) {
    insert_lines_before(lines, i + 1, new_lines);
}

fn insert_lines_before<'a>(
    lines: &mut Vec<String>,
    i: usize,
    new_lines: impl IntoIterator<Item = &'a str>,
) {
    lines.splice(i..i, new_lines.into_iter().map(|line| line.to_owned()));
}

fn display_node(module: &Module, graph: &ValGraph, node: Node) -> String {
    let mut s = String::new();
    write_node(&mut s, module, graph, node).unwrap();
    s
}

pub fn select_test_producer(input: &str) -> Box<dyn TestProducer> {
    let (_, run_command) = find_run_line(input.lines()).expect("no run line in test file");
    match run_command {
        "verify" => Box::new(VerifyTest),
        _ => panic!("unknown run command '{run_command}'"),
    }
}

fn find_run_line<'a>(lines: impl Iterator<Item = &'a str>) -> Option<(usize, &'a str)> {
    let run_regex = regex!(r#"^run:\s*(.+)"#);

    lines.enumerate().find_map(|(i, line)| {
        let comment_start = find_comment_start(line)?;
        let comment = line[comment_start + 1..].trim();
        let captures = run_regex.captures(comment)?;
        Some((i, captures.get(1).unwrap().as_str()))
    })
}

pub fn run_test(producer: &dyn TestProducer, input: &str, update_if_failed: bool) {
    let (checker, mut lines) = build_checker_and_lines(input);
    let module = parse_module(input).expect("failed to parse module");
    let output = producer.output_for(&module);
    let (ok, explanation) = checker
        .explain(&output, filecheck::NO_VARIABLES)
        .expect("bad filecheck directive");
    if !ok {
        if update_if_failed {
            producer.update(&module, &mut lines, &output);
            eprintln!("{}", lines.join("\n"));
        } else {
            eprintln!("{}", explanation);
            panic!("checks failed");
        }
    }
}

fn build_checker_and_lines(input: &str) -> (Checker, Vec<String>) {
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

            let was_directive = builder.directive(comment).expect("invalid directive");

            let pre_line = if was_directive {
                line[..comment_start].trim()
            } else {
                line
            };

            if pre_line.is_empty() {
                // This was a directive-only line, so delete any empty lines that came before it.
                post_directive_only = was_directive;
            } else {
                other_lines.extend(iter::repeat(String::new()).take(empty_line_run));
                other_lines.push(pre_line.to_owned());
                post_directive_only = false;
            }
        } else {
            other_lines.extend(iter::repeat(String::new()).take(empty_line_run));
            other_lines.push(line.to_owned());
            post_directive_only = false;
        }

        empty_line_run = 0;
    }

    let checker = builder.finish();
    assert!(!checker.is_empty(), "no filecheck directives in input file");

    (checker, other_lines)
}

fn find_comment_start(line: &str) -> Option<usize> {
    line.find('#')
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use super::*;

    fn check_lines(input: &str, expected: Expect) {
        let (_, lines) = build_checker_and_lines(input);
        let lines = lines.join("\n");
        expected.assert_eq(&lines);
    }

    #[test]
    fn lines_empty() {
        check_lines(
            "
            # check: a


        ",
            expect![[""]],
        );
    }

    #[test]
    fn lines_simple() {
        check_lines(
            r"
# run: verify

# regex: val=%\d+

func @test(ptr, i32) {
    # check: function `test`:
    # nextln: `$val:ctrl, $val:i32, $val:ptr = entry`: bad value kind for output 1, expected one of `ptr`, got `i32`
    %0:ctrl, %2:i32, %1:ptr = entry

    # fsjlkdf
    return %0

}
            ",
            expect![[r#"

                            # run: verify
                            func @test(ptr, i32) {
                                %0:ctrl, %2:i32, %1:ptr = entry

                                # fsjlkdf
                                return %0

                            }"#]],
        );
    }

    #[test]
    fn lines_interleaved_directives() {
        check_lines(
            r"
# run: verify

# regex: val=%\d+

func @test(ptr, i32) {
    # check: function `test`:
    # a
    # nextln: `$val:ctrl, $val:i32, $val:ptr = entry`: bad value kind for output 1, expected one of `ptr`, got `i32`
    # b
    %0:ctrl, %2:i32, %1:ptr = entry

    # fsjlkdf
    return %0
}
            ",
            expect![[r#"

                            # run: verify
                            func @test(ptr, i32) {
                                # a
                                # b
                                %0:ctrl, %2:i32, %1:ptr = entry

                                # fsjlkdf
                                return %0
                            }"#]],
        );
    }

    #[test]
    fn lines_interleaved_whitespace() {
        check_lines(
            r"
# run: verify

# regex: val=%\d+

func @test(ptr, i32) {
    # check: function `test`:


    # nextln: `$val:ctrl, $val:i32, $val:ptr = entry`: bad value kind for output 1, expected one of `ptr`, got `i32`


    # nextln: sdjlkf
    %0:ctrl, %2:i32, %1:ptr = entry

    # fsjlkdf
    return %0

}
        ",
            expect![[r#"

                            # run: verify
                            func @test(ptr, i32) {
                                %0:ctrl, %2:i32, %1:ptr = entry

                                # fsjlkdf
                                return %0

                            }"#]],
        )
    }

    #[test]
    fn lines_leading_trailing_whitespace() {
        check_lines(
            r"
# run: verify

# regex: val=%\d+

func @test(ptr, i32) {


    # nextln: `$val:ctrl, $val:i32, $val:ptr = entry`: bad value kind for output 1, expected one of `ptr`, got `i32`


    %0:ctrl, %2:i32, %1:ptr = entry

    # fsjlkdf
    return %0

}
        ",
            expect![[r#"

                            # run: verify
                            func @test(ptr, i32) {
                                %0:ctrl, %2:i32, %1:ptr = entry

                                # fsjlkdf
                                return %0

                            }"#]],
        )
    }
}

use core::iter;

use filecheck::{Checker, CheckerBuilder};
use parser::parse_module;

use provider::{select_test_provider, TestProvider};
use utils::find_comment_start;

use crate::utils::find_run_line;

#[macro_use]
mod utils;
mod provider;

pub fn select_test_provider_from_input(input: &str) -> Box<dyn TestProvider> {
    let (_, run_command) = find_run_line(input.lines()).expect("no run line in test file");
    select_test_provider(run_command)
}

pub fn run_test(provider: &dyn TestProvider, input: &str, update_if_failed: bool) {
    let (checker, mut lines) = build_checker_and_lines(input);
    let module = parse_module(input).expect("failed to parse module");
    let output = provider.output_for(&module);
    let (ok, explanation) = checker
        .explain(&output, filecheck::NO_VARIABLES)
        .expect("bad filecheck directive");
    if !ok {
        if update_if_failed {
            provider.update(&module, &mut lines, &output);
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

#[cfg(test)]
mod tests;

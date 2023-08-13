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

pub trait TestProducer {
    fn output_for(&self, module: &Module) -> String;
    fn update(&self, module: &Module, input_str: &str, output_str: &str);
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
        if !global_errors.is_empty() {
            writeln!(output, "global:").unwrap();
            for error in global_errors {
                writeln!(output, "{}", error.display(module)).unwrap();
            }
            writeln!(output).unwrap();
        }

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
                writeln!(output).unwrap();
            }
        }

        output
    }

    fn update(&self, _module: &Module, _input_str: &str, _output_str: &str) {
        todo!()
    }
}

pub fn select_test_producer(input: &str) -> Box<dyn TestProducer> {
    let run_regex = {
        static RUN_REGEX: OnceLock<Regex> = OnceLock::new();
        RUN_REGEX.get_or_init(|| Regex::new(r#"^run:\s*(.+)"#).unwrap())
    };

    let run_command = &comments(input)
        .find_map(|comment| run_regex.captures(comment))
        .expect("no run line in test file")[1];

    match run_command {
        "verify" => Box::new(VerifyTest),
        _ => panic!("unknown run command '{run_command}'"),
    }
}

pub fn run_test(producer: &dyn TestProducer, input: &str) {
    let checker = build_checker(input);
    let module = parse_module(input).expect("failed to parse module");
    let output = producer.output_for(&module);
    let (ok, explanation) = checker
        .explain(&output, filecheck::NO_VARIABLES)
        .expect("bad filecheck directive");
    if !ok {
        eprintln!("{}", explanation);
        panic!("checks failed");
    }
}

fn display_node(module: &Module, graph: &ValGraph, node: Node) -> String {
    let mut s = String::new();
    write_node(&mut s, module, graph, node).unwrap();
    s
}

fn build_checker(input: &str) -> Checker {
    let mut builder = CheckerBuilder::new();
    for comment in comments(input) {
        builder.directive(comment).expect("invalid directive");
    }
    let checker = builder.finish();
    assert!(!checker.is_empty(), "no filecheck directives in input file");
    checker
}

fn comments(input: &str) -> impl Iterator<Item = &str> {
    input.lines().filter_map(|line| {
        line.find('#')
            .map(|comment_start| line[comment_start + 1..].trim())
    })
}

use std::fmt::Write;

use anyhow::{anyhow, bail, Ok, Result};
use filecheck::Value;
use fx_utils::FxHashMap;
use ir::{module::Module, write::quote_ident};

use crate::utils::parse_output_func_heading;

use self::{
    domtree::DomTreeProvider,
    graphviz::{AnnotatorKind, GraphvizTestProvider},
    loop_forest::LoopForestProvider,
    verify::{VerifyErrProvider, VerifyOkProvider},
};

mod domtree;
mod graphviz;
mod loop_forest;
mod verify;

pub trait TestProvider {
    fn env(&self) -> FxHashMap<String, Value<'_>> {
        FxHashMap::default()
    }

    fn output_for(&self, module: &Module) -> Result<String>;
    fn update(&self, updater: &mut Updater<'_>, module: &Module, output_str: &str) -> Result<()>;
}

pub fn select_test_provider(run_command: &str) -> Result<Box<dyn TestProvider>> {
    match run_command {
        "domtree" => Ok(Box::new(DomTreeProvider)),
        "graphviz[plain]" => Ok(Box::new(GraphvizTestProvider::new(AnnotatorKind::Plain))),
        "graphviz[colored]" => Ok(Box::new(GraphvizTestProvider::new(AnnotatorKind::Colored))),
        "graphviz[verify]" => Ok(Box::new(GraphvizTestProvider::new(AnnotatorKind::Error))),
        "loop-forest" => Ok(Box::new(LoopForestProvider)),
        "verify-err" => Ok(Box::new(VerifyErrProvider)),
        "verify-ok" => Ok(Box::new(VerifyOkProvider)),
        _ => bail!("unknown run command '{run_command}'"),
    }
}

fn update_per_func_output(updater: &mut Updater<'_>, output_str: &str) -> Result<()> {
    let mut in_func = false;

    for output_line in output_str.lines() {
        if output_line.is_empty() {
            continue;
        }
        if let Some(new_func) = parse_output_func_heading(output_line) {
            if in_func {
                updater.blank_line();
            }
            updater.advance_to_function(new_func)?;
            updater.directive(4, "check", output_line);
            in_func = true;
        } else {
            updater.directive(4, "nextln", output_line);
        }
    }

    updater.blank_line();

    Ok(())
}

pub struct Updater<'a> {
    lines: &'a [&'a str],
    new_lines: FxHashMap<usize, Vec<String>>,
    insertion_point: usize,
}

impl<'a> Updater<'a> {
    pub fn new(lines: &'a [&'a str]) -> Self {
        Self {
            lines,
            new_lines: FxHashMap::default(),
            insertion_point: 0,
        }
    }

    pub fn output(&self) -> String {
        let mut output = String::new();
        for (i, line) in self.lines.iter().enumerate() {
            self.append_new_lines(&mut output, i);
            writeln!(output, "{line}").unwrap();
        }
        self.append_new_lines(&mut output, self.lines.len());
        output
    }

    pub fn advance_to_function(&mut self, name: &str) -> Result<()> {
        self.advance_to_after(|line| {
            line.trim()
                .starts_with(&format!("func @{}", quote_ident(name)))
        })
    }

    pub fn advance_to_before(&mut self, mut pred: impl FnMut(&str) -> bool) -> Result<()> {
        self.insertion_point += self.lines[self.insertion_point..]
            .iter()
            .position(|line| pred(line))
            .ok_or_else(|| anyhow!("could not find line to advance to"))?;
        Ok(())
    }

    pub fn advance_to_after(&mut self, pred: impl FnMut(&str) -> bool) -> Result<()> {
        self.advance_to_before(pred)?;
        self.insertion_point += 1;
        Ok(())
    }

    pub fn directive(&mut self, indent: usize, name: &str, contents: &str) {
        self.insert(format!("{:indent$}# {name}: {contents}", ""))
    }

    pub fn blank_line(&mut self) {
        self.insert("".to_owned())
    }

    fn insert(&mut self, line: String) {
        self.new_lines
            .entry(self.insertion_point)
            .or_default()
            .push(line);
    }

    fn append_new_lines(&self, output: &mut String, line_num: usize) {
        if let Some(new_lines) = self.new_lines.get(&line_num) {
            for new_line in new_lines {
                writeln!(output, "{new_line}").unwrap();
            }
        }
    }
}

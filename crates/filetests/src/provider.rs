use std::{borrow::Cow, fmt::Write, iter};

use anyhow::{Context, Result, anyhow, bail};
use filecheck::{Checker, CheckerBuilder, Value, VariableMap};

use fx_utils::FxHashMap;
use ir::{function::FunctionBody, module::Module, write::quote_ident};
use parser::parse_module;

use crate::utils::{
    borrow_func_by_name, find_comment_start, generalize_module_value_names,
    parse_module_func_start, parse_output_func_heading, parse_run_line, verify_module_with_err,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpdateMode {
    Never,
    IfFailed,
    Always,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestOutcome<U> {
    Ok,
    Update(U),
}

pub trait TestProvider {
    type AuxOutput;

    fn expects_valid_module(&self) -> bool {
        true
    }

    fn env(&self) -> FxHashMap<String, Value<'_>> {
        FxHashMap::default()
    }

    fn output_for(&self, module: Module) -> Result<(String, Self::AuxOutput)>;
    fn update(
        &self,
        updater: &mut Updater<'_>,
        output_str: &str,
        aux: Self::AuxOutput,
    ) -> Result<()>;
}

pub trait SimpleTestProvider {
    fn expects_valid_module(&self) -> bool {
        true
    }

    fn env(&self) -> FxHashMap<String, Value<'_>> {
        FxHashMap::default()
    }

    fn output_for(&self, module: Module) -> Result<String>;
    fn update(&self, updater: &mut Updater<'_>, output_str: &str) -> Result<()>;
}

impl<P: SimpleTestProvider> TestProvider for P {
    type AuxOutput = ();

    fn expects_valid_module(&self) -> bool {
        SimpleTestProvider::expects_valid_module(self)
    }

    fn env(&self) -> FxHashMap<String, Value<'_>> {
        SimpleTestProvider::env(self)
    }

    fn output_for(&self, module: Module) -> Result<(String, ())> {
        SimpleTestProvider::output_for(self, module).map(|output| (output, ()))
    }

    fn update(&self, updater: &mut Updater<'_>, output_str: &str, _aux: ()) -> Result<()> {
        SimpleTestProvider::update(self, updater, output_str)
    }
}

pub trait DynTestProvider {
    fn run(&self, input: &str, update_mode: UpdateMode) -> Result<TestOutcome<String>>;
}

impl<P: TestProvider> DynTestProvider for P {
    fn run(&self, input: &str, update_mode: UpdateMode) -> Result<TestOutcome<String>> {
        let module = parse_module(input).context("failed to parse module")?;
        if self.expects_valid_module() {
            verify_module_with_err(&module, "parsed module invalid")?;
        }
        let (output, aux) = self.output_for(module)?;

        match update_mode {
            UpdateMode::Never => {
                run_test_checks(self.env(), input, &output, true)?;
                Ok(TestOutcome::Ok)
            }
            UpdateMode::IfFailed => {
                if run_test_checks(self.env(), input, &output, false).is_err() {
                    return Ok(TestOutcome::Update(compute_update(
                        self, input, &output, aux,
                    )?));
                }
                Ok(TestOutcome::Ok)
            }
            UpdateMode::Always => Ok(TestOutcome::Update(compute_update(
                self, input, &output, aux,
            )?)),
        }
    }
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

pub fn update_per_func_output(updater: &mut Updater<'_>, output_str: &str) -> Result<()> {
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

pub fn update_transformed_module_output(
    updater: &mut Updater<'_>,
    module: &Module,
    module_str: &str,
) -> Result<()> {
    let output_str = generalize_module_value_names(module, module_str)?;

    let mut in_func = false;
    let mut ordered_lines = 0;

    for output_line in output_str.lines() {
        if output_line.is_empty() {
            continue;
        }

        if let Some(new_func) = parse_module_func_start(output_line) {
            updater.advance_to_function(&new_func)?;
            updater.directive(4, "check", &format!("  {output_line}"));
            in_func = true;

            // We want the entry node and all nodes with identity to be matched in order after the
            // opening brace, to avoid the matching getting confused by different nodes with the
            // same textual representation.
            ordered_lines =
                count_nodes_with_identity(borrow_func_by_name(module, &new_func).body) + 1;
            continue;
        } else if !in_func {
            continue;
        }

        if output_line.trim() == "}" {
            updater.directive(4, "nextln", &format!(" {output_line}"));
            updater.blank_line();
        } else if ordered_lines > 0 {
            updater.directive(4, "nextln", &format!(" {output_line}"));
            ordered_lines -= 1;
        } else {
            // We don't actually care about the order in which nodes are printed, just that they are
            // all attached correctly. This should make tests more resilient to minor reshuffles.
            updater.directive(4, "dag", &format!(r"    {output_line}"));
        }
    }

    Ok(())
}

fn count_nodes_with_identity(body: &FunctionBody) -> usize {
    body.compute_full_walk_info()
        .postorder(&body.graph)
        .filter(|&node| body.graph.node_kind(node).has_identity())
        .count()
}

struct HashMapEnv<'a>(FxHashMap<String, Value<'a>>);
impl VariableMap for HashMapEnv<'_> {
    fn lookup(&self, varname: &str) -> Option<Value<'_>> {
        self.0.get(varname).cloned()
    }
}

fn run_test_checks(
    env: FxHashMap<String, Value<'_>>,
    input: &str,
    output: &str,
    explain: bool,
) -> Result<()> {
    let checker = build_checker(input)?;
    let env = HashMapEnv(env);

    let ok = checker.check(output, &env).context("filecheck failed")?;

    if !ok {
        if explain {
            let (_, explanation) = checker
                .explain(output, &env)
                .context("filecheck explain failed")?;
            bail!("filecheck failed:\n{explanation}");
        } else {
            bail!("filecheck failed");
        }
    }

    Ok(())
}

fn compute_update<P: TestProvider>(
    provider: &P,
    input: &str,
    output: &str,
    aux: P::AuxOutput,
) -> Result<String> {
    let lines = get_non_directive_lines(input);
    let mut updater = Updater::new(&lines);
    updater
        .advance_to_after(|line| parse_run_line(line).is_some())
        .context("failed to find run line")?;
    provider
        .update(&mut updater, output, aux)
        .context("failed to update file directives")?;
    Ok(updater.output())
}

fn build_checker(input: &str) -> Result<Checker> {
    let mut builder = CheckerBuilder::new();
    for (line_no, line) in input.lines().enumerate() {
        if let Some(comment_start) = find_comment_start(line) {
            let comment = line[comment_start + 1..].trim();
            add_directive(&mut builder, comment)
                .with_context(|| format!("invalid filecheck directive on line {}", line_no + 1))?;
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

            let was_directive = add_directive(&mut builder, comment).unwrap_or(true);

            let pre_line = if was_directive {
                line[..comment_start].trim()
            } else {
                line
            };

            if pre_line.is_empty() {
                // This was a directive-only line, so delete any empty lines that came before it.
                post_directive_only = was_directive;
            } else {
                other_lines.extend(iter::repeat_n("", empty_line_run));
                other_lines.push(pre_line);
                post_directive_only = false;
            }
        } else {
            other_lines.extend(iter::repeat_n("", empty_line_run));
            other_lines.push(line);
            post_directive_only = false;
        }

        empty_line_run = 0;
    }

    other_lines
}

fn add_directive(builder: &mut CheckerBuilder, comment: &str) -> Result<bool> {
    let mut comment = Cow::Borrowed(comment);

    // Support "dag" as a shorthand for "unordered" that must end on a word boundary.
    if let Some(dag_directive) = comment.strip_prefix("dag:") {
        comment = Cow::Owned(format!(r"unordered:{dag_directive}$(=\b)"));
    }

    Ok(builder.directive(&comment)?)
}

#[cfg(test)]
mod tests;

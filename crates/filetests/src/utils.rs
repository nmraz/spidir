use core::fmt::{self, Write};

use anyhow::Result;
use fx_utils::FxHashMap;
use hashbrown::hash_map::Entry;
use ir::{
    module::{FunctionBody, FunctionData, Module},
    valgraph::{DepValue, Node},
    write::{write_annotated_body, write_annotated_node, write_node_kind, AnnotateGraph},
};
use regex::{Captures, Regex};
use std::{cmp, sync::OnceLock};

use crate::regexes::VAL_REGEX;

macro_rules! regex {
    ($val:expr) => {{
        static REGEX: OnceLock<Regex> = OnceLock::new();
        REGEX.get_or_init(|| Regex::new($val).unwrap())
    }};
}

pub fn write_body_with_trailing_comments(
    output: &mut String,
    module: &Module,
    func: &FunctionData,
    comment_fn: impl FnMut(&mut String, Node),
) {
    writeln!(output, "function `{}`:", func.metadata.name).unwrap();
    let mut comment_annotator = CommentAnnotator { comment_fn };

    write_annotated_body(output, &mut comment_annotator, module, &func.body, 0).unwrap();
}

struct CommentAnnotator<F> {
    comment_fn: F,
}

impl<F: FnMut(&mut String, Node)> AnnotateGraph<String> for CommentAnnotator<F> {
    fn write_node(
        &mut self,
        s: &mut String,
        module: &Module,
        body: &FunctionBody,
        node: Node,
    ) -> fmt::Result {
        const MIN_LINE_LIMIT: usize = 40;
        const TAB_WIDTH: usize = 8;

        let orig_len = s.len();
        write_annotated_node(s, self, module, body, node).unwrap();
        let node_line_len = s.len() - orig_len;
        let comment_start = round_up(cmp::max(MIN_LINE_LIMIT, node_line_len + 2), TAB_WIDTH);
        let pad_len = comment_start - node_line_len;

        write!(s, "{:pad_len$}# ", "").unwrap();
        (self.comment_fn)(s, node);

        Ok(())
    }
}

fn round_up(num: usize, divisor: usize) -> usize {
    ((num + divisor - 1) / divisor) * divisor
}

pub fn parse_output_func_heading(output_line: &str) -> Option<&str> {
    let func_regex = regex!(r"^function `(.+)`:$");
    func_regex
        .captures(output_line)
        .map(|caps| caps.get(1).unwrap().as_str())
}

pub fn find_run_line<'a>(lines: impl Iterator<Item = &'a str>) -> Option<(usize, &'a str)> {
    lines.enumerate().find_map(|(i, line)| {
        let line = parse_run_line(line)?;
        Some((i, line))
    })
}

pub fn parse_run_line(line: &str) -> Option<&str> {
    let run_regex = regex!(r"^run:\s*(.+)");
    let comment_start = find_comment_start(line)?;
    let comment = line[comment_start + 1..].trim();
    let captures = run_regex.captures(comment)?;
    Some(captures.get(1).unwrap().as_str())
}

pub fn find_comment_start(line: &str) -> Option<usize> {
    line.find('#')
}

pub fn sanitize_raw_output(output: &str) -> String {
    output.replace('$', "$$")
}

pub fn generalize_value_names(module: &Module, output_str: &str) -> Result<String> {
    let mut new_output = String::new();
    let mut cur_func = None;

    let mut name_counter = FxHashMap::default();
    let mut val_names = FxHashMap::<DepValue, String>::default();

    let val_regex = regex!(VAL_REGEX);

    for line in output_str.lines() {
        if let Some(new_func) = parse_output_func_heading(line) {
            cur_func = module
                .functions
                .values()
                .find(|func| func.metadata.name == new_func);
            name_counter.clear();
            val_names.clear();
            writeln!(new_output, "{line}").unwrap();
            continue;
        }

        let Some(cur_func) = cur_func else {
            writeln!(new_output, "{line}").unwrap();
            continue;
        };

        let line = val_regex.replace_all(line, |caps: &Captures<'_>| {
            let value = DepValue::from_u32(caps[1].parse().unwrap());
            match val_names.entry(value) {
                Entry::Occupied(existing_name) => format!("${}", existing_name.get()),
                Entry::Vacant(vacant_entry) => {
                    let name = get_value_var_name(module, &cur_func.body, &mut name_counter, value);
                    let replacement = format!("$({name}=$val)");
                    vacant_entry.insert(name);
                    replacement
                }
            }
        });

        writeln!(new_output, "{line}").unwrap();
    }

    Ok(new_output.to_owned())
}

fn get_value_var_name(
    module: &Module,
    body: &FunctionBody,
    name_counter: &mut FxHashMap<String, usize>,
    value: DepValue,
) -> String {
    let node_kind = body.graph.node_kind(body.graph.value_def(value).0);
    let mut node_string = String::new();
    write_node_kind(&mut node_string, module, body, node_kind).unwrap();
    let name_prefix = node_string.split(&[' ', '.']).next().unwrap();
    let counter = name_counter.entry(name_prefix.to_owned()).or_default();
    let name = format!("{name_prefix}{counter}");
    *counter += 1;
    name
}

use core::fmt::{self, Write};

use anyhow::Result;
use ir::{
    module::{FunctionData, Module},
    valgraph::{Node, ValGraph},
    write::{write_annotated_graph, write_annotated_node, AnnotateGraph},
};
use regex::Regex;
use std::sync::OnceLock;

macro_rules! regex {
    ($val:expr) => {{
        static REGEX: OnceLock<Regex> = OnceLock::new();
        REGEX.get_or_init(|| Regex::new($val).unwrap())
    }};
}

pub fn write_graph_with_trailing_comments(
    output: &mut String,
    module: &Module,
    func: &FunctionData,
    comment_fn: impl FnMut(&mut String, Node) -> Result<()>,
) -> Result<()> {
    writeln!(output, "function `{}`:", func.name)?;
    let mut comment_annotator = CommentAnnotator {
        comment_fn,
        result: Ok(()),
    };

    let _ = write_annotated_graph(
        output,
        &mut comment_annotator,
        module,
        &func.graph,
        func.entry,
        0,
    );

    comment_annotator.result
}

struct CommentAnnotator<F> {
    comment_fn: F,
    result: Result<()>,
}

impl<F: FnMut(&mut String, Node) -> Result<()>> AnnotateGraph<String> for CommentAnnotator<F> {
    fn write_node(
        &mut self,
        s: &mut String,
        module: &Module,
        graph: &ValGraph,
        node: Node,
    ) -> fmt::Result {
        const MAX_LINE_LENGTH: usize = 45;

        let orig_len = s.len();
        write_annotated_node(s, self, module, graph, node).unwrap();
        let node_line_len = s.len() - orig_len;
        let pad_len = if node_line_len >= MAX_LINE_LENGTH {
            2
        } else {
            MAX_LINE_LENGTH - node_line_len
        };

        write!(s, "{:pad_len$}# ", "").unwrap();
        if let Err(err) = (self.comment_fn)(s, node) {
            self.result = Err(err);
            // Stop trying to print out the rest of the output.
            return Err(fmt::Error);
        }

        Ok(())
    }
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

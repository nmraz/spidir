use regex::Regex;
use std::sync::OnceLock;

macro_rules! regex {
    ($val:literal) => {{
        static REGEX: OnceLock<Regex> = OnceLock::new();
        REGEX.get_or_init(|| Regex::new($val).unwrap())
    }};
}

pub fn insert_lines_after<'a>(
    lines: &mut Vec<String>,
    i: usize,
    new_lines: impl IntoIterator<Item = &'a str>,
) {
    insert_lines_before(lines, i + 1, new_lines);
}

pub fn insert_lines_before<'a>(
    lines: &mut Vec<String>,
    i: usize,
    new_lines: impl IntoIterator<Item = &'a str>,
) {
    lines.splice(i..i, new_lines.into_iter().map(|line| line.to_owned()));
}

pub fn find_run_line<'a>(lines: impl Iterator<Item = &'a str>) -> Option<(usize, &'a str)> {
    let run_regex = regex!(r#"^run:\s*(.+)"#);

    lines.enumerate().find_map(|(i, line)| {
        let comment_start = find_comment_start(line)?;
        let comment = line[comment_start + 1..].trim();
        let captures = run_regex.captures(comment)?;
        Some((i, captures.get(1).unwrap().as_str()))
    })
}

pub fn find_comment_start(line: &str) -> Option<usize> {
    line.find('#')
}

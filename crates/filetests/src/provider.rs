use std::fmt::Write;

use fx_utils::FxHashMap;
use ir::module::Module;

use self::verify::VerifyProvider;

mod verify;

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

    pub fn advance_to_before(&mut self, mut pred: impl FnMut(&str) -> bool) {
        self.insertion_point += self.lines[self.insertion_point..]
            .iter()
            .position(|line| pred(line))
            .expect("could not find line to advance to");
    }

    pub fn advance_to_after(&mut self, pred: impl FnMut(&str) -> bool) {
        self.advance_to_before(pred);
        self.insertion_point += 1;
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

pub trait TestProvider {
    fn output_for(&self, module: &Module) -> String;
    fn update(&self, updater: &mut Updater<'_>, module: &Module, output_str: &str);
}

pub fn select_test_provider(run_command: &str) -> Box<dyn TestProvider> {
    match run_command {
        "verify" => Box::new(VerifyProvider),
        _ => panic!("unknown run command '{run_command}'"),
    }
}

use ir::module::Module;

use self::verify::VerifyProvider;

mod verify;

pub struct Updater<'a> {
    lines: &'a mut Vec<String>,
    insertion_point: usize,
}

impl<'a> Updater<'a> {
    pub fn new(lines: &'a mut Vec<String>) -> Self {
        Self {
            lines,
            insertion_point: 0,
        }
    }

    pub fn output(&self) -> String {
        let mut output = self.lines.join("\n");
        output.push('\n');
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
        self.lines.insert(self.insertion_point, line);
        self.insertion_point += 1;
    }
}

pub trait TestProvider {
    fn output_for(&self, module: &Module) -> String;
    fn update(&self, module: &Module, updater: &mut Updater<'_>, output_str: &str);
}

pub fn select_test_provider(run_command: &str) -> Box<dyn TestProvider> {
    match run_command {
        "verify" => Box::new(VerifyProvider),
        _ => panic!("unknown run command '{run_command}'"),
    }
}

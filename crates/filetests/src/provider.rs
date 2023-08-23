use ir::module::Module;

use self::verify::VerifyTest;

mod verify;

pub trait TestProvider {
    fn output_for(&self, module: &Module) -> String;
    fn update(&self, module: &Module, input_lines: &mut Vec<String>, output_str: &str);
}

pub fn select_test_provider(run_command: &str) -> Box<dyn TestProvider> {
    match run_command {
        "verify" => Box::new(VerifyTest),
        _ => panic!("unknown run command '{run_command}'"),
    }
}

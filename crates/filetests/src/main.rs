use std::{env, fs, path::Path};

use filetests::{run_test, select_test_provider_from_input};

fn main() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("filetests/verify.spdr");
    let contents = fs::read_to_string(path).expect("failed to read file");
    let producer = select_test_provider_from_input(&contents);
    run_test(&*producer, &contents, env::var("UPDATE_EXPECT").is_ok());
}

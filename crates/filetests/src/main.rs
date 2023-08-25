use std::{env, path::Path};

use anyhow::{Ok, Result};
use filetests::{run_file_test, TestOutcome, UpdateMode};

fn main() -> Result<()> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("filetests/verify.spdr");
    let outcome = run_file_test(&path, update_mode_from_env())?;
    if outcome == TestOutcome::Update(()) {
        eprintln!("Updated");
    }
    Ok(())
}

fn update_mode_from_env() -> UpdateMode {
    if env::var("UPDATE_EXPECT").is_ok() {
        UpdateMode::IfFailed
    } else {
        UpdateMode::Never
    }
}

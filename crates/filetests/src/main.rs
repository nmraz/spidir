use std::{env, fs, path::Path};

use anyhow::{Context, Ok, Result};
use filetests::{run_test, select_test_provider_from_input, TestOutcome, UpdateMode};

fn main() -> Result<()> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("filetests/verify.spdr");
    let contents = fs::read_to_string(path).context("failed to read file")?;
    let provider =
        select_test_provider_from_input(&contents).context("failed to select test provider")?;
    let outcome = run_test(&*provider, &contents, update_mode_from_env())?;
    if let TestOutcome::Update(updated) = outcome {
        eprint!("Updated:\n{updated}");
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

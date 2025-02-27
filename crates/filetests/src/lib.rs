use std::{fs, path::Path};

use anyhow::{Context, Ok, Result, anyhow};
use hooks::catch_panic_message;

use crate::utils::find_run_line;

use self::{provider::DynTestProvider, providers::select_test_provider};

pub use self::provider::{TestOutcome, UpdateMode};

#[macro_use]
mod utils;
mod provider;
mod providers;
mod regexes;

pub mod hooks;

pub fn run_file_test(path: &Path, update_mode: UpdateMode) -> Result<TestOutcome<()>> {
    catch_panic_message(|| {
        let input = fs::read_to_string(path).context("failed to read file")?;
        let provider =
            select_test_provider_from_input(&input).context("failed to select test provider")?;
        let outcome = provider.run(&input, update_mode)?;
        if let TestOutcome::Update(new_contents) = outcome {
            fs::write(path, new_contents).context("failed to update file")?;
            return Ok(TestOutcome::Update(()));
        }
        Ok(TestOutcome::Ok)
    })
    .map_err(|panic_msg| anyhow!(panic_msg))?
}

pub fn select_test_provider_from_input(input: &str) -> Result<Box<dyn DynTestProvider>> {
    let (_, run_command) =
        find_run_line(input.lines()).ok_or_else(|| anyhow!("no run line in test file"))?;
    select_test_provider(run_command)
}

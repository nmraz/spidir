use std::{
    env, fs,
    path::{Path, PathBuf},
    process,
};

use anyhow::Result;
use filetests::{run_file_test, TestOutcome, UpdateMode};

const OK: &str = "\x1b[32mok\x1b[0m";
const UPDATED: &str = "\x1b[34mupdated\x1b[0m";
const FAILED: &str = "\x1b[31mfailed\x1b[0m";

fn main() -> Result<()> {
    let case_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("cases");
    let cases = {
        let mut cases = Vec::new();
        walk_dir(&case_dir, &mut cases)?;
        cases
    };

    let mut passed = 0;
    let mut failures = Vec::new();
    let mut updated = 0;

    let update_mode = update_mode_from_env();
    eprintln!("\nrunning {} tests", cases.len());
    for case_path in &cases {
        let case_name = case_path.strip_prefix(&case_dir).unwrap_or(case_path);
        eprint!("test {} ... ", case_name.display());
        match run_file_test(case_path, update_mode) {
            Ok(TestOutcome::Ok) => {
                passed += 1;
                eprintln!("{OK}");
            }
            Ok(TestOutcome::Update(())) => {
                updated += 1;
                eprintln!("{UPDATED}")
            }
            Err(err) => {
                failures.push((case_name, err));
                eprintln!("{FAILED}")
            }
        }
    }

    eprintln!();

    let failed = failures.len();

    if failed > 0 {
        eprintln!("failures:\n");
        for (name, err) in &failures {
            eprintln!("\x1b[1m---- {} ----\x1b[0m\n{err}\n", name.display());
        }
    }

    let status = if failed > 0 { FAILED } else { OK };
    eprintln!("test result: {status}. {passed} passed; {failed} failed; {updated} updated\n");

    if failed > 0 {
        process::exit(1);
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

fn walk_dir(path: &Path, results: &mut Vec<PathBuf>) -> Result<()> {
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let entry_path = entry.path();
        if entry.file_type()?.is_dir() {
            walk_dir(&entry_path, results)?;
        } else {
            results.push(entry_path);
        }
    }
    Ok(())
}

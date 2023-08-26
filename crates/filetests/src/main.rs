use std::{
    env, fs,
    path::{Path, PathBuf},
    process,
};

use anyhow::{Context, Result};
use clap::{Parser, ValueEnum};
use filetests::{run_file_test, TestOutcome, UpdateMode};
use glob::Pattern;

const OK: &str = "\x1b[32mok\x1b[0m";
const UPDATED: &str = "\x1b[34mupdated\x1b[0m";
const FAILED: &str = "\x1b[31mfailed\x1b[0m";

#[derive(Clone, Copy, ValueEnum)]
enum CliUpdateMode {
    Never,
    Failed,
    Always,
}

impl CliUpdateMode {
    fn to_update_mode(self) -> UpdateMode {
        match self {
            CliUpdateMode::Never => UpdateMode::Never,
            CliUpdateMode::Failed => UpdateMode::IfFailed,
            CliUpdateMode::Always => UpdateMode::Always,
        }
    }
}

#[derive(Parser)]
#[command(max_term_width(100))]
struct Cli {
    /// How test files should be updated
    ///
    /// When set to `never`, tests that do not pass filecheck will be marked as failed and cause the
    /// run to fail.
    ///
    /// When set to `failed`, tests that do not pass filecheck will be updated with the correct
    /// directives (any old directives will be deleted).
    ///
    /// When set to `always`, all test files will be updated, deleting any existing directives.
    ///
    /// If this argument is not specified, the update mode will be inferred from the `UPDATE_EXPECT`
    /// environment variable. If this variable is *not* set, no tests will be updated (as if
    /// `never` had been specified); otherwise, failing tests will be updated, as if `failed` had
    /// been specified.
    #[arg(long)]
    update_mode: Option<CliUpdateMode>,

    /// One or more glob patterns indicating which tests to run
    ///
    /// If no patterns are specified, all tests will be run.
    cases: Option<Vec<String>>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let update_mode = cli
        .update_mode
        .map_or_else(update_mode_from_env, CliUpdateMode::to_update_mode);

    let case_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("cases");
    let (cases, filtered) = collect_test_cases(&case_dir, cli.cases)?;

    let mut passed = 0;
    let mut failures = Vec::new();
    let mut updated = 0;

    eprintln!("\nrunning {} tests", cases.len());
    for case_path in &cases {
        let case_name = case_path.strip_prefix(&case_dir)?;
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
            eprintln!("\x1b[1m---- {} ----\x1b[0m\n{err:?}\n", name.display());
        }
    }

    let status = if failed > 0 { FAILED } else { OK };
    eprintln!("test result: {status}. {passed} passed; {failed} failed; {updated} updated; {filtered} filtered out\n");

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

fn collect_test_cases(
    case_dir: &Path,
    case_patterns: Option<Vec<String>>,
) -> Result<(Vec<PathBuf>, usize)> {
    let case_patterns = case_patterns
        .map(|case_patterns| {
            case_patterns
                .into_iter()
                .map(|case| {
                    Pattern::new(&case).with_context(|| format!("invalid glob pattern '{case}'"))
                })
                .collect::<Result<Vec<_>, _>>()
        })
        .transpose()?;

    let mut cases = Vec::new();
    let mut filtered = 0;
    walk_dir(
        case_dir,
        case_dir,
        case_patterns.as_deref(),
        &mut cases,
        &mut filtered,
    )?;
    Ok((cases, filtered))
}

fn walk_dir(
    base_path: &Path,
    path: &Path,
    patterns: Option<&[Pattern]>,
    results: &mut Vec<PathBuf>,
    filtered: &mut usize,
) -> Result<()> {
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let entry_path = entry.path();
        if entry.file_type()?.is_dir() {
            walk_dir(base_path, &entry_path, patterns, results, filtered)?;
        } else {
            let entry_relpath = entry_path.strip_prefix(base_path)?;
            if matches_patterns(entry_relpath, patterns) {
                results.push(entry_path);
            } else {
                (*filtered) += 1;
            }
        }
    }

    Ok(())
}

fn matches_patterns(path: &Path, patterns: Option<&[Pattern]>) -> bool {
    match patterns {
        Some(patterns) => patterns.iter().any(|pat| pat.matches_path(path)),
        // All tests should be run if no patterns are specified
        None => true,
    }
}

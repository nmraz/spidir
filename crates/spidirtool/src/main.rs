use std::{
    ffi::OsStr,
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::{anyhow, bail, Context, Result};
use clap::{Args, Parser, Subcommand, ValueEnum};
use ir::{
    domtree,
    loop_forest::LoopForest,
    module::{FunctionData, Module},
    verify::{verify_func, verify_module},
};
use ir_graphviz::{
    annotate::{Annotate, ColoredAnnotator, DomTreeAnnotator, ErrorAnnotator, LoopAnnotator},
    write_graphviz,
};
use parser::parse_module;
use tempfile::NamedTempFile;

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    subcommand: ToolCommand,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum GraphFormat {
    /// Output a `.dot` file
    Dot,
    /// Output an SVG (requires `dot` to be installed)
    Svg,
}

impl GraphFormat {
    fn as_str(self) -> &'static str {
        match self {
            GraphFormat::Dot => "dot",
            GraphFormat::Svg => "svg",
        }
    }
}

#[derive(Args)]
struct AnnotatorOptions {
    /// Don't show verifier errors inline in the graph
    #[arg(long)]
    no_verify: bool,

    /// Show dominance edges in the graph
    #[arg(long)]
    domtree: bool,

    /// Annotate loop headers and bodies in the graph
    #[arg(long)]
    loops: bool,
}

#[derive(Subcommand)]
enum ToolCommand {
    /// Display the specified function as a graphviz graph
    Graph {
        /// The input IR file
        input_file: PathBuf,
        /// The function to graph
        function: String,

        #[clap(flatten)]
        annotator_opts: AnnotatorOptions,

        /// The output file to use
        ///
        /// A name derived from the input file and function name will be used if this parameter is
        /// not provided.
        #[arg(short, long)]
        output_file: Option<PathBuf>,

        /// The output format to use
        #[arg(short, long, value_enum, default_value = "svg")]
        format: GraphFormat,

        /// Don't open the generated file using `xdg-open`
        #[arg(long)]
        no_open: bool,
    },
    /// Verify an IR module
    Verify {
        /// The input IR file
        input_file: PathBuf,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.subcommand {
        ToolCommand::Graph {
            input_file,
            function: function_name,
            annotator_opts,
            output_file,
            format,
            no_open,
        } => {
            let output_file = output_file.unwrap_or_else(|| {
                let mut filename = input_file
                    .file_stem()
                    .unwrap_or(OsStr::new("graph"))
                    .to_owned();
                filename.push("_");
                filename.push(&function_name);
                filename.push(".");
                filename.push(format.as_str());
                input_file.parent().unwrap_or(Path::new(".")).join(filename)
            });

            let module = read_module(&input_file)?;
            let func = module
                .functions
                .values()
                .find(|func| func.name == function_name)
                .ok_or_else(|| anyhow!("function `{}` not found in module", function_name))?;

            match format {
                GraphFormat::Dot => {
                    let mut output_file =
                        File::create(&output_file).context("failed to create output file")?;
                    output_dot_file(&mut output_file, &annotator_opts, &module, func)?;
                }
                GraphFormat::Svg => {
                    let mut dotfile =
                        NamedTempFile::new().context("failed to create temporary dot file")?;
                    output_dot_file(dotfile.as_file_mut(), &annotator_opts, &module, func)?;

                    run_command(
                        Command::new("dot")
                            .arg("-Tsvg")
                            .arg(dotfile.path())
                            .arg("-o")
                            .arg(&output_file),
                    )
                    .context("failed to run `dot`")?;
                }
            }

            if !no_open {
                run_command(Command::new("xdg-open").arg(&output_file))
                    .context("failed to run `xdg-open`")?;
            }
        }
        ToolCommand::Verify { input_file } => {
            let module = read_module(&input_file)?;
            match verify_module(&module) {
                Ok(_) => eprintln!("Module valid"),
                Err(errors) => {
                    for error in errors {
                        eprintln!("error: {}", error.display_with_context(&module));
                    }
                    bail!("module contained errors")
                }
            }
        }
    }

    Ok(())
}

fn run_command(command: &mut Command) -> Result<()> {
    let status = command.spawn()?.wait()?;
    if !status.success() {
        bail!("command failed with exit code {status}");
    }
    Ok(())
}

fn output_dot_file(
    file: &mut File,
    annotator_opts: &AnnotatorOptions,
    module: &Module,
    func: &FunctionData,
) -> Result<()> {
    let errors;
    let loop_forest;

    let domtree = if annotator_opts.domtree || annotator_opts.loops {
        Some(domtree::compute(&func.graph, func.entry))
    } else {
        None
    };

    let mut annotators: Vec<Box<dyn Annotate>> = vec![Box::new(ColoredAnnotator)];

    if annotator_opts.domtree {
        let domtree = domtree.as_ref().unwrap();
        annotators.push(Box::new(DomTreeAnnotator::new(domtree)));
    }

    if annotator_opts.loops {
        let domtree = domtree.as_ref().unwrap();
        loop_forest = LoopForest::compute(&func.graph, domtree);
        annotators.push(Box::new(LoopAnnotator::new(domtree, &loop_forest)));
    }

    if !annotator_opts.no_verify {
        if let Err(inner_errors) = verify_func(module, func) {
            errors = inner_errors;
            annotators.push(Box::new(ErrorAnnotator::new(&func.graph, &errors)));
        }
    };

    let s = get_graphviz_str(&mut annotators, module, func)?;

    file.write_all(s.as_bytes())
        .context("failed to write dot file")?;

    Ok(())
}

fn get_graphviz_str(
    annotators: &mut [Box<dyn Annotate + '_>],
    module: &Module,
    func: &FunctionData,
) -> Result<String> {
    let mut s = String::new();
    write_graphviz(&mut s, annotators, module, &func.graph, func.entry)
        .context("failed to format dot graph")?;

    Ok(s)
}

fn read_module(input_file: &Path) -> Result<Module> {
    let input_data = fs::read_to_string(input_file).context("failed to read input file")?;
    let module = parse_module(&input_data).context("error parsing module")?;
    Ok(module)
}

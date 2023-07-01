use std::{
    ffi::OsStr,
    fmt::Display,
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::{anyhow, bail, Context, Result};
use clap::{Parser, Subcommand, ValueEnum};
use ir::{
    module::{FunctionData, Module},
    node::DepValueKind,
    valgraph::{Node, ValGraph},
    verify::{verify_module, GraphVerifierError, ModuleVerifierError},
    write::write_node,
};
use ir_graphviz::write_graphviz;
use itertools::Itertools;
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

#[derive(Subcommand)]
enum ToolCommand {
    /// Display the specified function as a graphviz graph
    Graph {
        /// The input IR file
        input_file: PathBuf,
        /// The function to graph
        function: String,

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
                    output_dot_file(&mut output_file, &module, func)?;
                }
                GraphFormat::Svg => {
                    let mut dotfile =
                        NamedTempFile::new().context("failed to create temporary dot file")?;
                    output_dot_file(dotfile.as_file_mut(), &module, func)?;

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
                        report_verifier_error(&module, &error);
                    }
                    bail!("module contained errors")
                }
            }
        }
    }

    Ok(())
}

fn report_verifier_error(module: &Module, error: &ModuleVerifierError) {
    eprint!("error: ");
    match error {
        ModuleVerifierError::Graph { function, error } => {
            let function = &module.functions[*function];
            let graph = &function.graph;
            eprint!("in function `{}`: ", function.name);
            match error {
                GraphVerifierError::UnusedControl(value) => {
                    let (node, output_idx) = graph.value_def(*value);
                    eprintln!(
                        "`{}`: control output {output_idx} unused",
                        display_node(module, graph, node)
                    );
                }
                GraphVerifierError::ReusedControl(value) => {
                    let (node, output_idx) = graph.value_def(*value);
                    eprintln!(
                        "`{}`: control output {output_idx} reused",
                        display_node(module, graph, node)
                    );
                }
                GraphVerifierError::BadInputCount { node, expected } => {
                    eprintln!(
                        "`{}`: bad input count, expected {expected}",
                        display_node(module, graph, *node)
                    );
                }
                GraphVerifierError::BadOutputCount { node, expected } => {
                    eprintln!(
                        "`{}`: bad output count, expected {expected}",
                        display_node(module, graph, *node)
                    );
                }
                GraphVerifierError::BadInputKind {
                    node,
                    input,
                    expected,
                } => {
                    eprintln!(
                        "`{}`: bad value kind for input {input}, expected one of {}, got `{}`",
                        display_node(module, graph, *node),
                        display_expected_kinds(expected),
                        graph.value_kind(graph.node_inputs(*node)[*input as usize])
                    );
                }
                GraphVerifierError::BadOutputKind { value, expected } => {
                    let (node, output_idx) = graph.value_def(*value);
                    eprintln!(
                        "`{}`: bad value kind for output {output_idx}, expected one of {}, got `{}`",
                        display_node(module, graph, node),
                        display_expected_kinds(expected),
                        graph.value_kind(*value)
                    );
                }
                GraphVerifierError::BadEntry(node) => {
                    eprintln!("`{}`: bad entry node", display_node(module, graph, *node));
                }
                GraphVerifierError::MisplacedEntry(node) => {
                    eprintln!(
                        "`{}`: misplaced entry node",
                        display_node(module, graph, *node)
                    );
                }
                GraphVerifierError::ConstantOutOfRange(node) => {
                    eprintln!(
                        "`{}`: constant value out of range",
                        display_node(module, graph, *node)
                    );
                }
            }
        }
        ModuleVerifierError::ReusedFunctionName(name) => eprintln!("function name `{name}` reused"),
    }
}

fn display_expected_kinds(kinds: &[DepValueKind]) -> impl Display + '_ {
    kinds
        .iter()
        .format_with(", ", |kind, f| f(&format_args!("`{kind}`")))
}

fn display_node(module: &Module, graph: &ValGraph, node: Node) -> String {
    let mut s = String::new();
    write_node(&mut s, module, graph, node, 0).unwrap();
    s
}

fn run_command(command: &mut Command) -> Result<()> {
    let status = command.spawn()?.wait()?;
    if !status.success() {
        bail!("command failed with exit code {status}");
    }
    Ok(())
}

fn output_dot_file(file: &mut File, module: &Module, func: &FunctionData) -> Result<()> {
    let mut s = String::new();
    write_graphviz(&mut s, module, &func.graph, func.entry)
        .context("failed to format dot graph")?;
    file.write_all(s.as_bytes())
        .context("failed to write dot file")?;
    Ok(())
}

fn read_module(input_file: &Path) -> Result<Module> {
    let input_data = fs::read_to_string(input_file).context("failed to read input file")?;
    let module = parse_module(&input_data).context("error parsing module")?;
    Ok(module)
}

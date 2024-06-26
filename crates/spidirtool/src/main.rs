use std::{
    ffi::OsStr,
    fmt::Write as _,
    fs::File,
    io::{self, Write as _},
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::{anyhow, bail, Context, Result};
use clap::{Args, Parser, Subcommand, ValueEnum};

use codegen::{
    cfg::CfgContext, isel::select_instrs, lir::Lir, schedule::Schedule, target::x86_64::X64Machine,
};
use ir::{
    domtree::DomTree,
    loops::LoopForest,
    module::{FunctionData, Module},
    valwalk::cfg_preorder,
    verify::verify_func,
    write::{display_node, quote_ident, write_signature},
};
use ir_graphviz::{
    annotate::{Annotate, ColoredAnnotator, DomTreeAnnotator, ErrorAnnotator, LoopAnnotator},
    write_graphviz,
};
use tempfile::NamedTempFile;

use crate::extract::extract_function;
use crate::utils::{function_by_name, read_and_verify_module, read_module};

mod extract;
mod utils;

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
    /// Extract a single function into its own dedicated module, marking other functions as `extfunc`
    Extract {
        /// The input IR file
        input_file: PathBuf,
        /// The function to extract
        function: String,
    },
    /// Schedule an IR module in preparation for codegen
    Schedule {
        /// The input IR file
        input_file: PathBuf,
    },
    /// Schedule and instruction-select an IR module, then dump the resulting LIR
    Lir {
        /// The input IR file
        input_file: PathBuf,

        /// Run register allocator on generated LIR
        #[arg(long)]
        regalloc: bool,
    },
}

fn main() -> Result<()> {
    init_logging();

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
            let (_, func) = function_by_name(&module, &function_name)?;

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
            read_and_verify_module(&input_file)?;
            eprintln!("Module valid");
        }
        ToolCommand::Extract {
            input_file,
            function,
        } => {
            let module = read_module(&input_file)?;
            let extracted_module = extract_function(&module, &function)?;
            write!(io::stdout(), "{extracted_module}")?;
        }
        ToolCommand::Schedule { input_file } => {
            let module = read_and_verify_module(&input_file)?;
            io::stdout().write_all(get_module_schedule_str(&module).as_bytes())?;
        }
        ToolCommand::Lir {
            input_file,
            regalloc,
        } => {
            let module = read_and_verify_module(&input_file)?;
            io::stdout().write_all(get_module_lir_str(&module, regalloc)?.as_bytes())?;
        }
    }

    Ok(())
}

fn init_logging() {
    #[cfg(not(feature = "no_logging"))]
    {
        env_logger::builder()
            .format(|buf, record| {
                writeln!(
                    buf,
                    "[{} {}] {}",
                    record.level(),
                    record.module_path().unwrap_or_default(),
                    record.args()
                )
            })
            .init()
    }
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
        Some(DomTree::compute(&func.graph, func.entry))
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

fn get_module_schedule_str(module: &Module) -> String {
    let mut output = String::new();

    for func in module.functions.values() {
        write!(output, "func @{}", quote_ident(&func.name)).unwrap();
        write_signature(&mut output, &func.sig).unwrap();

        let cfg_preorder: Vec<_> = cfg_preorder(&func.graph, func.entry).collect();
        let (cfg_ctx, block_map) = CfgContext::compute_for_valgraph(&func.graph, &cfg_preorder);
        let schedule = Schedule::compute(&func.graph, &cfg_preorder, &cfg_ctx, &block_map);

        writeln!(
            output,
            " {{\n{}}}\n",
            schedule.display(module, &func.graph, &cfg_ctx.cfg, &cfg_ctx.block_order)
        )
        .unwrap();
    }

    output
}

fn get_module_lir_str(module: &Module, regalloc: bool) -> Result<String> {
    let mut output = String::new();

    for func in module.functions.values() {
        writeln!(output, "func @{} {{", quote_ident(&func.name)).unwrap();
        let (cfg_ctx, lir) = get_function_lir(module, func)?;

        if regalloc {
            let assignment = codegen::regalloc::run(&lir, &cfg_ctx, &X64Machine)
                .map_err(|err| anyhow!("register allocation failed: {err:?}"))?;
            write!(output, "{}", assignment.display(&cfg_ctx.block_order, &lir)).unwrap();
        } else {
            write!(
                output,
                "{}",
                lir.display(&cfg_ctx.cfg, &cfg_ctx.block_order)
            )
            .unwrap();
        }
        writeln!(output, "}}\n").unwrap();
    }

    Ok(output)
}

fn get_function_lir(module: &Module, func: &FunctionData) -> Result<(CfgContext, Lir<X64Machine>)> {
    let cfg_preorder: Vec<_> = cfg_preorder(&func.graph, func.entry).collect();
    let (cfg_ctx, block_map) = CfgContext::compute_for_valgraph(&func.graph, &cfg_preorder);
    let schedule = Schedule::compute(&func.graph, &cfg_preorder, &cfg_ctx, &block_map);
    let machine = X64Machine;
    let lir =
        select_instrs(module, func, &schedule, &cfg_ctx, &block_map, &machine).map_err(|err| {
            anyhow!(
                "failed to select `{}`: `{}`",
                func.name,
                display_node(module, &func.graph, err.node)
            )
        })?;
    Ok((cfg_ctx, lir))
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

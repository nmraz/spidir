use std::{
    ffi::OsStr,
    fmt::Write as _,
    fs::File,
    io::{self, Write as _},
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::{Context, Result, anyhow, bail};
use clap::{Args, Parser, Subcommand, ValueEnum};

use codegen::{
    api::{CodegenOpts, codegen_func, lower_func, schedule_graph},
    target::x64::{CodeModel, X64Machine, X64MachineConfig},
};
use codegen_test_tools::{disasm::disasm_code, exec::codegen_and_exec};
use ir::{
    domtree::DomTree,
    function::{FunctionBody, FunctionBorrow},
    loops::LoopForest,
    module::{Module, ModuleMetadata},
    verify::verify_func,
    write::{display_node, quote_ident, write_function_metadata},
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum CliCodeModel {
    /// Calls have +/-2GiB range
    SmallPic,
    /// Calls have full 64-bit address range
    LargeAbs,
}

impl CliCodeModel {
    fn as_code_model(self) -> CodeModel {
        match self {
            CliCodeModel::SmallPic => CodeModel::SmallPic,
            CliCodeModel::LargeAbs => CodeModel::LargeAbs,
        }
    }
}

#[derive(Args)]
struct MachineOptions {
    #[arg(long)]
    internal_code_model: Option<CliCodeModel>,
    #[arg(long)]
    extern_code_model: Option<CliCodeModel>,
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
    /// Optimize an IR module and dump it
    Opt {
        /// The input IR file
        input_file: PathBuf,
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

        /// Verify register allocation performed by `--regalloc`
        #[arg(long, requires("regalloc"))]
        verify_regalloc: bool,

        #[clap(flatten)]
        machine_opts: MachineOptions,
    },
    /// Generate code for an IR module and dump the disassembly
    Codegen {
        input_file: PathBuf,
        #[clap(flatten)]
        machine_opts: MachineOptions,
    },
    /// Optimize and generate code for an IR module, then dump the disassembly
    Compile {
        input_file: PathBuf,
        #[clap(flatten)]
        machine_opts: MachineOptions,
    },
    /// Generate native code for an IR module and execute it
    CodegenExec {
        /// The input IR file
        input_file: PathBuf,
        /// The function to run
        function: String,
        /// Integer arguments to pass to the function
        args: Vec<isize>,
        /// Place a breakpoint before the start of the function
        ///
        /// This breakpoint will have to be manually skipped using a debugger, and will cause a
        /// crash if no debugger is attached.
        #[arg(long, short)]
        r#break: bool,
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
                    output_dot_file(&mut output_file, &annotator_opts, &module.metadata, func)?;
                }
                GraphFormat::Svg => {
                    let mut dotfile =
                        NamedTempFile::new().context("failed to create temporary dot file")?;
                    output_dot_file(
                        dotfile.as_file_mut(),
                        &annotator_opts,
                        &module.metadata,
                        func,
                    )?;

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
        ToolCommand::Opt { input_file } => {
            let mut module = read_and_verify_module(&input_file)?;
            optimize_module(&mut module);
            write!(io::stdout(), "{module}")?;
        }
        ToolCommand::Schedule { input_file } => {
            let module = read_and_verify_module(&input_file)?;
            io::stdout().write_all(get_module_schedule_str(&module).as_bytes())?;
        }
        ToolCommand::Lir {
            input_file,
            regalloc,
            verify_regalloc,
            machine_opts,
        } => {
            let module = read_and_verify_module(&input_file)?;
            io::stdout().write_all(
                get_module_lir_str(&module, &machine_opts, regalloc, verify_regalloc)?.as_bytes(),
            )?;
        }
        ToolCommand::Codegen {
            input_file,
            machine_opts,
        } => {
            let module = read_and_verify_module(&input_file)?;
            io::stdout().write_all(get_module_code_str(&module, &machine_opts)?.as_bytes())?;
        }
        ToolCommand::Compile {
            input_file,
            machine_opts,
        } => {
            let mut module = read_and_verify_module(&input_file)?;
            optimize_module(&mut module);
            io::stdout().write_all(get_module_code_str(&module, &machine_opts)?.as_bytes())?;
        }
        ToolCommand::CodegenExec {
            input_file,
            function,
            args,
            r#break,
        } => {
            let module = read_and_verify_module(&input_file)?;
            let (func, _) = function_by_name(&module, &function)?;
            let ret = unsafe { codegen_and_exec(&module, func, &args, r#break)? };
            println!("{ret}");
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
    module_metadata: &ModuleMetadata,
    func: FunctionBorrow<'_>,
) -> Result<()> {
    let graph = &func.body().graph;

    let errors;
    let loop_forest;

    let domtree = if annotator_opts.domtree || annotator_opts.loops {
        Some(DomTree::compute(graph, func.body().entry))
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
        loop_forest = LoopForest::compute(graph, domtree);
        annotators.push(Box::new(LoopAnnotator::new(domtree, &loop_forest)));
    }

    if !annotator_opts.no_verify {
        if let Err(inner_errors) = verify_func(module_metadata, func) {
            errors = inner_errors;
            annotators.push(Box::new(ErrorAnnotator::new(graph, &errors)));
        }
    };

    let s = get_graphviz_str(&mut annotators, module_metadata, func.body())?;

    file.write_all(s.as_bytes())
        .context("failed to write dot file")?;

    Ok(())
}

fn optimize_module(module: &mut Module) {
    // For now: just run the canonicalization pass on everything.
    for func in module.functions.values_mut() {
        opt::canonicalize::canonicalize(&mut func.body, &mut func.node_cache);
    }
}

fn get_module_schedule_str(module: &Module) -> String {
    let mut output = String::new();

    for (func, metadata) in module.metadata.functions() {
        let func = &module.functions[func];

        write_function_metadata(&mut output, metadata).unwrap();

        let (cfg_ctx, _, schedule) = schedule_graph(&func.body.graph, func.body.entry);

        writeln!(
            output,
            " {{\n{}}}\n",
            schedule.display(
                &module.metadata,
                &func.body,
                &cfg_ctx.cfg,
                &cfg_ctx.block_order
            )
        )
        .unwrap();
    }

    output
}

fn get_module_lir_str(
    module: &Module,
    machine_opts: &MachineOptions,
    regalloc: bool,
    verify_regalloc: bool,
) -> Result<String> {
    let mut output = String::new();

    let machine = create_machine(machine_opts);

    for func in module.metadata.functions().keys() {
        let func = module.borrow_function(func);

        writeln!(output, "func @{} {{", quote_ident(&func.metadata.name)).unwrap();
        let (cfg_ctx, lir) = lower_func(&module.metadata, func, &machine).map_err(|err| {
            anyhow!(
                "failed to select `{}`: `{}`",
                func.metadata.name,
                display_node(&module.metadata, func.body(), err.node)
            )
        })?;

        if regalloc {
            let assignment = codegen::regalloc::run(&lir, &cfg_ctx, &machine)
                .map_err(|err| anyhow!("register allocation failed: {err:?}"))?;

            if verify_regalloc {
                codegen::regalloc::verify(&lir, &cfg_ctx, &assignment).map_err(|err| {
                    anyhow!(
                        "register allocation for `{}` invalid: {}",
                        func.metadata.name,
                        err.display(&lir, &assignment)
                    )
                })?;
            }

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

fn get_module_code_str(module: &Module, machine_opts: &MachineOptions) -> Result<String> {
    let mut output = String::new();
    let machine = create_machine(machine_opts);
    for func in module.metadata.functions().keys() {
        let func = module.borrow_function(func);
        let code = codegen_func(&module.metadata, func, &machine, &CodegenOpts::default())
            .map_err(|err| anyhow!("{}", err.display(&module.metadata, func)))?;
        writeln!(output, "{}:", quote_ident(&func.metadata.name)).unwrap();
        disasm_code(&module.metadata, &code, 4, &mut output)?;
        writeln!(output).unwrap();
    }
    Ok(output)
}

fn get_graphviz_str(
    annotators: &mut [Box<dyn Annotate + '_>],
    module_metadata: &ModuleMetadata,
    body: &FunctionBody,
) -> Result<String> {
    let mut s = String::new();
    write_graphviz(&mut s, annotators, module_metadata, body)
        .context("failed to format dot graph")?;
    Ok(s)
}

fn create_machine(machine_opts: &MachineOptions) -> X64Machine {
    let mut config = X64MachineConfig::default();
    if let Some(internal_code_model) = machine_opts.internal_code_model {
        config.internal_code_model = internal_code_model.as_code_model();
    }
    if let Some(extern_code_model) = machine_opts.extern_code_model {
        config.extern_code_model = extern_code_model.as_code_model();
    }

    X64Machine::new(config)
}

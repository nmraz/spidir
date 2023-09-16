use std::fmt::Write;

use anyhow::{anyhow, Result};
use ir::{
    module::{FunctionData, Module},
    verify::{verify_func, GraphVerifierError},
};
use ir_graphviz::{
    annotate::{Annotate, ColoredAnnotator, ErrorAnnotator},
    write_graphviz,
};

use super::{update_per_func_output, TestProvider, Updater};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnnotatorKind {
    Plain,
    Colored,
    Verify,
    VerifyColored,
}

pub struct GraphvizTestProvider {
    kind: AnnotatorKind,
}

impl GraphvizTestProvider {
    pub fn new(kind: AnnotatorKind) -> Self {
        Self { kind }
    }
}

impl TestProvider for GraphvizTestProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();
        for func in module.functions.values() {
            writeln!(output, "function `{}`:", func.name).unwrap();

            match self.kind {
                AnnotatorKind::Plain => {
                    write_graphviz_with_annotator(&mut output, &mut [], module, func);
                }
                AnnotatorKind::Colored => {
                    write_graphviz_with_annotator(
                        &mut output,
                        &mut [Box::new(ColoredAnnotator)],
                        module,
                        func,
                    );
                }
                AnnotatorKind::Verify => {
                    let errors = get_verifier_errors(module, func)?;
                    write_graphviz_with_annotator(
                        &mut output,
                        &mut [Box::new(ErrorAnnotator::new(&func.graph, &errors))],
                        module,
                        func,
                    );
                }
                AnnotatorKind::VerifyColored => {
                    let errors = get_verifier_errors(module, func)?;
                    write_graphviz_with_annotator(
                        &mut output,
                        &mut [
                            Box::new(ColoredAnnotator),
                            Box::new(ErrorAnnotator::new(&func.graph, &errors)),
                        ],
                        module,
                        func,
                    );
                }
            }
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, output_str: &str) -> Result<()> {
        update_per_func_output(updater, output_str)
    }
}

fn get_verifier_errors(module: &Module, func: &FunctionData) -> Result<Vec<GraphVerifierError>> {
    verify_func(module, func)
        .err()
        .ok_or_else(|| anyhow!("expected function to contain errors"))
}

fn write_graphviz_with_annotator(
    output: &mut String,
    annotators: &mut [Box<dyn Annotate + '_>],
    module: &Module,
    func: &FunctionData,
) {
    write_graphviz(output, annotators, module, &func.graph, func.entry, &[]).unwrap();
}

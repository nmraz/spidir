use std::fmt::Write;

use anyhow::{anyhow, Result};
use ir::{
    module::{FunctionData, Module},
    verify::verify_func,
};
use ir_graphviz::{
    annotate::{Annotate, ColoredAnnotator, ErrorAnnotator, PlainAnnotator},
    write_graphviz,
};

use super::{update_per_func_output, TestProvider, Updater};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnnotatorKind {
    Plain,
    Colored,
    Error,
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
                    write_graphviz_with_annotator(
                        &mut output,
                        Box::new(PlainAnnotator),
                        module,
                        func,
                    );
                }
                AnnotatorKind::Colored => {
                    write_graphviz_with_annotator(
                        &mut output,
                        Box::new(ColoredAnnotator),
                        module,
                        func,
                    );
                }
                AnnotatorKind::Error => {
                    let errors = verify_func(module, func)
                        .err()
                        .ok_or_else(|| anyhow!("expected function to contain errors"))?;
                    write_graphviz_with_annotator(
                        &mut output,
                        Box::new(ErrorAnnotator::new(&func.graph, &errors)),
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

fn write_graphviz_with_annotator(
    output: &mut String,
    annotator: Box<dyn Annotate + '_>,
    module: &Module,
    func: &FunctionData,
) {
    write_graphviz(
        output,
        &mut [annotator],
        module,
        &func.graph,
        func.entry,
        &[],
    )
    .unwrap();
}

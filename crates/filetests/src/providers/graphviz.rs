use std::fmt::Write;

use anyhow::{Result, anyhow, bail};
use ir::{
    domtree::DomTree,
    function::FunctionBorrow,
    loops::LoopForest,
    module::Module,
    verify::{FunctionVerifierError, verify_func},
};
use ir_graphviz::{
    annotate::{Annotate, ColoredAnnotator, DomTreeAnnotator, ErrorAnnotator, LoopAnnotator},
    write_graphviz,
};

use crate::provider::{SimpleTestProvider, Updater, update_per_func_output};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AnnotatorKind {
    Plain,
    Colored,
    Verify,
    VerifyColored,
    DomTree,
    Loops,
}

pub struct GraphvizTestProvider {
    kind: AnnotatorKind,
}

impl GraphvizTestProvider {
    pub fn new(params: &[&str]) -> Result<Self> {
        let &[kind] = params else {
            bail!("graphviz provider requires exactly one parameter")
        };

        let kind = match kind {
            "plain" => AnnotatorKind::Plain,
            "colored" => AnnotatorKind::Colored,
            "verify" => AnnotatorKind::Verify,
            "verify-colored" => AnnotatorKind::VerifyColored,
            "domtree" => AnnotatorKind::DomTree,
            "loops" => AnnotatorKind::Loops,
            _ => bail!("invalid annotator kind '{kind}'"),
        };

        Ok(Self { kind })
    }
}

impl SimpleTestProvider for GraphvizTestProvider {
    fn expects_valid_module(&self) -> bool {
        // Some of the test cases are intentionally invalid, to trigger error display on the graph.
        false
    }

    fn output_for(&self, module: Module) -> Result<String> {
        let mut output = String::new();
        for func in module.metadata.functions().keys() {
            let func = module.borrow_function(func);

            writeln!(output, "function `{}`:", func.metadata.name).unwrap();

            let body = func.body();

            match self.kind {
                AnnotatorKind::Plain => {
                    write_graphviz_with_annotator(&mut output, &mut [], &module, func);
                }
                AnnotatorKind::Colored => {
                    write_graphviz_with_annotator(
                        &mut output,
                        &mut [Box::new(ColoredAnnotator)],
                        &module,
                        func,
                    );
                }
                AnnotatorKind::Verify => {
                    let errors = get_verifier_errors(&module, func)?;
                    write_graphviz_with_annotator(
                        &mut output,
                        &mut [Box::new(ErrorAnnotator::new(&body.graph, &errors))],
                        &module,
                        func,
                    );
                }
                AnnotatorKind::VerifyColored => {
                    let errors = get_verifier_errors(&module, func)?;
                    write_graphviz_with_annotator(
                        &mut output,
                        &mut [
                            Box::new(ColoredAnnotator),
                            Box::new(ErrorAnnotator::new(&body.graph, &errors)),
                        ],
                        &module,
                        func,
                    );
                }
                AnnotatorKind::DomTree => {
                    let domtree = DomTree::compute(&body.graph, body.entry);
                    write_graphviz_with_annotator(
                        &mut output,
                        &mut [Box::new(DomTreeAnnotator::new(&domtree))],
                        &module,
                        func,
                    );
                }
                AnnotatorKind::Loops => {
                    let domtree = DomTree::compute(&body.graph, body.entry);
                    let loop_forest = LoopForest::compute(&body.graph, &domtree);
                    write_graphviz_with_annotator(
                        &mut output,
                        &mut [Box::new(LoopAnnotator::new(&domtree, &loop_forest))],
                        &module,
                        func,
                    );
                }
            }
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, output_str: &str) -> Result<()> {
        update_per_func_output(updater, output_str)
    }
}

fn get_verifier_errors(
    module: &Module,
    func: FunctionBorrow<'_>,
) -> Result<Vec<FunctionVerifierError>> {
    verify_func(module, func)
        .err()
        .ok_or_else(|| anyhow!("expected function to contain errors"))
}

fn write_graphviz_with_annotator(
    output: &mut String,
    annotators: &mut [Box<dyn Annotate + '_>],
    module: &Module,
    func: FunctionBorrow<'_>,
) {
    write_graphviz(output, annotators, module, func.body()).unwrap();
}

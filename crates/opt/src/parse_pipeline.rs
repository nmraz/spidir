use alloc::boxed::Box;

use pest::{Parser, error::Error, iterators::Pair};
use pest_derive::Parser;

use crate::{FunctionPass, FunctionPipeline, ModulePass, ModulePipeline, SccpPass};

#[derive(Parser)]
#[grammar = "pipeline.pest"]
struct PipelineDesc;

pub fn pipeline_from_desc(desc: &str) -> Result<ModulePipeline, Box<Error<Rule>>> {
    let parsed = PipelineDesc::parse(Rule::modpipeline, desc)?
        .next()
        .expect("expected top-level pipeline node");

    let passes = parsed
        .into_inner()
        .filter_map(|pair| match pair.as_rule() {
            Rule::modpass => Some(extract_modpass(pair)),
            Rule::EOI => None,
            _ => unreachable!("unkown module pass rule"),
        })
        .collect();
    Ok(ModulePipeline::new(passes))
}

fn extract_modpass(modpass_pair: Pair<'_, Rule>) -> Box<dyn ModulePass> {
    extract_special_modpass(modpass_pair)
}

fn extract_special_modpass(modpass_pair: Pair<'_, Rule>) -> Box<dyn ModulePass> {
    let inner = modpass_pair
        .into_inner()
        .next()
        .expect("expected special modpass rule");

    match inner.as_rule() {
        Rule::funcpipeline => {
            let inner_passes = inner.into_inner().map(extract_funcpass).collect();
            Box::new(FunctionPipeline::new(inner_passes))
        }
        _ => unreachable!("unknown special modpass rule"),
    }
}

fn extract_funcpass(funcpass_pair: Pair<'_, Rule>) -> Box<dyn FunctionPass> {
    assert!(funcpass_pair.as_rule() == Rule::funcpass);
    match funcpass_pair.as_str() {
        "sccp" => Box::new(SccpPass),
        _ => unreachable!("unknown function pass rule"),
    }
}

use core::fmt;

use alloc::{borrow::ToOwned, string::String, vec::Vec};
use fx_utils::FxHashSet;
use itertools::Itertools;

use crate::{
    module::{Function, FunctionData, Module, Signature},
    node::{DepValueKind, NodeKind},
    valgraph::{DepValue, Node, ValGraph},
    valwalk::LiveNodeInfo,
    write::write_node,
};

use self::node::verify_node_kind;

mod node;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleVerifierError {
    Graph {
        function: Function,
        error: GraphVerifierError,
    },
    ReusedFunctionName(String),
}

impl ModuleVerifierError {
    pub fn node<'a>(&self, module: &'a Module) -> Option<(&'a FunctionData, Node)> {
        match self {
            Self::Graph { function, error } => {
                let function = &module.functions[*function];
                match error {
                    GraphVerifierError::UnusedControl(value)
                    | GraphVerifierError::ReusedControl(value)
                    | GraphVerifierError::BadOutputKind { value, .. } => {
                        Some((function, function.graph.value_def(*value).0))
                    }
                    GraphVerifierError::BadInputCount { node, .. }
                    | GraphVerifierError::BadOutputCount { node, .. }
                    | GraphVerifierError::BadInputKind { node, .. }
                    | GraphVerifierError::BadEntry(node)
                    | GraphVerifierError::MisplacedEntry(node)
                    | GraphVerifierError::ConstantOutOfRange(node) => Some((function, *node)),
                }
            }
            _ => None,
        }
    }

    pub fn display<'a>(&'a self, module: &'a Module) -> DisplayError<'a> {
        DisplayError {
            module,
            error: self,
        }
    }

    pub fn display_with_context<'a>(&'a self, module: &'a Module) -> DisplayErrorWithContext<'a> {
        DisplayErrorWithContext {
            module,
            error: self,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GraphVerifierError {
    UnusedControl(DepValue),
    ReusedControl(DepValue),
    BadInputCount {
        node: Node,
        expected: u32,
    },
    BadOutputCount {
        node: Node,
        expected: u32,
    },
    BadInputKind {
        node: Node,
        input: u32,
        expected: Vec<DepValueKind>,
    },
    BadOutputKind {
        value: DepValue,
        expected: Vec<DepValueKind>,
    },
    BadEntry(Node),
    MisplacedEntry(Node),
    ConstantOutOfRange(Node),
}

#[derive(Clone, Copy)]
pub struct DisplayError<'a> {
    module: &'a Module,
    error: &'a ModuleVerifierError,
}

impl fmt::Display for DisplayError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.error {
            ModuleVerifierError::Graph { function, error } => {
                let function = &self.module.functions[*function];
                let graph = &function.graph;
                match error {
                    GraphVerifierError::UnusedControl(value) => {
                        let (_, output_idx) = graph.value_def(*value);
                        write!(f, "control output {output_idx} unused")?;
                    }
                    GraphVerifierError::ReusedControl(value) => {
                        let (_, output_idx) = graph.value_def(*value);
                        write!(f, "control output {output_idx} reused")?;
                    }
                    GraphVerifierError::BadInputCount { expected, .. } => {
                        write!(f, "bad input count, expected {expected}")?;
                    }
                    GraphVerifierError::BadOutputCount { expected, .. } => {
                        write!(f, "bad output count, expected {expected}")?;
                    }
                    GraphVerifierError::BadInputKind {
                        node,
                        input,
                        expected,
                    } => {
                        write!(
                            f,
                            "bad value kind for input {input}, expected one of {}, got `{}`",
                            display_expected_kinds(expected),
                            graph.value_kind(graph.node_inputs(*node)[*input as usize])
                        )?;
                    }
                    GraphVerifierError::BadOutputKind { value, expected } => {
                        let (_, output_idx) = graph.value_def(*value);
                        write!(
                            f,
                            "bad value kind for output {output_idx}, expected one of {}, got `{}`",
                            display_expected_kinds(expected),
                            graph.value_kind(*value)
                        )?;
                    }
                    GraphVerifierError::BadEntry(_) => {
                        write!(f, "bad entry node")?;
                    }
                    GraphVerifierError::MisplacedEntry(_) => {
                        write!(f, "misplaced entry node")?;
                    }
                    GraphVerifierError::ConstantOutOfRange(_) => {
                        write!(f, "constant value out of range")?;
                    }
                }
            }
            ModuleVerifierError::ReusedFunctionName(name) => {
                write!(f, "function name `{name}` reused")?;
            }
        }
        Ok(())
    }
}

pub struct DisplayErrorWithContext<'a> {
    module: &'a Module,
    error: &'a ModuleVerifierError,
}

impl<'a> fmt::Display for DisplayErrorWithContext<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some((function, node)) = self.error.node(self.module) {
            write!(
                f,
                "in function `{}`: `{}`: {}",
                function.name,
                display_node(self.module, &function.graph, node),
                self.error.display(self.module)
            )?;
        }

        Ok(())
    }
}

pub fn verify_module<'m>(module: &'m Module) -> Result<(), Vec<ModuleVerifierError>> {
    let mut errors = Vec::new();
    let mut names = FxHashSet::<&str>::default();
    let mut reported_names = FxHashSet::<&str>::default();

    let mut check_name = |name: &'m str, errors: &mut Vec<ModuleVerifierError>| {
        if names.contains(&name) {
            if !reported_names.contains(&name) {
                errors.push(ModuleVerifierError::ReusedFunctionName(name.to_owned()));
                reported_names.insert(name);
            }
        } else {
            names.insert(name);
        }
    };

    for extern_function_data in module.extern_functions.values() {
        check_name(&extern_function_data.name, &mut errors);
    }

    for (function, function_data) in &module.functions {
        check_name(&function_data.name, &mut errors);
        if let Err(graph_errors) = verify_graph(
            &function_data.graph,
            &function_data.sig,
            function_data.entry,
        ) {
            errors.extend(
                graph_errors
                    .into_iter()
                    .map(|error| ModuleVerifierError::Graph { function, error }),
            )
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

pub fn verify_graph(
    graph: &ValGraph,
    signature: &Signature,
    entry: Node,
) -> Result<(), Vec<GraphVerifierError>> {
    let mut errors = Vec::new();

    if graph.node_kind(entry) != &NodeKind::Entry {
        errors.push(GraphVerifierError::BadEntry(entry));
    }

    for node in LiveNodeInfo::compute(graph, entry).iter_live_nodes() {
        verify_node_kind(graph, signature, entry, node, &mut errors);
        verify_control_outputs(graph, node, &mut errors);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn verify_control_outputs(graph: &ValGraph, node: Node, errors: &mut Vec<GraphVerifierError>) {
    for output in graph.node_outputs(node) {
        if graph.value_kind(output) != DepValueKind::Control {
            continue;
        }

        let mut uses = graph.value_uses(output);
        if uses.next().is_none() {
            errors.push(GraphVerifierError::UnusedControl(output));
            continue;
        }

        if uses.next().is_some() {
            errors.push(GraphVerifierError::ReusedControl(output));
        }
    }
}

fn display_expected_kinds(kinds: &[DepValueKind]) -> impl fmt::Display + '_ {
    kinds
        .iter()
        .format_with(", ", |kind, f| f(&format_args!("`{kind}`")))
}

fn display_node(module: &Module, graph: &ValGraph, node: Node) -> String {
    let mut s = String::new();
    write_node(&mut s, module, graph, node, 0).unwrap();
    s
}

#[cfg(test)]
mod tests;

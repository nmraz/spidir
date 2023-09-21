use core::fmt;

use alloc::{borrow::ToOwned, string::String, vec::Vec};

use fx_utils::FxHashSet;
use itertools::{izip, Itertools};
use smallvec::SmallVec;

use crate::{
    domtree::{DomTree, TreeNode},
    module::{Function, FunctionData, Module},
    node::{DepValueKind, NodeKind},
    schedule::{schedule_early, ByNodeSchedule, ScheduleCtx},
    valgraph::{DepValue, Node, ValGraph},
    valwalk::walk_live_nodes,
    write::display_node,
};

use self::node::verify_node_kind;

mod node;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GraphVerifierError {
    UnusedControl(DepValue),
    ReusedControl(DepValue),
    UseNotDominated {
        node: Node,
        input: u32,
    },
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
    BadFillWidth(Node),
    BadStackSlotAlign(Node),
}

impl GraphVerifierError {
    pub fn node(&self, graph: &ValGraph) -> Node {
        match self {
            Self::UnusedControl(value)
            | Self::ReusedControl(value)
            | Self::BadOutputKind { value, .. } => graph.value_def(*value).0,
            Self::UseNotDominated { node, .. }
            | Self::BadInputCount { node, .. }
            | Self::BadOutputCount { node, .. }
            | Self::BadInputKind { node, .. }
            | Self::BadEntry(node)
            | Self::MisplacedEntry(node)
            | Self::ConstantOutOfRange(node)
            | Self::BadFillWidth(node)
            | Self::BadStackSlotAlign(node) => *node,
        }
    }

    pub fn node_input(&self) -> Option<(Node, u32)> {
        match self {
            &Self::BadInputKind { node, input, .. } | &Self::UseNotDominated { node, input } => {
                Some((node, input))
            }
            _ => None,
        }
    }

    pub fn display<'a>(&'a self, graph: &'a ValGraph) -> DisplayGraphVerifierError<'a> {
        DisplayGraphVerifierError { graph, error: self }
    }
}

pub struct DisplayGraphVerifierError<'a> {
    graph: &'a ValGraph,
    error: &'a GraphVerifierError,
}

impl fmt::Display for DisplayGraphVerifierError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.error {
            GraphVerifierError::UnusedControl(value) => {
                let (_, output_idx) = self.graph.value_def(*value);
                write!(f, "control output {output_idx} unused")?;
            }
            GraphVerifierError::ReusedControl(value) => {
                let (_, output_idx) = self.graph.value_def(*value);
                write!(f, "control output {output_idx} reused")?;
            }
            GraphVerifierError::UseNotDominated { input, .. } => {
                write!(f, "input {input} not dominated by def")?;
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
                    self.graph
                        .value_kind(self.graph.node_inputs(*node)[*input as usize])
                )?;
            }
            GraphVerifierError::BadOutputKind { value, expected } => {
                let (_, output_idx) = self.graph.value_def(*value);
                write!(
                    f,
                    "bad value kind for output {output_idx}, expected one of {}, got `{}`",
                    display_expected_kinds(expected),
                    self.graph.value_kind(*value)
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
            GraphVerifierError::BadFillWidth(_) => {
                write!(f, "bad fill width")?;
            }
            GraphVerifierError::BadStackSlotAlign(_) => {
                write!(f, "illegal stack slot alignment")?;
            }
        }

        Ok(())
    }
}

fn display_expected_kinds(kinds: &[DepValueKind]) -> impl fmt::Display + '_ {
    kinds
        .iter()
        .format_with(", ", |kind, f| f(&format_args!("`{kind}`")))
}

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
                Some((function, error.node(&function.graph)))
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
                write!(f, "{}", error.display(&function.graph))
            }
            ModuleVerifierError::ReusedFunctionName(name) => {
                write!(f, "function name `{name}` reused")
            }
        }
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
            )
        } else {
            write!(f, "{}", self.error.display(self.module))
        }
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
        if let Err(graph_errors) = verify_func(module, function_data) {
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

pub fn verify_func(module: &Module, func: &FunctionData) -> Result<(), Vec<GraphVerifierError>> {
    let mut errors = Vec::new();

    if func.graph.node_kind(func.entry) != &NodeKind::Entry {
        errors.push(GraphVerifierError::BadEntry(func.entry));
    }

    for node in walk_live_nodes(&func.graph, func.entry) {
        verify_node_kind(module, func, node, &mut errors);
        verify_control_outputs(&func.graph, node, &mut errors);
    }

    if !errors.is_empty() {
        // If there were type/CFG errors, don't move on and try to verify data flow.
        // This allows us to assume basic well-formedness during data flow verification.
        return Err(errors);
    }

    verify_dataflow(&func.graph, func.entry, &mut errors);

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

type InputLocScratch = SmallVec<[(TreeNode, u32); 4]>;

fn verify_dataflow(graph: &ValGraph, entry: Node, errors: &mut Vec<GraphVerifierError>) {
    let domtree = DomTree::compute(graph, entry);
    let ctx = ScheduleCtx::prepare(graph, &domtree);

    let mut input_locs = InputLocScratch::new();
    let schedule = schedule_early(&ctx, |sched, node| {
        get_last_scheduled_input(&ctx, sched, node, &mut input_locs, errors)
            .map(|(domtree_node, _input_idx)| domtree_node)
            .unwrap_or(ctx.domtree().root())
    });

    for &cfg_node in ctx.cfg_preorder() {
        let domtree_node = ctx.domtree().get_tree_node(cfg_node).unwrap();
        verify_dataflow_cfg_node_inputs(&ctx, domtree_node, &schedule, &mut input_locs, errors);

        for phi in ctx.get_attached_phis(cfg_node) {
            verify_dataflow_phi_node_inputs(&ctx, cfg_node, phi, &schedule, errors);
        }
    }
}

fn verify_dataflow_cfg_node_inputs(
    ctx: &ScheduleCtx<'_>,
    domtree_node: TreeNode,
    schedule: &ByNodeSchedule,
    input_locs: &mut InputLocScratch,
    errors: &mut Vec<GraphVerifierError>,
) {
    let cfg_node = ctx.domtree().get_cfg_node(domtree_node);
    let Some((last_scheduled_input, last_scheduled_input_idx)) =
        get_last_scheduled_input(ctx, schedule, cfg_node, input_locs, errors)
    else {
        // If we have no data inputs, everything is fine.
        return;
    };

    if !ctx.domtree().dominates(last_scheduled_input, domtree_node) {
        errors.push(GraphVerifierError::UseNotDominated {
            node: cfg_node,
            input: last_scheduled_input_idx,
        })
    }
}

fn verify_dataflow_phi_node_inputs(
    ctx: &ScheduleCtx<'_>,
    cfg_node: Node,
    phi: Node,
    schedule: &ByNodeSchedule,
    errors: &mut Vec<GraphVerifierError>,
) {
    let graph = ctx.graph();
    for (i, phi_input, ctrl_input) in izip!(
        1..,
        graph.node_inputs(phi).into_iter().skip(1),
        graph.node_inputs(cfg_node),
    ) {
        let Some(ctrl_tree_node) = schedule[graph.value_def(ctrl_input).0].expand() else {
            // Inputs from dead regions are actually okay: if we can never select the
            // corresponding value in the phi, we don't need to check any constraints on it.
            // This can happen in practice when existing regions in the code are made dead
            // by optimizations.
            continue;
        };

        let input_node = graph.value_def(phi_input).0;

        let Some(input_sched_tree_node) = schedule[input_node].expand() else {
            // Having dead *inputs* for a non-dead control edge, on the other hand, is
            // problematic. Report that now.
            errors.push(GraphVerifierError::UseNotDominated {
                node: phi,
                input: i,
            });
            continue;
        };

        // Finally, we can actually check that the phi input is scheduled before the
        // corresponding control input.
        if !ctx
            .domtree()
            .dominates(input_sched_tree_node, ctrl_tree_node)
        {
            errors.push(GraphVerifierError::UseNotDominated {
                node: phi,
                input: i,
            });
        }
    }
}

fn get_last_scheduled_input(
    ctx: &ScheduleCtx<'_>,
    sched: &ByNodeSchedule,
    node: Node,
    input_locs: &mut InputLocScratch,
    errors: &mut Vec<GraphVerifierError>,
) -> Option<(TreeNode, u32)> {
    let graph = ctx.graph();

    input_locs.clear();

    // Part 1: Find the schedule position for every input data value.
    for (i, input) in (0..).zip(graph.node_inputs(node)) {
        if !graph.value_kind(input).is_value() {
            continue;
        }

        let input_node = graph.value_def(input).0;

        let sched_node = sched[input_node].expand();
        let Some(sched_node) = sched_node else {
            // If the node producing this input hasn't been scheduled, we either have a data flow
            // cycle or that node depends on a computation that is unreachable in the CFG. Both
            // scenarios are errors.
            errors.push(GraphVerifierError::UseNotDominated { node, input: i });
            continue;
        };
        // Save the input index so we can report errors accurately later.
        input_locs.push((sched_node, i));
    }

    // Part 2: Check that input schedules are totally ordered with respect to dominance, and find
    // the maximum with respect to that relation.
    // We use a simple insertion sort for doing both things.

    for i in 1..input_locs.len() {
        // Insert `input_locs[i]` into the sorted subslice `input_locs[0..i]`.
        for j in (0..i).rev() {
            let (prev, prev_input_idx) = input_locs[j];
            // Note: `j < i`, so `j + 1 <= i < len`.
            let (cur, cur_input_idx) = input_locs[j + 1];

            // TODO: We could save some redundant comparisons here by reintroducing `DomTree::compare`.
            if ctx.domtree().dominates(cur, prev) {
                input_locs.swap(j, j + 1);
            } else if !ctx.domtree().dominates(prev, cur) {
                // The inputs at positions `j` and `j + 1` are not ordered by dominance; report an
                // error on one of them. We choose `j + 1` here so that nodes always appear to be
                // placed based on earlier (lower-indexed) inputs.
                errors.push(GraphVerifierError::UseNotDominated {
                    node,
                    input: cur_input_idx,
                });

                // We have a broken invariant, so don't try to schedule based on remaining inputs;
                // just go by what we've seen so far to enable at least some additional errors to be
                // detected.
                return Some((prev, prev_input_idx));
            }
        }
    }

    input_locs.last().copied()
}

#[cfg(test)]
mod tests;

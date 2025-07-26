use core::fmt;

use alloc::{borrow::ToOwned, string::String, vec::Vec};

use cranelift_entity::{SecondaryMap, packed_option::PackedOption};
use fx_utils::FxHashSet;
use graphwalk::PostOrderContext;
use itertools::{Itertools, izip};
use smallvec::SmallVec;

use crate::{
    domtree::{DomTree, DomTreeNode},
    function::{FunctionBody, FunctionBorrow, FunctionMetadata},
    module::{Function, Module, ModuleMetadata},
    node::{DepValueKind, NodeKind},
    schedule::{ScheduleContext, schedule_early},
    valgraph::{DepValue, Node, ValGraph},
    valwalk::walk_graph,
    write::display_node,
};

use self::node::verify_node_kind;

mod node;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionVerifierError {
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

impl FunctionVerifierError {
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

    pub fn display<'a>(&'a self, graph: &'a ValGraph) -> DisplayFunctionVerifierError<'a> {
        DisplayFunctionVerifierError { graph, error: self }
    }
}

pub struct DisplayFunctionVerifierError<'a> {
    graph: &'a ValGraph,
    error: &'a FunctionVerifierError,
}

impl fmt::Display for DisplayFunctionVerifierError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.error {
            FunctionVerifierError::UnusedControl(value) => {
                let (_, output_idx) = self.graph.value_def(*value);
                write!(f, "control output {output_idx} unused")?;
            }
            FunctionVerifierError::ReusedControl(value) => {
                let (_, output_idx) = self.graph.value_def(*value);
                write!(f, "control output {output_idx} reused")?;
            }
            FunctionVerifierError::UseNotDominated { input, .. } => {
                write!(f, "input {input} not dominated by def")?;
            }
            FunctionVerifierError::BadInputCount { expected, .. } => {
                write!(f, "bad input count, expected {expected}")?;
            }
            FunctionVerifierError::BadOutputCount { expected, .. } => {
                write!(f, "bad output count, expected {expected}")?;
            }
            FunctionVerifierError::BadInputKind {
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
            FunctionVerifierError::BadOutputKind { value, expected } => {
                let (_, output_idx) = self.graph.value_def(*value);
                write!(
                    f,
                    "bad value kind for output {output_idx}, expected one of {}, got `{}`",
                    display_expected_kinds(expected),
                    self.graph.value_kind(*value)
                )?;
            }
            FunctionVerifierError::BadEntry(_) => {
                write!(f, "bad entry node")?;
            }
            FunctionVerifierError::MisplacedEntry(_) => {
                write!(f, "misplaced entry node")?;
            }
            FunctionVerifierError::ConstantOutOfRange(_) => {
                write!(f, "constant value out of range")?;
            }
            FunctionVerifierError::BadFillWidth(_) => {
                write!(f, "bad fill width")?;
            }
            FunctionVerifierError::BadStackSlotAlign(_) => {
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
    Func {
        function: Function,
        error: FunctionVerifierError,
    },
    ReusedFunctionName(String),
}

impl ModuleVerifierError {
    pub fn node<'a>(
        &self,
        module: &'a Module,
    ) -> Option<(&'a FunctionMetadata, &'a FunctionBody, Node)> {
        match self {
            Self::Func { function, error } => {
                let metadata = &module.metadata.functions()[*function];
                let body = &module.functions[*function].body;
                Some((metadata, body, error.node(&body.graph)))
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
            ModuleVerifierError::Func { function, error } => {
                let function = &self.module.functions[*function];
                write!(f, "{}", error.display(&function.body.graph))
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

impl fmt::Display for DisplayErrorWithContext<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some((metadata, body, node)) = self.error.node(self.module) {
            write!(
                f,
                "in function `{}`: `{}`: {}",
                metadata.name,
                display_node(&self.module.metadata, body, node),
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

    for extern_function_data in module.metadata.extern_functions().values() {
        check_name(&extern_function_data.name, &mut errors);
    }

    for function in module.metadata.functions().keys() {
        let func_borrow = module.borrow_function(function);
        check_name(&func_borrow.metadata.name, &mut errors);
        if let Err(graph_errors) = verify_func(&module.metadata, func_borrow) {
            errors.extend(
                graph_errors
                    .into_iter()
                    .map(|error| ModuleVerifierError::Func { function, error }),
            )
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

pub fn verify_func(
    module_metadata: &ModuleMetadata,
    func: FunctionBorrow<'_>,
) -> Result<(), Vec<FunctionVerifierError>> {
    let body = func.body();

    let mut errors = Vec::new();

    if body.graph.node_kind(body.entry) != &NodeKind::Entry {
        errors.push(FunctionVerifierError::BadEntry(body.entry));
    }

    for node in walk_graph(&body.graph, body.entry) {
        verify_node_kind(module_metadata, func, node, &mut errors);
        verify_control_outputs(&body.graph, node, &mut errors);
    }

    if !errors.is_empty() {
        // If there were type/CFG errors, don't move on and try to verify data flow.
        // This allows us to assume basic well-formedness during data flow verification.
        return Err(errors);
    }

    verify_dataflow(body, &mut errors);

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn verify_control_outputs(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    for output in graph.node_outputs(node) {
        if graph.value_kind(output) != DepValueKind::Control {
            continue;
        }

        let mut uses = graph.value_uses(output);
        if uses.next().is_none() {
            errors.push(FunctionVerifierError::UnusedControl(output));
            continue;
        }

        if uses.next().is_some() {
            errors.push(FunctionVerifierError::ReusedControl(output));
        }
    }
}

type InputLocScratch = SmallVec<[(DomTreeNode, u32); 4]>;
type ByNodeSchedule = SecondaryMap<Node, PackedOption<DomTreeNode>>;

fn verify_dataflow(body: &FunctionBody, errors: &mut Vec<FunctionVerifierError>) {
    let cfg_preorder = body.compute_cfg_preorder_info();
    let walk_info = body.compute_cfg_live_walk_info(&cfg_preorder);
    let ctx = ScheduleContext::new(&body.graph, &walk_info, &cfg_preorder.preorder);

    let domtree = DomTree::compute(&body.graph, body.entry);
    let mut scheduler = VerifierScheduler {
        domtree: &domtree,
        errors,
        input_loc_scratch: InputLocScratch::new(),
        schedule: ByNodeSchedule::new(),
    };

    scheduler.pin_nodes(&ctx);

    schedule_early(&ctx, &mut PostOrderContext::new(), |ctx, node| {
        assert!(scheduler.schedule[node].is_none());
        scheduler.schedule[node] = scheduler
            .get_last_scheduled_input(ctx, node)
            .map(|(loc, _input_idx)| loc)
            .unwrap_or(scheduler.domtree.root())
            .into();
    });

    for &cfg_node in ctx.cfg_preorder {
        let domtree_node = domtree.get_tree_node(cfg_node).unwrap();
        scheduler.verify_dataflow_cfg_node_inputs(&ctx, domtree_node);

        for phi in ctx.get_attached_phis(cfg_node) {
            scheduler.verify_dataflow_phi_node_inputs(&ctx, cfg_node, phi);
        }
    }
}

struct VerifierScheduler<'a> {
    domtree: &'a DomTree,
    errors: &'a mut Vec<FunctionVerifierError>,
    input_loc_scratch: InputLocScratch,
    schedule: ByNodeSchedule,
}

impl VerifierScheduler<'_> {
    fn pin_nodes(&mut self, ctx: &ScheduleContext<'_>) {
        for &cfg_node in ctx.cfg_preorder {
            let domtree_node = self
                .domtree
                .get_tree_node(cfg_node)
                .expect("live CFG node not in dominator tree");

            self.schedule[cfg_node] = domtree_node.into();
            for phi in ctx.get_attached_phis(cfg_node) {
                self.schedule[phi] = domtree_node.into();
            }
        }
    }

    fn verify_dataflow_cfg_node_inputs(
        &mut self,
        ctx: &ScheduleContext<'_>,
        domtree_node: DomTreeNode,
    ) {
        let cfg_node = self.domtree.get_cfg_node(domtree_node);
        let Some((last_scheduled_input, last_scheduled_input_idx)) =
            self.get_last_scheduled_input(ctx, cfg_node)
        else {
            // If we have no data inputs, everything is fine.
            return;
        };

        if !self.domtree.dominates(last_scheduled_input, domtree_node) {
            self.errors.push(FunctionVerifierError::UseNotDominated {
                node: cfg_node,
                input: last_scheduled_input_idx,
            })
        }
    }

    fn verify_dataflow_phi_node_inputs(
        &mut self,
        ctx: &ScheduleContext<'_>,
        cfg_node: Node,
        phi: Node,
    ) {
        let graph = ctx.graph;
        for (i, phi_input, ctrl_input) in izip!(
            1..,
            graph.node_inputs(phi).into_iter().skip(1),
            graph.node_inputs(cfg_node),
        ) {
            let Some(ctrl_tree_node) = self.schedule[graph.value_def(ctrl_input).0].expand() else {
                // Inputs from dead regions are actually okay: if we can never select the
                // corresponding value in the phi, we don't need to check any constraints on it.
                // This can happen in practice when existing regions in the code are made dead
                // by optimizations.
                continue;
            };

            let input_node = graph.value_def(phi_input).0;

            let Some(input_sched_tree_node) = self.schedule[input_node].expand() else {
                // Having dead *inputs* for a non-dead control edge, on the other hand, is
                // problematic. Report that now.
                self.errors.push(FunctionVerifierError::UseNotDominated {
                    node: phi,
                    input: i,
                });
                continue;
            };

            // Finally, we can actually check that the phi input is scheduled before the
            // corresponding control input.
            if !self
                .domtree
                .dominates(input_sched_tree_node, ctrl_tree_node)
            {
                self.errors.push(FunctionVerifierError::UseNotDominated {
                    node: phi,
                    input: i,
                });
            }
        }
    }

    fn get_last_scheduled_input(
        &mut self,
        ctx: &ScheduleContext<'_>,
        node: Node,
    ) -> Option<(DomTreeNode, u32)> {
        let graph = ctx.graph;

        let input_loc_scratch = &mut self.input_loc_scratch;
        input_loc_scratch.clear();

        // Part 1: Find the schedule position for every input data value.
        for (i, input) in (0..).zip(graph.node_inputs(node)) {
            if !graph.value_kind(input).is_value() {
                continue;
            }

            let input_node = graph.value_def(input).0;

            let sched_node = self.schedule[input_node].expand();
            let Some(sched_node) = sched_node else {
                // If the node producing this input hasn't been scheduled, we either have a data flow
                // cycle or that node depends on a computation that is unreachable in the CFG. Both
                // scenarios are errors.
                self.errors
                    .push(FunctionVerifierError::UseNotDominated { node, input: i });
                continue;
            };
            // Save the input index so we can report errors accurately later.
            input_loc_scratch.push((sched_node, i));
        }

        // Part 2: Check that input schedules are totally ordered with respect to dominance, and find
        // the maximum with respect to that relation.
        // We use a simple insertion sort for doing both things.

        for i in 1..input_loc_scratch.len() {
            // Insert `input_locs[i]` into the sorted subslice `input_locs[0..i]`.
            for j in (0..i).rev() {
                let (prev, prev_input_idx) = input_loc_scratch[j];
                // Note: `j < i`, so `j + 1 <= i < len`.
                let (cur, cur_input_idx) = input_loc_scratch[j + 1];

                // TODO: We could save some redundant comparisons here by reintroducing `DomTree::compare`.
                if self.domtree.dominates(cur, prev) {
                    input_loc_scratch.swap(j, j + 1);
                } else if !self.domtree.dominates(prev, cur) {
                    // The inputs at positions `j` and `j + 1` are not ordered by dominance; report an
                    // error on one of them. We choose `j + 1` here so that nodes always appear to be
                    // placed based on earlier (lower-indexed) inputs.
                    self.errors.push(FunctionVerifierError::UseNotDominated {
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

        input_loc_scratch.last().copied()
    }
}

#[cfg(test)]
mod tests;

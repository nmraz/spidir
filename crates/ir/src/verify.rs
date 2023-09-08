use core::fmt;

use alloc::{borrow::ToOwned, string::String, vec, vec::Vec};
use cranelift_entity::{packed_option::PackedOption, EntitySet, SecondaryMap};
use fx_utils::FxHashSet;
use itertools::{izip, Itertools};
use smallvec::SmallVec;

use crate::{
    domtree::{self, DomTree, TreeNode},
    module::{Function, FunctionData, Module},
    node::{DepValueKind, NodeKind},
    valgraph::{DepValue, Node, ValGraph},
    valwalk::{walk_live_nodes, PostOrderContext, Succs},
    write::write_node,
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

fn display_node(module: &Module, graph: &ValGraph, node: Node) -> String {
    let mut s = String::new();
    write_node(&mut s, module, graph, node).unwrap();
    s
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

type InputSchedules = SmallVec<[(TreeNode, u32); 4]>;

fn verify_dataflow(graph: &ValGraph, entry: Node, errors: &mut Vec<GraphVerifierError>) {
    let domtree = domtree::compute(graph, entry);

    let mut schedule = SecondaryMap::new();
    let mut visited = EntitySet::new();
    let mut data_postorder = PostOrderContext::new(ValUseDefSuccs(graph), []);

    let mut input_schedules = InputSchedules::new();

    // Part 1: Walk down the dominator tree in preorder, and then walk back across use edges for
    // every visited control node (in postorder), excluding edges entering phis. Every new node
    // discovered must be "scheduled" (attached to a node in the dominator tree); specifically, each
    // node will be placed at the highest-scheduled (relative to dominance) one of its inputs.
    //
    // If cycles are found during the backward DFS, or a node's inputs' scheduling positions aren't
    // totally ordered by dominance, the appropriate error is reported.

    let root_tree_node = domtree.get_tree_node(entry).unwrap();
    let mut stack = vec![root_tree_node];
    while let Some(cfg_tree_node) = stack.pop() {
        stack.extend(domtree.children(cfg_tree_node));

        let cfg_node = domtree.get_cfg_node(cfg_tree_node);
        data_postorder.reset([cfg_node]);

        while let Some(node) = data_postorder.next_post(&mut visited) {
            let highest_scheduled_input = get_highest_scheduled_input(
                graph,
                node,
                &domtree,
                &schedule,
                &mut input_schedules,
                errors,
            );

            if has_ctrl_edges(graph, node) {
                // This node has explicit control edges, so schedule it exactly where the dominator
                // tree says it should be (which could also be nowhere).

                let tree_node = domtree.get_tree_node(node);
                if let (Some(tree_node), Some(highest_scheduled_input)) =
                    (tree_node, highest_scheduled_input)
                {
                    // We have both a dominator tree node dictating our schedule and a
                    // last-scheduled input; make sure we aren't trying to use something that can't
                    // be computed yet.
                    let (highest_scheduled_input, highest_scheduled_input_idx) =
                        highest_scheduled_input;
                    if !domtree.dominates(highest_scheduled_input, tree_node) {
                        // If the last (dominance-wise) input to be scheduled doesn't dominate
                        // this node itself, report the error now.
                        errors.push(GraphVerifierError::UseNotDominated {
                            node: cfg_node,
                            input: highest_scheduled_input_idx,
                        });
                    }
                }

                // This node already has an explicit control edge forcing its schedule.
                // Not that if this node is unreachable in the CFG, it will still be unscheduled at
                // this point.
                schedule[node] = tree_node.into();
            } else {
                // This node doesn't have any control edges forcing its schedule; place it as early
                // as possible, falling back to immediately after the entry if it has no inputs.
                schedule[node] = highest_scheduled_input
                    .map_or(root_tree_node, |(highest_scheduled_input, _)| {
                        highest_scheduled_input
                    })
                    .into();
            }
        }
    }

    // Part 2: Now that everything is scheduled, check that phi inputs are legal by checking that
    // the node outputting the control edge relevant to each input is dominated by the computation
    // of the input.
    stack.clear();
    stack.push(root_tree_node);
    while let Some(cfg_tree_node) = stack.pop() {
        let cfg_node = domtree.get_cfg_node(cfg_tree_node);

        let Some(phisel) = graph
            .node_outputs(cfg_node)
            .into_iter()
            .find(|&output| graph.value_kind(output) == DepValueKind::PhiSelector)
        else {
            continue;
        };

        for (phi, _) in graph.value_uses(phisel) {
            if !visited.contains(phi) {
                // Ignore any dead phi nodes, they may not even be meaningful.
                continue;
            }

            for (i, phi_input, ctrl_input) in izip!(
                1..,
                graph.node_inputs(phi).into_iter().skip(1),
                graph.node_inputs(cfg_node),
            ) {
                let Some(ctrl_tree_node) = schedule
                    .get(graph.value_def(ctrl_input).0)
                    .and_then(|node| node.expand())
                else {
                    // Inputs from dead regions are actually okay: if we can never select the
                    // corresponding value in the phi, we don't need to check any constraints on it.
                    // This can happen in practice when existing regions in the code are made dead
                    // by optimizations.
                    continue;
                };

                let Some(input_sched_tree_node) = schedule
                    .get(graph.value_def(phi_input).0)
                    .and_then(|node| node.expand())
                else {
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
                if !domtree.dominates(input_sched_tree_node, ctrl_tree_node) {
                    errors.push(GraphVerifierError::UseNotDominated {
                        node: phi,
                        input: i,
                    });
                }
            }
        }
    }
}

fn get_highest_scheduled_input(
    graph: &ValGraph,
    node: Node,
    domtree: &DomTree,
    schedule: &SecondaryMap<Node, PackedOption<TreeNode>>,
    input_schedules: &mut InputSchedules,
    errors: &mut Vec<GraphVerifierError>,
) -> Option<(TreeNode, u32)> {
    input_schedules.clear();

    let follow_values = should_follow_values(graph, node);
    let inputs = graph.node_inputs(node);
    input_schedules.reserve(inputs.len());

    // Part 1: Find the schedule position for every non-control input value.
    for (i, input) in (0..).zip(inputs) {
        if !is_followable_input(graph, follow_values, input) {
            continue;
        }

        let sched_node = schedule
            .get(graph.value_def(input).0)
            .and_then(|sched_node| sched_node.expand());
        let Some(sched_node) = sched_node else {
            // If the node producing this input hasn't been scheduled, we either have a data flow
            // cycle or that node depends on a computation that is unreachable in the CFG. Both
            // scenarios are errors.
            errors.push(GraphVerifierError::UseNotDominated { node, input: i });
            continue;
        };
        // Save the input index so we can report errors accurately later.
        input_schedules.push((sched_node, i));
    }

    // Part 2: Check that input schedules are totally ordered with respect to dominance, and find
    // the maximum with respect to that relation.
    // We use a simple insertion sort for doing both things.

    for i in 1..input_schedules.len() {
        // Insert `input_schedules[i]` into the sorted subslice `input_schedules[0..i]`.
        for j in (0..i).rev() {
            let (prev, prev_input_idx) = input_schedules[j];
            // Note: `j < i`, so `j + 1 <= i < len`.
            let (cur, cur_input_idx) = input_schedules[j + 1];

            // TODO: We could save some redundant comparisons here by reintroducing `DomTree::compare`.
            if domtree.dominates(cur, prev) {
                input_schedules.swap(j, j + 1);
            } else if !domtree.dominates(prev, cur) {
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

    input_schedules.last().copied()
}

struct ValUseDefSuccs<'a>(&'a ValGraph);
impl Succs for ValUseDefSuccs<'_> {
    fn successors(&self, node: Node, mut f: impl FnMut(Node)) {
        let follow_values = should_follow_values(self.0, node);
        for input in self.0.node_inputs(node) {
            if is_followable_input(self.0, follow_values, input) {
                f(self.0.value_def(input).0);
            }
        }
    }
}

fn is_followable_input(graph: &ValGraph, follow_values: bool, input: DepValue) -> bool {
    let input_kind = graph.value_kind(input);
    !input_kind.is_control() && (follow_values || !input_kind.is_value())
}

fn should_follow_values(graph: &ValGraph, node: Node) -> bool {
    // Don't follow value inputs on phi nodes, since they should actually count as "used" at the
    // corresponding branch and not where the phi is.
    graph.node_kind(node) != &NodeKind::Phi
}

fn has_ctrl_edges(graph: &ValGraph, node: Node) -> bool {
    graph
        .node_inputs(node)
        .into_iter()
        .any(|input| graph.value_kind(input).is_control())
        || graph
            .node_outputs(node)
            .into_iter()
            .any(|input| graph.value_kind(input).is_control())
}

#[cfg(test)]
mod tests;

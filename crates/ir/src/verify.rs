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

struct DataflowPreds<'a> {
    graph: &'a ValGraph,
}
impl Succs for DataflowPreds<'_> {
    fn successors(&self, node: Node, mut f: impl FnMut(Node)) {
        // Never walk through pinned nodes when searching for predecessors; these nodes are
        // scheduled manually, and we don't want to look at their inputs at all if they are dead
        // in the CFG.
        // Note: this check is performed on the source node here and not on the destination nodes
        // below as we still want pinned nodes to be considered visited if their outputs are used.
        // We take advantage of this fact when checking phi nodes later.
        if is_pinned_node(self.graph, node) {
            return;
        }

        for input in self.graph.node_inputs(node) {
            if self.graph.value_kind(input).is_value() {
                f(self.graph.value_def(input).0);
            }
        }
    }
}

/// Scratch buffers that are reused throughout dataflow verification.
struct ScratchSpace<'a> {
    data_postorder: PostOrderContext<DataflowPreds<'a>>,
    input_schedules: InputSchedules,
}

/// Associates every node with a point in the reachable CFG at which it can be computed (if one
/// exists), and tracks all nodes which have already had scheduling attempted.
struct Schedule {
    schedule: SecondaryMap<Node, PackedOption<TreeNode>>,
    visited: EntitySet<Node>,
}

impl Schedule {
    fn get(&self, node: Node) -> Option<TreeNode> {
        self.schedule
            .get(node)
            .and_then(|tree_node| tree_node.expand())
    }

    fn set(&mut self, node: Node, sched_tree_node: impl Into<PackedOption<TreeNode>>) {
        self.schedule[node] = sched_tree_node.into();
    }
}

fn verify_dataflow(graph: &ValGraph, entry: Node, errors: &mut Vec<GraphVerifierError>) {
    let domtree = domtree::compute(graph, entry);

    let mut sched = Schedule {
        schedule: SecondaryMap::new(),
        visited: EntitySet::new(),
    };
    let mut scratch_space = ScratchSpace {
        data_postorder: PostOrderContext::new(DataflowPreds { graph }, []),
        input_schedules: InputSchedules::new(),
    };

    let domtree_preorder = get_domtree_preorder(&domtree);

    // Pass 1: Schedule all pinned nodes, which are nodes with control edges and phis.
    // Note: we are careful not to mark the nodes as visited, as some of the phis may be dead.
    for &domtree_node in &domtree_preorder {
        let cfg_node = domtree.get_cfg_node(domtree_node);
        sched.set(cfg_node, domtree_node);

        for phi in get_attached_phis(graph, cfg_node) {
            sched.set(phi, domtree_node);
        }
    }

    // Pass 2: Schedule all other nodes by performing a postorder DFS along *inputs* (tracing
    // def-use edges backwards), reporting any impossibilities as errors.
    for &domtree_node in &domtree_preorder {
        for input in graph.node_inputs(domtree.get_cfg_node(domtree_node)) {
            schedule_node_early(
                graph,
                &domtree,
                graph.value_def(input).0,
                &mut scratch_space,
                &mut sched,
                errors,
            );
        }
    }

    // Pass 3: Verify data inputs to CFG and phi nodes.
    for &domtree_node in &domtree_preorder {
        verify_dataflow_cfg_node_inputs(
            graph,
            &domtree,
            domtree_node,
            &mut scratch_space.input_schedules,
            &sched,
            errors,
        );
        let cfg_node = domtree.get_cfg_node(domtree_node);
        for phi in get_attached_phis(graph, cfg_node) {
            if !sched.visited.contains(phi) {
                // Skip any dead phis, they should never count as part of the graph anyway.
                continue;
            }
            verify_dataflow_phi_node_inputs(
                graph,
                &domtree,
                cfg_node,
                phi,
                &mut scratch_space,
                &mut sched,
                errors,
            );
        }
    }
}

fn schedule_node_early(
    graph: &ValGraph,
    domtree: &DomTree,
    node: Node,
    scratch_space: &mut ScratchSpace<'_>,
    sched: &mut Schedule,
    errors: &mut Vec<GraphVerifierError>,
) {
    scratch_space.data_postorder.reset([node]);
    while let Some(node) = scratch_space.data_postorder.next_post(&mut sched.visited) {
        if is_pinned_node(graph, node) {
            continue;
        }

        let last_scheduled_input = get_last_scheduled_input(
            graph,
            domtree,
            sched,
            node,
            &mut scratch_space.input_schedules,
            errors,
        )
        .map(|(last_scheduled_input, _)| last_scheduled_input);

        let sched_tree_node = last_scheduled_input.unwrap_or(domtree.root());
        sched.set(node, sched_tree_node);
    }
}

fn verify_dataflow_cfg_node_inputs(
    graph: &ValGraph,
    domtree: &DomTree,
    domtree_node: TreeNode,
    input_schedules: &mut InputSchedules,
    sched: &Schedule,
    errors: &mut Vec<GraphVerifierError>,
) {
    let cfg_node = domtree.get_cfg_node(domtree_node);
    let Some((last_scheduled_input, last_scheduled_input_idx)) =
        get_last_scheduled_input(graph, domtree, sched, cfg_node, input_schedules, errors)
    else {
        // If we have no data inputs, everything is fine.
        return;
    };

    if !domtree.dominates(last_scheduled_input, domtree_node) {
        errors.push(GraphVerifierError::UseNotDominated {
            node: cfg_node,
            input: last_scheduled_input_idx,
        })
    }
}

fn verify_dataflow_phi_node_inputs(
    graph: &ValGraph,
    domtree: &DomTree,
    cfg_node: Node,
    phi: Node,
    scratch_space: &mut ScratchSpace<'_>,
    sched: &mut Schedule,
    errors: &mut Vec<GraphVerifierError>,
) {
    for (i, phi_input, ctrl_input) in izip!(
        1..,
        graph.node_inputs(phi).into_iter().skip(1),
        graph.node_inputs(cfg_node),
    ) {
        let Some(ctrl_tree_node) = sched.get(graph.value_def(ctrl_input).0) else {
            // Inputs from dead regions are actually okay: if we can never select the
            // corresponding value in the phi, we don't need to check any constraints on it.
            // This can happen in practice when existing regions in the code are made dead
            // by optimizations.
            continue;
        };

        let input_node = graph.value_def(phi_input).0;

        // This input may not have been scheduled yet, because we skipped all phi inputs during
        // the initial traversal.
        schedule_node_early(graph, domtree, input_node, scratch_space, sched, errors);

        let Some(input_sched_tree_node) = sched.get(input_node) else {
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

fn get_last_scheduled_input(
    graph: &ValGraph,
    domtree: &DomTree,
    sched: &Schedule,
    node: Node,
    input_schedules: &mut InputSchedules,
    errors: &mut Vec<GraphVerifierError>,
) -> Option<(TreeNode, u32)> {
    input_schedules.clear();

    let inputs = graph.node_inputs(node);
    input_schedules.reserve(inputs.len());

    // Part 1: Find the schedule position for every input data value.
    for (i, input) in (0..).zip(inputs) {
        if !graph.value_kind(input).is_value() {
            continue;
        }

        let sched_node = sched.get(graph.value_def(input).0);
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

fn get_attached_phis(graph: &ValGraph, node: Node) -> impl Iterator<Item = Node> + '_ {
    graph
        .node_outputs(node)
        .into_iter()
        .filter(|&output| graph.value_kind(output) == DepValueKind::PhiSelector)
        .flat_map(|phisel| graph.value_uses(phisel))
        .map(|(phi, _)| phi)
}

fn get_domtree_preorder(domtree: &DomTree) -> Vec<TreeNode> {
    let mut preorder = Vec::new();
    let mut stack = vec![domtree.root()];
    while let Some(node) = stack.pop() {
        preorder.push(node);
        stack.extend(domtree.children(node));
    }
    preorder
}

fn is_pinned_node(graph: &ValGraph, node: Node) -> bool {
    graph
        .node_inputs(node)
        .into_iter()
        .any(|input| graph.value_kind(input).is_control_or_phisel())
        || graph
            .node_outputs(node)
            .into_iter()
            .any(|input| graph.value_kind(input).is_control())
}

#[cfg(test)]
mod tests;

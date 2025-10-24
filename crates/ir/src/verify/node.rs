use core::array;

use alloc::{borrow::ToOwned, vec::Vec};

use crate::{
    function::{FunctionBody, FunctionBorrow, SignatureRef},
    module::ModuleMetadata,
    node::{DepValueKind, FunctionRef, MemSize, NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
};

use super::FunctionVerifierError;

pub fn verify_node_kind(
    module_metadata: &ModuleMetadata,
    func: FunctionBorrow<'_>,
    node: Node,
    errors: &mut Vec<FunctionVerifierError>,
) {
    let graph = &func.body.graph;
    match graph.node_kind(node) {
        NodeKind::Entry => verify_entry(func, node, errors),
        NodeKind::Return => verify_return(func, node, errors),
        NodeKind::Region => verify_region(graph, node, errors),
        NodeKind::Unreachable => verify_unreachable(graph, node, errors),
        NodeKind::Phi => verify_phi(graph, node, errors),
        NodeKind::Iconst(val) => verify_iconst(graph, node, *val, errors),
        NodeKind::Iadd => verify_int_binop(graph, node, errors),
        NodeKind::Isub => verify_int_binop(graph, node, errors),
        NodeKind::And => verify_int_binop(graph, node, errors),
        NodeKind::Or => verify_int_binop(graph, node, errors),
        NodeKind::Xor => verify_int_binop(graph, node, errors),
        NodeKind::Shl => verify_shift(graph, node, errors),
        NodeKind::Lshr => verify_shift(graph, node, errors),
        NodeKind::Ashr => verify_shift(graph, node, errors),
        NodeKind::Imul => verify_int_binop(graph, node, errors),
        NodeKind::Sdiv => verify_int_div(graph, node, errors),
        NodeKind::Udiv => verify_int_div(graph, node, errors),
        NodeKind::Srem => verify_int_div(graph, node, errors),
        NodeKind::Urem => verify_int_div(graph, node, errors),
        NodeKind::Iext => verify_iext(graph, node, errors),
        NodeKind::Itrunc => verify_itrunc(graph, node, errors),
        NodeKind::Sfill(width) => verify_sfill(graph, node, *width, errors),
        NodeKind::Icmp(_) => verify_icmp(graph, node, errors),
        NodeKind::Fconst32(_) => verify_fconst32(graph, node, errors),
        NodeKind::Fconst64(_) => verify_fconst64(graph, node, errors),
        NodeKind::Fadd => verify_float_binop(graph, node, errors),
        NodeKind::Fsub => verify_float_binop(graph, node, errors),
        NodeKind::Fmul => verify_float_binop(graph, node, errors),
        NodeKind::Fdiv => verify_float_binop(graph, node, errors),
        NodeKind::Fcmp(_) => verify_fcmp(graph, node, errors),
        NodeKind::SintToFloat => verify_inttofloat(graph, node, errors),
        NodeKind::UintToFloat => verify_inttofloat(graph, node, errors),
        NodeKind::FloatToSint => verify_floattoint(graph, node, errors),
        NodeKind::FloatToUint => verify_floattoint(graph, node, errors),
        NodeKind::PtrOff => verify_ptroff(graph, node, errors),
        NodeKind::IntToPtr => verify_inttoptr(graph, node, errors),
        NodeKind::PtrToInt => verify_ptrtoint(graph, node, errors),
        NodeKind::Load(size) => verify_load(graph, node, *size, errors),
        NodeKind::Store(size) => verify_store(graph, node, *size, errors),
        NodeKind::StackSlot { align, .. } => verify_stack_slot(graph, node, *align, errors),
        NodeKind::BrCond => verify_brcond(graph, node, errors),
        NodeKind::FuncAddr(_) => verify_funcaddr(graph, node, errors),
        NodeKind::Call(func) => verify_call(module_metadata, graph, node, *func, errors),
        NodeKind::CallInd(sig) => verify_call_ind(func.body, node, *sig, errors),
    }
}

fn verify_entry(func: FunctionBorrow<'_>, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let body = func.body;
    let metadata = func.metadata;

    if node != body.entry {
        errors.push(FunctionVerifierError::MisplacedEntry(node));
        return;
    }

    let expected_output_count = metadata.sig.param_types.len() + 1;
    let outputs = body.graph.node_outputs(node);
    if outputs.len() != expected_output_count {
        errors.push(FunctionVerifierError::BadOutputCount {
            node,
            expected: expected_output_count as u32,
        });
        return;
    }

    let _ = verify_ctrl_output_kind(&body.graph, outputs[0], errors);
    for (output, &expected_ty) in outputs.into_iter().skip(1).zip(&metadata.sig.param_types) {
        let _ = verify_output_kind(
            &body.graph,
            output,
            &[DepValueKind::Value(expected_ty)],
            errors,
        );
    }
}

fn verify_return(func: FunctionBorrow<'_>, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let body = func.body;
    let metadata = func.metadata;

    match metadata.sig.ret_type {
        Some(ret_type) => {
            let Ok([]) = verify_node_arity(&body.graph, node, 2, errors) else {
                return;
            };
            let _ = verify_ctrl_input_kind(&body.graph, node, 0, errors);
            let _ = verify_input_kind(
                &body.graph,
                node,
                1,
                &[DepValueKind::Value(ret_type)],
                errors,
            );
        }
        None => {
            let Ok([]) = verify_node_arity(&body.graph, node, 1, errors) else {
                return;
            };
            let _ = verify_ctrl_input_kind(&body.graph, node, 0, errors);
        }
    }
}

fn verify_region(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([ctrl, phisel]) = verify_outputs(graph, node, errors) else {
        return;
    };

    let _ = verify_ctrl_output_kind(graph, ctrl, errors);
    let _ = verify_output_kind(graph, phisel, &[DepValueKind::PhiSelector], errors);

    for input in 0..graph.node_inputs(node).len() as u32 {
        let _ = verify_ctrl_input_kind(graph, node, input, errors);
    }
}

fn verify_unreachable(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([]) = verify_node_arity(graph, node, 1, errors) else {
        return;
    };
    let _ = verify_input_kind(graph, node, 0, &[DepValueKind::Control], errors);
}

fn verify_phi(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_outputs(graph, node, errors) else {
        return;
    };

    let inputs = graph.node_inputs(node);
    if inputs.is_empty() {
        errors.push(FunctionVerifierError::BadInputCount { node, expected: 1 });
        return;
    }

    if verify_input_kind(graph, node, 0, &[DepValueKind::PhiSelector], errors).is_err() {
        return;
    }

    let phisel = inputs[0];
    let phi_region = graph.value_def(phisel).0;

    let expected_input_count = graph.node_inputs(phi_region).len() + 1;
    if inputs.len() != expected_input_count {
        errors.push(FunctionVerifierError::BadInputCount {
            node,
            expected: expected_input_count as u32,
        });
        return;
    }

    if verify_value_output_kind(graph, result, errors).is_err() {
        return;
    }

    let result_kind = graph.value_kind(result);
    for input in 1..inputs.len() as u32 {
        let _ = verify_input_kind(graph, node, input, &[result_kind], errors);
    }
}

fn verify_iconst(graph: &ValGraph, node: Node, val: u64, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 0, errors) else {
        return;
    };

    if verify_intlike_output_kind(graph, result, errors).is_err() {
        return;
    }

    let result_type = graph.value_kind(result).as_value().expect("type verified");
    match result_type {
        Type::I32 => {
            if val > u32::MAX as u64 {
                errors.push(FunctionVerifierError::ConstantOutOfRange(node));
            }
        }
        Type::Ptr | Type::I64 => {
            // Value is already a `u64`; we always treat pointers like `u64`s.
        }
        _ => panic!("type should have been verified here"),
    }
}

fn verify_int_binop(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 2, errors) else {
        return;
    };

    if verify_integer_output_kind(graph, result, errors).is_err() {
        return;
    }

    let result_kind = graph.value_kind(result);
    let _ = verify_input_kind(graph, node, 0, &[result_kind], errors);
    let _ = verify_input_kind(graph, node, 1, &[result_kind], errors);
}

fn verify_shift(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 2, errors) else {
        return;
    };

    if verify_integer_output_kind(graph, result, errors).is_err()
        || verify_integer_input_kind(graph, node, 1, errors).is_err()
    {
        return;
    }

    let result_kind = graph.value_kind(result);
    let _ = verify_input_kind(graph, node, 0, &[result_kind], errors);
}

fn verify_int_div(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([out_ctrl, result]) = verify_node_arity(graph, node, 3, errors) else {
        return;
    };

    let _ = verify_ctrl_input_kind(graph, node, 0, errors);
    let _ = verify_ctrl_output_kind(graph, out_ctrl, errors);

    if verify_integer_output_kind(graph, result, errors).is_err() {
        return;
    }

    let result_kind = graph.value_kind(result);
    let _ = verify_input_kind(graph, node, 1, &[result_kind], errors);
    let _ = verify_input_kind(graph, node, 2, &[result_kind], errors);
}

fn verify_iext(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 1, errors) else {
        return;
    };
    let _ = verify_input_kind(graph, node, 0, &[DepValueKind::Value(Type::I32)], errors);
    let _ = verify_output_kind(graph, result, &[DepValueKind::Value(Type::I64)], errors);
}

fn verify_itrunc(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 1, errors) else {
        return;
    };
    let _ = verify_input_kind(graph, node, 0, &[DepValueKind::Value(Type::I64)], errors);
    let _ = verify_output_kind(graph, result, &[DepValueKind::Value(Type::I32)], errors);
}

fn verify_sfill(graph: &ValGraph, node: Node, width: u8, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 1, errors) else {
        return;
    };

    if verify_integer_output_kind(graph, result, errors).is_err() {
        return;
    }

    let result_kind = graph.value_kind(result);
    let _ = verify_input_kind(graph, node, 0, &[result_kind], errors);

    let bit_width = result_kind.as_value().unwrap().bit_width() as u8;

    if !(1..bit_width).contains(&width) {
        errors.push(FunctionVerifierError::BadFillWidth(node));
    }
}

fn verify_icmp(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 2, errors) else {
        return;
    };

    let _ = verify_integer_output_kind(graph, result, errors);

    if verify_intlike_input_kind(graph, node, 0, errors).is_err() {
        return;
    }

    let _ = verify_input_kind(
        graph,
        node,
        1,
        &[graph.value_kind(graph.node_inputs(node)[0])],
        errors,
    );
}

fn verify_fconst32(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 0, errors) else {
        return;
    };
    let _ = verify_output_kind(graph, result, &[DepValueKind::Value(Type::F32)], errors);
}

fn verify_fconst64(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 0, errors) else {
        return;
    };
    let _ = verify_output_kind(graph, result, &[DepValueKind::Value(Type::F64)], errors);
}

fn verify_float_binop(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 2, errors) else {
        return;
    };

    if verify_float_output_kind(graph, result, errors).is_err() {
        return;
    }

    let result_kind = graph.value_kind(result);
    let _ = verify_input_kind(graph, node, 0, &[result_kind], errors);
    let _ = verify_input_kind(graph, node, 1, &[result_kind], errors);
}

fn verify_fcmp(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 2, errors) else {
        return;
    };

    let _ = verify_integer_output_kind(graph, result, errors);

    if verify_float_input_kind(graph, node, 0, errors).is_err() {
        return;
    }

    let _ = verify_input_kind(
        graph,
        node,
        1,
        &[graph.value_kind(graph.node_inputs(node)[0])],
        errors,
    );
}

fn verify_inttofloat(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 1, errors) else {
        return;
    };
    let _ = verify_float_output_kind(graph, result, errors);
    let _ = verify_integer_input_kind(graph, node, 0, errors);
}

fn verify_floattoint(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 1, errors) else {
        return;
    };
    let _ = verify_integer_output_kind(graph, result, errors);
    let _ = verify_float_input_kind(graph, node, 0, errors);
}

fn verify_ptroff(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 2, errors) else {
        return;
    };
    let _ = verify_output_kind(graph, result, &[DepValueKind::Value(Type::Ptr)], errors);
    let _ = verify_input_kind(graph, node, 0, &[DepValueKind::Value(Type::Ptr)], errors);
    let _ = verify_input_kind(graph, node, 1, &[DepValueKind::Value(Type::I64)], errors);
}

fn verify_inttoptr(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 1, errors) else {
        return;
    };
    let _ = verify_output_kind(graph, result, &[DepValueKind::Value(Type::Ptr)], errors);
    let _ = verify_input_kind(graph, node, 0, &[DepValueKind::Value(Type::I64)], errors);
}

fn verify_ptrtoint(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 1, errors) else {
        return;
    };
    let _ = verify_output_kind(graph, result, &[DepValueKind::Value(Type::I64)], errors);
    let _ = verify_input_kind(graph, node, 0, &[DepValueKind::Value(Type::Ptr)], errors);
}

fn verify_load(
    graph: &ValGraph,
    node: Node,
    size: MemSize,
    errors: &mut Vec<FunctionVerifierError>,
) {
    let Ok([out_ctrl, result]) = verify_node_arity(graph, node, 2, errors) else {
        return;
    };
    let _ = verify_ctrl_output_kind(graph, out_ctrl, errors);
    let _ = verify_output_kind(graph, result, allowed_kinds_for_mem_size(size), errors);
    let _ = verify_ctrl_input_kind(graph, node, 0, errors);
    let _ = verify_input_kind(graph, node, 1, &[DepValueKind::Value(Type::Ptr)], errors);
}

fn verify_store(
    graph: &ValGraph,
    node: Node,
    size: MemSize,
    errors: &mut Vec<FunctionVerifierError>,
) {
    let Ok([out_ctrl]) = verify_node_arity(graph, node, 3, errors) else {
        return;
    };
    let _ = verify_ctrl_output_kind(graph, out_ctrl, errors);
    let _ = verify_ctrl_input_kind(graph, node, 0, errors);
    let _ = verify_input_kind(graph, node, 1, allowed_kinds_for_mem_size(size), errors);
    let _ = verify_input_kind(graph, node, 2, &[DepValueKind::Value(Type::Ptr)], errors);
}

fn allowed_kinds_for_mem_size(size: MemSize) -> &'static [DepValueKind] {
    match size {
        MemSize::S1 => &[
            DepValueKind::Value(Type::I32),
            DepValueKind::Value(Type::I64),
        ],
        MemSize::S2 => &[
            DepValueKind::Value(Type::I32),
            DepValueKind::Value(Type::I64),
        ],
        MemSize::S4 => &[
            DepValueKind::Value(Type::I32),
            DepValueKind::Value(Type::I64),
            DepValueKind::Value(Type::F32),
        ],
        MemSize::S8 => &[
            DepValueKind::Value(Type::I64),
            DepValueKind::Value(Type::F64),
            DepValueKind::Value(Type::Ptr),
        ],
    }
}

fn verify_stack_slot(
    graph: &ValGraph,
    node: Node,
    align: u32,
    errors: &mut Vec<FunctionVerifierError>,
) {
    let Ok([result]) = verify_node_arity(graph, node, 0, errors) else {
        return;
    };
    let _ = verify_output_kind(graph, result, &[DepValueKind::Value(Type::Ptr)], errors);
    if !align.is_power_of_two() {
        errors.push(FunctionVerifierError::BadStackSlotAlign(node));
    }
}

fn verify_brcond(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([taken_ctrl, nontaken_ctrl]) = verify_node_arity(graph, node, 2, errors) else {
        return;
    };
    let _ = verify_ctrl_output_kind(graph, taken_ctrl, errors);
    let _ = verify_ctrl_output_kind(graph, nontaken_ctrl, errors);
    let _ = verify_ctrl_input_kind(graph, node, 0, errors);
    let _ = verify_integer_input_kind(graph, node, 1, errors);
}

fn verify_call(
    module_metadata: &ModuleMetadata,
    graph: &ValGraph,
    node: Node,
    func: FunctionRef,
    errors: &mut Vec<FunctionVerifierError>,
) {
    let sig = &module_metadata.resolve_funcref(func).sig;
    let expected_input_count = sig.param_types.len() as u32 + 1;

    if let Some(ret_type) = sig.ret_type {
        let Ok([out_ctrl, retval]) = verify_node_arity(graph, node, expected_input_count, errors)
        else {
            return;
        };
        let _ = verify_ctrl_output_kind(graph, out_ctrl, errors);
        let _ = verify_output_kind(graph, retval, &[DepValueKind::Value(ret_type)], errors);
    } else {
        let Ok([out_ctrl]) = verify_node_arity(graph, node, expected_input_count, errors) else {
            return;
        };
        let _ = verify_ctrl_output_kind(graph, out_ctrl, errors);
    }

    let _ = verify_ctrl_input_kind(graph, node, 0, errors);
    for (i, &param_type) in (0..).zip(&sig.param_types) {
        let _ = verify_input_kind(
            graph,
            node,
            i + 1,
            &[DepValueKind::Value(param_type)],
            errors,
        );
    }
}

fn verify_funcaddr(graph: &ValGraph, node: Node, errors: &mut Vec<FunctionVerifierError>) {
    let Ok([result]) = verify_node_arity(graph, node, 0, errors) else {
        return;
    };
    let _ = verify_output_kind(graph, result, &[DepValueKind::Value(Type::Ptr)], errors);
}

fn verify_call_ind(
    body: &FunctionBody,
    node: Node,
    sig: SignatureRef,
    errors: &mut Vec<FunctionVerifierError>,
) {
    let sig = &body.call_ind_sigs[sig];
    let graph = &body.graph;

    let expected_input_count = sig.param_types.len() as u32 + 2;

    if let Some(ret_type) = sig.ret_type {
        let Ok([out_ctrl, retval]) = verify_node_arity(graph, node, expected_input_count, errors)
        else {
            return;
        };
        let _ = verify_ctrl_output_kind(graph, out_ctrl, errors);
        let _ = verify_output_kind(graph, retval, &[DepValueKind::Value(ret_type)], errors);
    } else {
        let Ok([out_ctrl]) = verify_node_arity(graph, node, expected_input_count, errors) else {
            return;
        };
        let _ = verify_ctrl_output_kind(graph, out_ctrl, errors);
    }

    let _ = verify_ctrl_input_kind(graph, node, 0, errors);
    let _ = verify_input_kind(graph, node, 1, &[DepValueKind::Value(Type::Ptr)], errors);
    for (i, &param_type) in (0..).zip(&sig.param_types) {
        let _ = verify_input_kind(
            graph,
            node,
            i + 2,
            &[DepValueKind::Value(param_type)],
            errors,
        );
    }
}

fn verify_node_arity<const O: usize>(
    graph: &ValGraph,
    node: Node,
    input_count: u32,
    errors: &mut Vec<FunctionVerifierError>,
) -> Result<[DepValue; O], ()> {
    let inputs = graph.node_inputs(node);

    if inputs.len() != input_count as usize {
        errors.push(FunctionVerifierError::BadInputCount {
            node,
            expected: input_count,
        });
        return Err(());
    }

    verify_outputs(graph, node, errors)
}

fn verify_outputs<const O: usize>(
    graph: &ValGraph,
    node: Node,
    errors: &mut Vec<FunctionVerifierError>,
) -> Result<[DepValue; O], ()> {
    let outputs = graph.node_outputs(node);
    if outputs.len() != O {
        errors.push(FunctionVerifierError::BadOutputCount {
            node,
            expected: O as u32,
        });
        return Err(());
    }

    Ok(array::from_fn(|i| outputs[i]))
}

fn verify_ctrl_input_kind(
    graph: &ValGraph,
    node: Node,
    input: u32,
    errors: &mut Vec<FunctionVerifierError>,
) -> Result<(), ()> {
    verify_input_kind(graph, node, input, &[DepValueKind::Control], errors)
}

fn verify_ctrl_output_kind(
    graph: &ValGraph,
    value: DepValue,
    errors: &mut Vec<FunctionVerifierError>,
) -> Result<(), ()> {
    verify_output_kind(graph, value, &[DepValueKind::Control], errors)
}

fn verify_integer_input_kind(
    graph: &ValGraph,
    node: Node,
    input: u32,
    errors: &mut Vec<FunctionVerifierError>,
) -> Result<(), ()> {
    verify_input_kind(graph, node, input, ALL_INTEGER_TYPES, errors)
}

fn verify_integer_output_kind(
    graph: &ValGraph,
    value: DepValue,
    errors: &mut Vec<FunctionVerifierError>,
) -> Result<(), ()> {
    verify_output_kind(graph, value, ALL_INTEGER_TYPES, errors)
}

fn verify_intlike_input_kind(
    graph: &ValGraph,
    node: Node,
    input: u32,
    errors: &mut Vec<FunctionVerifierError>,
) -> Result<(), ()> {
    verify_input_kind(graph, node, input, ALL_INTLIKE_TYPES, errors)
}

fn verify_intlike_output_kind(
    graph: &ValGraph,
    value: DepValue,
    errors: &mut Vec<FunctionVerifierError>,
) -> Result<(), ()> {
    verify_output_kind(graph, value, ALL_INTLIKE_TYPES, errors)
}

fn verify_float_input_kind(
    graph: &ValGraph,
    node: Node,
    input: u32,
    errors: &mut Vec<FunctionVerifierError>,
) -> Result<(), ()> {
    verify_input_kind(graph, node, input, ALL_FLOAT_TYPES, errors)
}

fn verify_float_output_kind(
    graph: &ValGraph,
    value: DepValue,
    errors: &mut Vec<FunctionVerifierError>,
) -> Result<(), ()> {
    verify_output_kind(graph, value, ALL_FLOAT_TYPES, errors)
}

fn verify_value_output_kind(
    graph: &ValGraph,
    result: DepValue,
    errors: &mut Vec<FunctionVerifierError>,
) -> Result<(), ()> {
    verify_output_kind(graph, result, ALL_VALUE_TYPES, errors)
}

fn verify_input_kind(
    graph: &ValGraph,
    node: Node,
    input: u32,
    expected_kinds: &[DepValueKind],
    errors: &mut Vec<FunctionVerifierError>,
) -> Result<(), ()> {
    if !expected_kinds.contains(&graph.value_kind(graph.node_inputs(node)[input as usize])) {
        errors.push(FunctionVerifierError::BadInputKind {
            node,
            input,
            expected: expected_kinds.to_owned(),
        });
        return Err(());
    }

    Ok(())
}

fn verify_output_kind(
    graph: &ValGraph,
    value: DepValue,
    expected_kinds: &[DepValueKind],
    errors: &mut Vec<FunctionVerifierError>,
) -> Result<(), ()> {
    if !expected_kinds.contains(&graph.value_kind(value)) {
        errors.push(FunctionVerifierError::BadOutputKind {
            value,
            expected: expected_kinds.to_owned(),
        });
        return Err(());
    }
    Ok(())
}

const ALL_VALUE_TYPES: &[DepValueKind] = &[
    DepValueKind::Value(Type::I32),
    DepValueKind::Value(Type::I64),
    DepValueKind::Value(Type::F64),
    DepValueKind::Value(Type::Ptr),
];

const ALL_INTLIKE_TYPES: &[DepValueKind] = &[
    DepValueKind::Value(Type::I32),
    DepValueKind::Value(Type::I64),
    DepValueKind::Value(Type::Ptr),
];

const ALL_INTEGER_TYPES: &[DepValueKind] = &[
    DepValueKind::Value(Type::I32),
    DepValueKind::Value(Type::I64),
];

const ALL_FLOAT_TYPES: &[DepValueKind] = &[
    DepValueKind::Value(Type::F32),
    DepValueKind::Value(Type::F64),
];

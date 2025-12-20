use smallvec::SmallVec;

use ir::{
    builder::BuilderExt,
    node::{IcmpKind, NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
};

use crate::{
    constfold::{
        fold_and, fold_ashr, fold_iadd, fold_icmp, fold_iext, fold_imul, fold_isub, fold_itrunc,
        fold_lshr, fold_or, fold_sdiv, fold_sfill, fold_shl, fold_srem, fold_udiv, fold_urem,
        fold_xor,
    },
    state::EditContext,
    utils::{match_iconst, replace_with_iconst},
};

macro_rules! fold_binop {
    ($ctx:expr, $output:expr, $a:expr, $b:expr, $func:ident) => {{
        let ty = $ctx.graph().value_kind($output).as_value().unwrap();
        replace_with_iconst($ctx, $output, $func(ty, $a, $b));
    }};
}

pub fn canonicalize_node(ctx: &mut EditContext<'_>, node: Node) {
    let graph = ctx.graph();
    match graph.node_kind(node) {
        NodeKind::Region => {
            let inputs = graph.node_inputs(node);

            // If we have a single-input region, remove it and any phi nodes attached to it.
            if inputs.len() == 1 {
                let in_ctrl = inputs[0];
                let [out_ctrl, out_phisel] = graph.node_outputs_exact(node);
                let phis: SmallVec<[_; 4]> =
                    graph.value_uses(out_phisel).map(|(node, _)| node).collect();

                for phi in phis {
                    let graph = ctx.graph();
                    let [_, input] = graph.node_inputs_exact(phi);
                    let [output] = graph.node_outputs_exact(phi);
                    ctx.replace_value(output, input);
                }

                ctx.replace_value_and_kill(out_ctrl, in_ctrl);
            }
        }
        NodeKind::Phi => {
            if let Some(input) = single_phi_input(graph, node) {
                // All inputs to this phi are actually the same value, so it can be removed.
                let [output] = graph.node_outputs_exact(node);
                ctx.replace_value(output, input);
            }
        }
        NodeKind::Iadd => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_binop!(ctx, output, a, b, fold_iadd),
                (_, Some(0)) => ctx.replace_value(output, a),
                (Some(_), None) => commute_node_inputs(ctx, node, a, b),
                _ => {}
            }
        }
        NodeKind::Isub => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);

            if a == b {
                replace_with_iconst(ctx, output, 0);
                return;
            }

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_binop!(ctx, output, a, b, fold_isub),
                (_, Some(0)) => ctx.replace_value(output, a),
                _ => {}
            }
        }
        NodeKind::And => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);
            let ty = graph.value_kind(output).as_value().unwrap();

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_binop!(ctx, output, a, b, fold_and),

                (_, Some(0)) => replace_with_iconst(ctx, output, 0),
                (_, Some(c)) if c == ty.all_ones_val() => ctx.replace_value(output, a),

                (Some(_), None) => commute_node_inputs(ctx, node, a, b),

                _ => {}
            }
        }
        NodeKind::Or => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);
            let ty = graph.value_kind(output).as_value().unwrap();

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_binop!(ctx, output, a, b, fold_or),

                (_, Some(0)) => ctx.replace_value(output, a),
                (_, Some(c)) if c == ty.all_ones_val() => replace_with_iconst(ctx, output, c),

                (Some(_), None) => commute_node_inputs(ctx, node, a, b),

                _ => {}
            }
        }
        NodeKind::Xor => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);

            if a == b {
                replace_with_iconst(ctx, output, 0);
                return;
            }

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_binop!(ctx, output, a, b, fold_xor),
                (_, Some(0)) => ctx.replace_value(output, a),

                (Some(_), None) => commute_node_inputs(ctx, node, a, b),

                _ => {}
            }
        }
        NodeKind::Shl => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_binop!(ctx, output, a, b, fold_shl),
                (Some(0), _) => replace_with_iconst(ctx, output, 0),
                (_, Some(0)) => ctx.replace_value(output, a),
                _ => {}
            }
        }
        NodeKind::Lshr => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_binop!(ctx, output, a, b, fold_lshr),
                (Some(0), _) => replace_with_iconst(ctx, output, 0),
                (_, Some(0)) => ctx.replace_value(output, a),
                _ => {}
            }
        }
        NodeKind::Ashr => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_binop!(ctx, output, a, b, fold_ashr),
                (Some(0), _) => replace_with_iconst(ctx, output, 0),
                (_, Some(0)) => ctx.replace_value(output, a),
                _ => {}
            }
        }
        NodeKind::Imul => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_binop!(ctx, output, a, b, fold_imul),

                (_, Some(0)) => replace_with_iconst(ctx, output, 0),
                (_, Some(1)) => ctx.replace_value(output, a),

                (_, Some(c)) if c.is_power_of_two() => {
                    let shift = c.trailing_zeros();
                    let shift = ctx.build_iconst(Type::I32, shift as u64);
                    let new_output = ctx.build_shl(a, shift);
                    ctx.replace_value(output, new_output);
                }

                (Some(_), None) => commute_node_inputs(ctx, node, a, b),

                _ => {}
            }
        }
        NodeKind::Sdiv => {
            let [ctrl_in, a, b] = graph.node_inputs_exact(node);
            let [ctrl_out, output] = graph.node_outputs_exact(node);
            let ty = graph.value_kind(output).as_value().unwrap();

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (a, Some(0)) => {
                    // Replace all divisions by 0 with 0/0.
                    if a != Some(0) {
                        let zero = ctx.build_iconst(ty, 0);
                        ctx.set_node_input(node, 1, zero);
                    }
                }
                (Some(a), Some(b)) => {
                    if let Some(quotient) = fold_sdiv(ty, a, b) {
                        replace_with_iconst(ctx, output, quotient);
                        ctx.replace_value_and_kill(ctrl_out, ctrl_in);
                    };
                }
                _ => {}
            }
        }
        NodeKind::Udiv => {
            let [ctrl_in, a, b] = graph.node_inputs_exact(node);
            let [ctrl_out, output] = graph.node_outputs_exact(node);
            let ty = graph.value_kind(output).as_value().unwrap();

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (a, Some(0)) => {
                    // Replace all divisions by 0 with 0/0.
                    if a != Some(0) {
                        let zero = ctx.build_iconst(ty, 0);
                        ctx.set_node_input(node, 1, zero);
                    }
                }
                (Some(a), Some(b)) => {
                    if let Some(quotient) = fold_udiv(ty, a, b) {
                        replace_with_iconst(ctx, output, quotient);
                        ctx.replace_value_and_kill(ctrl_out, ctrl_in);
                    };
                }
                _ => {}
            }
        }
        NodeKind::Srem => {
            let [ctrl_in, a, b] = graph.node_inputs_exact(node);
            let [ctrl_out, output] = graph.node_outputs_exact(node);
            let ty = graph.value_kind(output).as_value().unwrap();

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (a, Some(0)) => {
                    // Replace all divisions by 0 with 0/0.
                    if a != Some(0) {
                        let zero = ctx.build_iconst(ty, 0);
                        ctx.set_node_input(node, 1, zero);
                    }
                }
                (Some(a), Some(b)) => {
                    if let Some(remainder) = fold_srem(ty, a, b) {
                        replace_with_iconst(ctx, output, remainder);
                        ctx.replace_value_and_kill(ctrl_out, ctrl_in);
                    };
                }
                _ => {}
            }
        }
        NodeKind::Urem => {
            let [ctrl_in, a, b] = graph.node_inputs_exact(node);
            let [ctrl_out, output] = graph.node_outputs_exact(node);
            let ty = graph.value_kind(output).as_value().unwrap();

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (a, Some(0)) => {
                    // Replace all divisions by 0 with 0/0.
                    if a != Some(0) {
                        let zero = ctx.build_iconst(ty, 0);
                        ctx.set_node_input(node, 1, zero);
                    }
                }
                (Some(a), Some(b)) => {
                    if let Some(remainder) = fold_urem(ty, a, b) {
                        replace_with_iconst(ctx, output, remainder);
                        ctx.replace_value_and_kill(ctrl_out, ctrl_in);
                    };
                }
                _ => {}
            }
        }
        NodeKind::Iext => {
            let [input] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);
            if let Some(value) = match_iconst(graph, input) {
                replace_with_iconst(ctx, output, fold_iext(value));
            }
        }
        NodeKind::Itrunc => {
            let [input] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);
            if let Some(value) = match_iconst(graph, input) {
                replace_with_iconst(ctx, output, fold_itrunc(value));
            }
        }
        &NodeKind::Icmp(kind) => canonicalize_icmp(ctx, node, kind),
        &NodeKind::Sfill(width) => {
            let [input] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);
            let ty = graph.value_kind(output).as_value().unwrap();

            if let Some(value) = match_iconst(graph, input) {
                replace_with_iconst(ctx, output, fold_sfill(ty, value, width));
            }
        }
        NodeKind::PtrOff => {
            let [ptr, offset] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);

            if match_iconst(graph, offset) == Some(0) {
                ctx.replace_value(output, ptr);
            }
        }
        _ => {}
    }
}

fn single_phi_input(graph: &ValGraph, phi: Node) -> Option<DepValue> {
    let output = graph.node_outputs(phi)[0];
    let mut single_input = None;

    // The first input is the selector, the rest are incoming values.
    for input in graph.node_inputs(phi).into_iter().skip(1) {
        // Deal with larger phi cycles or "webs" involving loops. On reducible flowgraphs, this
        // simple added condition is sufficient to remove all non-minimal phis.
        if input == output {
            continue;
        }

        match single_input {
            Some(existing) => {
                if input != existing {
                    return None;
                }
            }
            None => single_input = Some(input),
        }
    }

    single_input
}

fn canonicalize_icmp(ctx: &mut EditContext<'_>, node: Node, kind: IcmpKind) {
    macro_rules! checked_op_signed {
        ($ty:expr, $a:expr, $b:expr, $op:ident) => {
            if $ty == Type::I32 {
                ($a as i32).$op($b as i32).map(|res| res as u32 as u64)
            } else {
                ($a as i64).$op($b as i64).map(|res| res as u64)
            }
        };
    }

    macro_rules! checked_op_unsigned {
        ($ty:expr, $a:expr, $b:expr, $op:ident) => {
            if $ty == Type::I32 {
                ($a as u32).$op($b as u32).map(|res| res as u64)
            } else {
                ($a).$op($b).map(|res| res)
            }
        };
    }

    let graph = ctx.graph();

    let [a, b] = graph.node_inputs_exact(node);
    let input_ty = graph.value_kind(a).as_value().unwrap();
    let [output] = graph.node_outputs_exact(node);

    match (match_iconst(graph, a), match_iconst(graph, b)) {
        (Some(a), Some(b)) => replace_with_iconst(ctx, output, fold_icmp(input_ty, kind, a, b)),

        (Some(_), None) if kind.is_commutative() => commute_node_inputs(ctx, node, a, b),

        (Some(a_val), None) => match kind {
            // Convert weak inequalities to strong inequalities/constants.
            IcmpKind::Sle => match checked_op_signed!(input_ty, a_val, 1, checked_sub) {
                Some(new_a_val) => {
                    let new_a = ctx.build_iconst(input_ty, new_a_val);
                    replace_with_icmp(ctx, output, IcmpKind::Slt, new_a, b);
                }
                None => replace_with_iconst(ctx, output, 1),
            },
            IcmpKind::Ule => match checked_op_unsigned!(input_ty, a_val, 1, checked_sub) {
                Some(new_a_val) => {
                    let new_a = ctx.build_iconst(input_ty, new_a_val);
                    replace_with_icmp(ctx, output, IcmpKind::Ult, new_a, b);
                }
                None => replace_with_iconst(ctx, output, 1),
            },

            // Simplify strong inequalities near boundaries.
            IcmpKind::Slt => simplify_icmp_gt_const_boundary(
                ctx,
                output,
                a,
                b,
                a_val,
                signed_min_for_ty(input_ty),
                signed_max_for_ty(input_ty),
            ),
            IcmpKind::Ult => simplify_icmp_gt_const_boundary(
                ctx,
                output,
                a,
                b,
                a_val,
                0,
                unsigned_max_for_ty(input_ty),
            ),

            _ => {}
        },

        (None, Some(b_val)) => match kind {
            // Convert weak inequalities to strong inequalities/constants.
            IcmpKind::Sle => match checked_op_signed!(input_ty, b_val, 1, checked_add) {
                Some(new_b_val) => {
                    let new_b = ctx.build_iconst(input_ty, new_b_val);
                    replace_with_icmp(ctx, output, IcmpKind::Slt, a, new_b);
                }
                None => replace_with_iconst(ctx, output, 1),
            },
            IcmpKind::Ule => match checked_op_unsigned!(input_ty, b_val, 1, checked_add) {
                Some(new_b_val) => {
                    let new_b = ctx.build_iconst(input_ty, new_b_val);
                    replace_with_icmp(ctx, output, IcmpKind::Ult, a, new_b);
                }
                None => replace_with_iconst(ctx, output, 1),
            },

            // Simplify strong inequalities near boundaries.
            IcmpKind::Slt => simplify_icmp_lt_const_boundary(
                ctx,
                output,
                a,
                b,
                b_val,
                signed_min_for_ty(input_ty),
                signed_max_for_ty(input_ty),
            ),
            IcmpKind::Ult => simplify_icmp_lt_const_boundary(
                ctx,
                output,
                a,
                b,
                b_val,
                0,
                unsigned_max_for_ty(input_ty),
            ),

            _ => {}
        },
        _ => {}
    }
}

fn simplify_icmp_gt_const_boundary(
    ctx: &mut EditContext<'_>,
    output: DepValue,
    a: DepValue,
    b: DepValue,
    a_val: u64,
    min: u64,
    max: u64,
) {
    if a_val == min {
        // Move the constant to the right now to avoid extra work later.
        replace_with_icmp(ctx, output, IcmpKind::Ne, b, a);
    } else if a_val == max - 1 {
        let ty = ctx.graph().value_kind(a).as_value().unwrap();
        let new_a = ctx.build_iconst(ty, max);
        // Move the constant to the right now to avoid extra work later.
        replace_with_icmp(ctx, output, IcmpKind::Eq, b, new_a);
    } else if a_val == max {
        replace_with_iconst(ctx, output, 0);
    }
}

fn simplify_icmp_lt_const_boundary(
    ctx: &mut EditContext<'_>,
    output: DepValue,
    a: DepValue,
    b: DepValue,
    b_val: u64,
    min: u64,
    max: u64,
) {
    if b_val == max {
        replace_with_icmp(ctx, output, IcmpKind::Ne, a, b);
    } else if b_val == min + 1 {
        let ty = ctx.graph().value_kind(a).as_value().unwrap();
        let new_b = ctx.build_iconst(ty, min);
        replace_with_icmp(ctx, output, IcmpKind::Eq, a, new_b);
    } else if b_val == min {
        replace_with_iconst(ctx, output, 0);
    }
}

fn replace_with_icmp(
    ctx: &mut EditContext<'_>,
    output: DepValue,
    kind: IcmpKind,
    a: DepValue,
    b: DepValue,
) {
    let output_ty = ctx.graph().value_kind(output).as_value().unwrap();
    let new_output = ctx.build_icmp(kind, output_ty, a, b);
    ctx.replace_value(output, new_output);
}

fn commute_node_inputs(ctx: &mut EditContext<'_>, node: Node, a: DepValue, b: DepValue) {
    ctx.set_node_input(node, 0, b);
    ctx.set_node_input(node, 1, a);
}

fn signed_min_for_ty(ty: Type) -> u64 {
    if ty == Type::I32 {
        i32::MIN as u32 as u64
    } else {
        i64::MIN as u64
    }
}

fn signed_max_for_ty(ty: Type) -> u64 {
    if ty == Type::I32 {
        i32::MAX as u32 as u64
    } else {
        i64::MAX as u64
    }
}

fn unsigned_max_for_ty(ty: Type) -> u64 {
    if ty == Type::I32 {
        u32::MAX as u64
    } else {
        u64::MAX
    }
}

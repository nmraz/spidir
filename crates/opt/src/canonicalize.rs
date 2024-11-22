use core::ops::{BitAnd, BitOr, BitXor};

use ir::{
    builder::BuilderExt,
    cache::NodeCache,
    function::FunctionBody,
    node::{IcmpKind, NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
};

use crate::reduce::{reduce_body, ReduceContext};

pub fn canonicalize(body: &mut FunctionBody, node_cache: &mut NodeCache) {
    reduce_body(body, node_cache, canonicalize_node);
}

macro_rules! fold_constant {
    ($ctx:expr, $output:expr, $a:expr, $b:expr, $func:ident) => {{
        let ty = $ctx.graph().value_kind($output).as_value().unwrap();
        let combined = if ty == Type::I32 {
            (($a as u32).$func($b as u32)) as u64
        } else {
            $a.$func($b)
        };
        replace_with_iconst($ctx, $output, combined);
    }};
}

fn canonicalize_node(ctx: &mut ReduceContext<'_>, node: Node) {
    let graph = ctx.graph();
    match graph.node_kind(node) {
        NodeKind::Iadd => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_constant!(ctx, output, a, b, wrapping_add),
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
                (Some(a), Some(b)) => fold_constant!(ctx, output, a, b, wrapping_sub),
                (_, Some(0)) => ctx.replace_value(output, a),
                _ => {}
            }
        }
        NodeKind::And => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);
            let ty = graph.value_kind(output).as_value().unwrap();

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_constant!(ctx, output, a, b, bitand),

                (_, Some(0)) => replace_with_iconst(ctx, output, 0),
                (_, Some(0xffffffff)) if ty == Type::I32 => ctx.replace_value(output, a),
                (_, Some(0xffffffffffffffff)) => ctx.replace_value(output, a),

                (Some(_), None) => commute_node_inputs(ctx, node, a, b),

                _ => {}
            }
        }
        NodeKind::Or => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);
            let ty = graph.value_kind(output).as_value().unwrap();

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_constant!(ctx, output, a, b, bitor),

                (_, Some(0)) => ctx.replace_value(output, a),
                (_, Some(0xffffffff)) if ty == Type::I32 => {
                    replace_with_iconst(ctx, output, 0xffffffff)
                }
                (_, Some(0xffffffffffffffff)) => {
                    replace_with_iconst(ctx, output, 0xffffffffffffffff)
                }

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
                (Some(a), Some(b)) => fold_constant!(ctx, output, a, b, bitxor),
                (_, Some(0)) => ctx.replace_value(output, a),

                (Some(_), None) => commute_node_inputs(ctx, node, a, b),

                _ => {}
            }
        }
        NodeKind::Shl => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => {
                    // Shifting past the bit width produces an unspecified value anyway, so it's
                    // easiest to just mask here.
                    fold_constant!(ctx, output, a, b as u32, wrapping_shl);
                }
                (Some(0), _) => replace_with_iconst(ctx, output, 0),
                (_, Some(0)) => ctx.replace_value(output, a),
                _ => {}
            }
        }
        NodeKind::Lshr => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => {
                    // Shifting past the bit width produces an unspecified value anyway, so it's
                    // easiest to just mask here.
                    fold_constant!(ctx, output, a, b as u32, wrapping_shr);
                }
                (Some(0), _) => replace_with_iconst(ctx, output, 0),
                (_, Some(0)) => ctx.replace_value(output, a),
                _ => {}
            }
        }
        NodeKind::Ashr => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => {
                    let ty = graph.value_kind(output).as_value().unwrap();
                    // Shifting past the bit width produces an unspecified value anyway, so it's
                    // easiest to just mask here.

                    // Note: make sure to shift signed values so we end up with an arithmetic shift.
                    let shifted = if ty == Type::I32 {
                        (a as i32).wrapping_shr(b as u32) as u32 as u64
                    } else {
                        (a as i64).wrapping_shr(b as u32) as u64
                    };
                    replace_with_iconst(ctx, output, shifted);
                }
                (Some(0), _) => replace_with_iconst(ctx, output, 0),
                (_, Some(0)) => ctx.replace_value(output, a),
                _ => {}
            }
        }
        NodeKind::Imul => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_constant!(ctx, output, a, b, wrapping_mul),

                (_, Some(0)) => replace_with_iconst(ctx, output, 0),
                (_, Some(1)) => ctx.replace_value(output, a),

                (_, Some(c)) if c.is_power_of_two() => {
                    let shift = c.trailing_zeros();
                    let shift = ctx.builder().build_iconst(Type::I32, shift as u64);
                    let new_output = ctx.builder().build_shl(a, shift);
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
                        let zero = ctx.builder().build_iconst(ty, 0);
                        ctx.set_node_input(node, 1, zero);
                    }
                }
                (Some(a), Some(b)) => {
                    let quotient = if ty == Type::I32 {
                        (a as i32)
                            .checked_div(b as i32)
                            .map(|quotient| quotient as u32 as u64)
                    } else {
                        (a as i64)
                            .checked_div(b as i64)
                            .map(|quotient| quotient as u64)
                    };

                    if let Some(quotient) = quotient {
                        replace_with_iconst(ctx, output, quotient);
                        ctx.kill_node(node);
                        ctx.replace_value(ctrl_out, ctrl_in);
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
                        let zero = ctx.builder().build_iconst(ty, 0);
                        ctx.set_node_input(node, 1, zero);
                    }
                }
                (Some(a), Some(b)) => {
                    let quotient = if ty == Type::I32 {
                        ((a as u32) / (b as u32)) as u64
                    } else {
                        a / (b)
                    };

                    replace_with_iconst(ctx, output, quotient);
                    ctx.kill_node(node);
                    ctx.replace_value(ctrl_out, ctrl_in);
                }
                _ => {}
            }
        }
        NodeKind::Iext => {
            let [input] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);
            if let Some(value) = match_iconst(graph, input) {
                replace_with_iconst(ctx, output, value);
            }
        }
        NodeKind::Itrunc => {
            let [input] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);
            if let Some(value) = match_iconst(graph, input) {
                replace_with_iconst(ctx, output, value as u32 as u64);
            }
        }
        &NodeKind::Icmp(kind) => canonicalize_icmp(ctx, node, kind),
        &NodeKind::Sfill(width) => {
            let [input] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);
            let ty = graph.value_kind(output).as_value().unwrap();

            if let Some(value) = match_iconst(graph, input) {
                let shift_width = 64 - width;

                let mut new_value = (((value << shift_width) as i64) >> shift_width) as u64;
                if ty == Type::I32 {
                    new_value = new_value as u32 as u64;
                }

                replace_with_iconst(ctx, output, new_value);
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

fn canonicalize_icmp(ctx: &mut ReduceContext<'_>, node: Node, kind: IcmpKind) {
    macro_rules! cmp_signed {
        ($ty:expr, $a:expr, $b:expr, $op:tt) => {
            if $ty == Type::I32 {
                ($a as i32) $op ($b as i32)
            } else {
                ($a as i64) $op ($b as i64)
            }
        };
    }

    macro_rules! checked_op_signed {
        ($ty:expr, $a:expr, $b:expr, $op:ident) => {
            if $ty == Type::I32 {
                ($a as i32).$op($b as i32).map(|res| res as u32 as u64)
            } else {
                ($a as i64).$op($b as i64).map(|res| res as u64)
            }
        };
    }

    let graph = ctx.graph();

    let [a, b] = graph.node_inputs_exact(node);
    let input_ty = graph.value_kind(a).as_value().unwrap();
    let [output] = graph.node_outputs_exact(node);

    match (match_iconst(graph, a), match_iconst(graph, b)) {
        (Some(a), Some(b)) => {
            let res = match kind {
                IcmpKind::Eq => a == b,
                IcmpKind::Ne => a != b,
                IcmpKind::Slt => cmp_signed!(input_ty, a, b, <),
                IcmpKind::Sle => cmp_signed!(input_ty, a, b, <=),
                // Note: these compares always behave the same for 32-bit and 64-bit values because
                // 32-bit `iconst` nodes are not allowed to have their high bits set.
                IcmpKind::Ult => a < b,
                IcmpKind::Ule => a <= b,
            };
            replace_with_iconst(ctx, output, res as u64);
        }

        (Some(_), None) if kind.is_commutative() => commute_node_inputs(ctx, node, a, b),

        (Some(a_val), None) => match kind {
            // Convert weak comparisons to strong comparisons/constants.
            IcmpKind::Sle => match checked_op_signed!(input_ty, a_val, 1, checked_sub) {
                Some(new_a_val) => {
                    let new_a = ctx.builder().build_iconst(input_ty, new_a_val);
                    replace_with_icmp(ctx, output, IcmpKind::Slt, new_a, b);
                }
                None => replace_with_iconst(ctx, output, 1),
            },
            IcmpKind::Ule => match a_val.checked_sub(1) {
                Some(new_a_val) => {
                    let new_a = ctx.builder().build_iconst(input_ty, new_a_val);
                    replace_with_icmp(ctx, output, IcmpKind::Ult, new_a, b);
                }
                None => replace_with_iconst(ctx, output, 1),
            },

            // Simplify strong comparisons near boundaries.
            IcmpKind::Slt => {
                if a_val == signed_min_for_ty(input_ty) {
                    // Move the constant to the right now to avoid extra work later.
                    replace_with_icmp(ctx, output, IcmpKind::Ne, b, a);
                } else if a_val == signed_max_for_ty(input_ty) {
                    replace_with_iconst(ctx, output, 0);
                }
            }
            IcmpKind::Ult => {
                if a_val == 0 {
                    // Move the constant to the right now to avoid extra work later.
                    replace_with_icmp(ctx, output, IcmpKind::Ne, b, a);
                } else if a_val == unsigned_max_for_ty(input_ty) {
                    replace_with_iconst(ctx, output, 0);
                }
            }

            _ => {}
        },

        (None, Some(b_val)) => match kind {
            // Convert weak comparisons to strong comparisons/constants.
            IcmpKind::Sle => match checked_op_signed!(input_ty, b_val, 1, checked_add) {
                Some(new_b_val) => {
                    let new_b = ctx.builder().build_iconst(input_ty, new_b_val);
                    replace_with_icmp(ctx, output, IcmpKind::Slt, a, new_b);
                }
                None => replace_with_iconst(ctx, output, 1),
            },
            IcmpKind::Ule => match b_val.checked_add(1) {
                Some(new_b_val) => {
                    let new_b = ctx.builder().build_iconst(input_ty, new_b_val);
                    replace_with_icmp(ctx, output, IcmpKind::Ult, a, new_b);
                }
                None => replace_with_iconst(ctx, output, 1),
            },

            // Simplify strong comparisons near boundaries.
            IcmpKind::Slt => {
                if b_val == signed_max_for_ty(input_ty) {
                    replace_with_icmp(ctx, output, IcmpKind::Ne, a, b);
                } else if b_val == signed_min_for_ty(input_ty) {
                    replace_with_iconst(ctx, output, 0);
                }
            }
            IcmpKind::Ult => {
                if b_val == unsigned_max_for_ty(input_ty) {
                    replace_with_icmp(ctx, output, IcmpKind::Ne, a, b);
                } else if b_val == 0 {
                    replace_with_iconst(ctx, output, 0);
                }
            }

            _ => {}
        },
        _ => {}
    }
}

fn replace_with_icmp(
    ctx: &mut ReduceContext<'_>,
    output: DepValue,
    kind: IcmpKind,
    a: DepValue,
    b: DepValue,
) {
    let output_ty = ctx.graph().value_kind(output).as_value().unwrap();
    let new_output = ctx.builder().build_icmp(kind, output_ty, a, b);
    ctx.replace_value(output, new_output);
}

fn replace_with_iconst(ctx: &mut ReduceContext<'_>, value: DepValue, iconst: u64) {
    let ty = ctx.graph().value_kind(value).as_value().unwrap();
    let iconst = ctx.builder().build_iconst(ty, iconst);
    ctx.replace_value(value, iconst);
}

fn commute_node_inputs(ctx: &mut ReduceContext<'_>, node: Node, a: DepValue, b: DepValue) {
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

fn match_iconst(graph: &ValGraph, value: DepValue) -> Option<u64> {
    if let (node, 0) = graph.value_def(value) {
        if let &NodeKind::IConst(value) = graph.node_kind(node) {
            return Some(value);
        }
    }

    None
}

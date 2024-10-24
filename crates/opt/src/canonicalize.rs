use core::ops::{BitAnd, BitOr, BitXor};

use ir::{
    builder::BuilderExt,
    cache::NodeCache,
    function::FunctionBody,
    node::{NodeKind, Type},
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

                (Some(_), None) => {
                    let new_output = ctx.builder().build_iadd(b, a);
                    ctx.replace_value(output, new_output);
                }

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

                (Some(_), None) => {
                    let new_output = ctx.builder().build_and(b, a);
                    ctx.replace_value(output, new_output);
                }

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

                (Some(_), None) => {
                    let new_output = ctx.builder().build_or(b, a);
                    ctx.replace_value(output, new_output);
                }

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

                (Some(_), None) => {
                    let new_output = ctx.builder().build_xor(b, a);
                    ctx.replace_value(output, new_output);
                }

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

                (Some(_), None) => {
                    let new_output = ctx.builder().build_imul(b, a);
                    ctx.replace_value(output, new_output);
                }

                _ => {}
            }
        }
        NodeKind::Sdiv => {
            let [ctrl_in, a, b] = graph.node_inputs_exact(node);
            let [ctrl_out, output] = graph.node_outputs_exact(node);

            if let (Some(a), Some(b)) = (match_iconst(graph, a), match_iconst(graph, b)) {
                let ty = graph.value_kind(output).as_value().unwrap();
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
        }
        NodeKind::Udiv => {
            let [ctrl_in, a, b] = graph.node_inputs_exact(node);
            let [ctrl_out, output] = graph.node_outputs_exact(node);

            if let (Some(a), Some(b)) = (match_iconst(graph, a), match_iconst(graph, b)) {
                let ty = graph.value_kind(output).as_value().unwrap();
                let quotient = if ty == Type::I32 {
                    (a as u32)
                        .checked_div(b as u32)
                        .map(|quotient| quotient as u64)
                } else {
                    a.checked_div(b)
                };

                if let Some(quotient) = quotient {
                    replace_with_iconst(ctx, output, quotient);
                    ctx.kill_node(node);
                    ctx.replace_value(ctrl_out, ctrl_in);
                };
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

fn replace_with_iconst(ctx: &mut ReduceContext<'_>, value: DepValue, iconst: u64) {
    let ty = ctx.graph().value_kind(value).as_value().unwrap();
    let iconst = ctx.builder().build_iconst(ty, iconst);
    ctx.replace_value(value, iconst);
}

fn match_iconst(graph: &ValGraph, value: DepValue) -> Option<u64> {
    if let (node, 0) = graph.value_def(value) {
        if let &NodeKind::IConst(value) = graph.node_kind(node) {
            return Some(value);
        }
    }

    None
}

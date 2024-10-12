use core::ops::BitAnd;

use crate::{
    builder::BuilderExt,
    cache::NodeCache,
    function::FunctionBody,
    node::{NodeKind, Type},
    reduce::{reduce_body, ReduceContext},
    valgraph::{DepValue, Node, ValGraph},
};

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

                (Some(0), _) => ctx.replace_value(output, b),
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

                (Some(0), _) | (_, Some(0)) => replace_with_iconst(ctx, output, 0),

                (Some(0xffffffff), _) if ty == Type::I32 => ctx.replace_value(output, b),
                (_, Some(0xffffffff)) if ty == Type::I32 => ctx.replace_value(output, a),

                (Some(0xffffffffffffffff), _) => ctx.replace_value(output, b),
                (_, Some(0xffffffffffffffff)) => ctx.replace_value(output, a),

                (Some(_), None) => {
                    let new_output = ctx.builder().build_and(b, a);
                    ctx.replace_value(output, new_output);
                }

                _ => {}
            }
        }
        NodeKind::Imul => {
            let [a, b] = graph.node_inputs_exact(node);
            let [output] = graph.node_outputs_exact(node);

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_constant!(ctx, output, a, b, wrapping_mul),
                (Some(0), _) | (_, Some(0)) => replace_with_iconst(ctx, output, 0),

                (Some(1), _) => ctx.replace_value(output, b),
                (_, Some(1)) => ctx.replace_value(output, a),

                (Some(_), None) => {
                    let new_output = ctx.builder().build_imul(b, a);
                    ctx.replace_value(output, new_output);
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

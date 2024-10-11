use core::array;

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

        let new_output = $ctx.builder().build_iconst(ty, combined);
        $ctx.replace_value($output, new_output);
    }};
}

fn canonicalize_node(ctx: &mut ReduceContext<'_>, node: Node) {
    let graph = ctx.graph();
    match graph.node_kind(node) {
        NodeKind::Iadd => {
            let [a, b] = node_inputs_exact(graph, node);
            let output = graph.node_outputs(node)[0];

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
            let [a, b] = node_inputs_exact(graph, node);
            let output = graph.node_outputs(node)[0];

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_constant!(ctx, output, a, b, wrapping_sub),
                (_, Some(0)) => ctx.replace_value(output, a),
                _ => {}
            }
        }
        NodeKind::Imul => {
            let [a, b] = node_inputs_exact(graph, node);
            let output = graph.node_outputs(node)[0];
            let ty = graph.value_kind(output).as_value().unwrap();

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_constant!(ctx, output, a, b, wrapping_mul),
                (Some(0), _) | (_, Some(0)) => {
                    let zero = ctx.builder().build_iconst(ty, 0);
                    ctx.replace_value(output, zero);
                }

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
            let [input] = node_inputs_exact(graph, node);
            let output = graph.node_outputs(node)[0];
            if let Some(value) = match_iconst(graph, input) {
                let new_output = ctx.builder().build_iconst(Type::I64, value);
                ctx.replace_value(output, new_output);
            }
        }
        NodeKind::Itrunc => {
            let [input] = node_inputs_exact(graph, node);
            let output = graph.node_outputs(node)[0];
            if let Some(value) = match_iconst(graph, input) {
                let new_output = ctx.builder().build_iconst(Type::I32, value as u32 as u64);
                ctx.replace_value(output, new_output);
            }
        }
        &NodeKind::Sfill(width) => {
            let [input] = node_inputs_exact(graph, node);
            let output = graph.node_outputs(node)[0];
            let ty = graph.value_kind(output).as_value().unwrap();

            if let Some(value) = match_iconst(graph, input) {
                let shift_width = 64 - width;

                let mut new_value = (((value << shift_width) as i64) >> shift_width) as u64;
                if ty == Type::I32 {
                    new_value = new_value as u32 as u64;
                }

                let new_output = ctx.builder().build_iconst(ty, new_value);
                ctx.replace_value(output, new_output);
            }
        }
        _ => {}
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

fn node_inputs_exact<const N: usize>(graph: &ValGraph, node: Node) -> [DepValue; N] {
    let inputs = graph.node_inputs(node);
    assert!(inputs.len() == N);
    array::from_fn(|i| inputs[i])
}

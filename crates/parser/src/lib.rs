#![cfg_attr(not(test), no_std)]

extern crate alloc;

use alloc::{borrow::ToOwned, boxed::Box, vec::Vec};

use fx_utils::FxHashMap;
use ir::{
    module::{ExternFunctionData, FunctionData, Module, Signature},
    node::{DepValueKind, IcmpKind, NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
};
use itertools::Itertools;
use pest::{
    error::{Error, ErrorVariant},
    iterators::Pair,
    Parser, Span,
};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct IrParser;

pub fn parse_module(input: &str) -> Result<Module, Box<Error<Rule>>> {
    let parsed = IrParser::parse(Rule::module, input)?
        .next()
        .expect("expected top-level module node");

    let mut module = Module::new();

    for item in parsed.into_inner() {
        match item.as_rule() {
            Rule::extfunc => {
                let (name, sig) = extract_name_signature(
                    item.into_inner()
                        .next()
                        .expect("external function should contain signature"),
                );
                module.extern_functions.push(ExternFunctionData {
                    name: name.to_owned(),
                    sig,
                });
            }
            Rule::func => {
                module.functions.push(extract_function(item)?);
            }
            Rule::EOI => {}
            _ => unreachable!("expected a top-level module item"),
        }
    }

    Ok(module)
}

struct DefListing<'a> {
    span: Span<'a>,
    kind: DepValueKind,
}

struct NodeListing<'a> {
    kind: NodeKind,
    defs: Vec<DefListing<'a>>,
    uses: Vec<Span<'a>>,
}

fn extract_function(func_pair: Pair<'_, Rule>) -> Result<FunctionData, Box<Error<Rule>>> {
    let mut inner = func_pair.into_inner();
    let sig_pair = inner.next().expect("function should have signature");
    assert!(sig_pair.as_rule() == Rule::signature);

    let (name, sig) = extract_name_signature(sig_pair);
    let graph_pair = inner.next().expect("function should have graph");
    let (graph, entry) = extract_graph(graph_pair)?;

    Ok(FunctionData {
        name: name.to_owned(),
        sig,
        valgraph: graph,
        entry_node: entry,
    })
}

fn extract_graph(graph_pair: Pair<'_, Rule>) -> Result<(ValGraph, Node), Box<Error<Rule>>> {
    let mut graph = ValGraph::new();
    let mut value_map = FxHashMap::<&str, DepValue>::default();

    let graph_start = graph_pair.as_span().start_pos();
    let node_listings = graph_pair
        .into_inner()
        .map(extract_node)
        .collect::<Result<Vec<_>, _>>()?;
    let mut nodes = Vec::new();

    if node_listings.is_empty() {
        return Err(Box::new(Error::new_from_pos(
            ErrorVariant::CustomError {
                message: "no nodes in graph".to_owned(),
            },
            graph_start,
        )));
    }

    // Pass 1: add nodes to graph
    for node_listing in &node_listings {
        let node = graph.create_node(
            node_listing.kind,
            [],
            node_listing.defs.iter().map(|def_listing| def_listing.kind),
        );
        nodes.push(node);

        let outputs = graph.node_outputs(node);
        for (name_span, output) in node_listing
            .defs
            .iter()
            .map(|def_listing| def_listing.span)
            .zip(outputs)
        {
            let name = &name_span.as_str()[1..];
            if value_map.contains_key(&name) {
                return Err(Box::new(Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: "value redefined".to_owned(),
                    },
                    name_span,
                )));
            }

            value_map.insert(name, output);
        }
    }

    // Pass 2: link inputs
    for (node, node_listing) in nodes.iter().zip(&node_listings) {
        for input_span in &node_listing.uses {
            let input_name = &input_span.as_str()[1..];
            let input = *value_map.get(&input_name).ok_or_else(|| {
                Box::new(Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: "undefined value".to_owned(),
                    },
                    *input_span,
                ))
            })?;

            graph.add_node_input(*node, input);
        }
    }

    Ok((graph, *nodes.first().expect("nodes should be nonempty")))
}

fn extract_node(node_pair: Pair<'_, Rule>) -> Result<NodeListing<'_>, Box<Error<Rule>>> {
    let mut inner = node_pair.into_inner().peekable();
    let defs = inner
        .peeking_take_while(|pair| pair.as_rule() == Rule::valdef)
        .map(extract_valdef)
        .collect();

    let kind = extract_node_kind(inner.next().expect("node should have kind"))?;

    let uses = inner
        .map(|pair| {
            assert!(pair.as_rule() == Rule::valname);
            pair.as_span()
        })
        .collect();

    Ok(NodeListing { kind, defs, uses })
}

fn extract_node_kind(node_kind_pair: Pair<'_, Rule>) -> Result<NodeKind, Box<Error<Rule>>> {
    match node_kind_pair.as_str() {
        "entry" => Ok(NodeKind::Entry),
        "return" => Ok(NodeKind::Return),
        "region" => Ok(NodeKind::Region),
        "phi" => Ok(NodeKind::Phi),
        "iadd" => Ok(NodeKind::Iadd),
        "isub" => Ok(NodeKind::Isub),
        "and" => Ok(NodeKind::And),
        "or" => Ok(NodeKind::Or),
        "xor" => Ok(NodeKind::Xor),
        "shl" => Ok(NodeKind::Shl),
        "lshr" => Ok(NodeKind::Lshr),
        "ashr" => Ok(NodeKind::Ashr),
        "imul" => Ok(NodeKind::Imul),
        "sdiv" => Ok(NodeKind::Sdiv),
        "udiv" => Ok(NodeKind::Udiv),
        "load" => Ok(NodeKind::Load),
        "store" => Ok(NodeKind::Store),
        "brcond" => Ok(NodeKind::BrCond),
        _ => extract_special_node_kind(node_kind_pair),
    }
}

fn extract_special_node_kind(node_kind_pair: Pair<'_, Rule>) -> Result<NodeKind, Box<Error<Rule>>> {
    let special_pair = node_kind_pair
        .into_inner()
        .next()
        .expect("expected special node rule");
    let rule = special_pair.as_rule();
    let inner_pair = special_pair
        .into_inner()
        .next()
        .expect("special node kind should have inner operand");

    let kind = match rule {
        Rule::iconst_nodekind => {
            let value = inner_pair.as_str().parse().map_err(|_| {
                Box::new(Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: "invalid integer literal".to_owned(),
                    },
                    inner_pair.as_span(),
                ))
            })?;
            NodeKind::IConst(value)
        }
        Rule::fconst_nodekind => {
            let value = inner_pair.as_str().parse().map_err(|_| {
                Box::new(Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: "invalid floating-point literal".to_owned(),
                    },
                    inner_pair.as_span(),
                ))
            })?;
            NodeKind::FConst(value)
        }
        Rule::icmp_nodekind => NodeKind::Icmp(extract_icmpkind(inner_pair)),
        Rule::call_nodekind => todo!(),
        _ => unreachable!("unknown special node kind {rule:?}"),
    };

    Ok(kind)
}

fn extract_icmpkind(icmpkind_pair: Pair<'_, Rule>) -> IcmpKind {
    match icmpkind_pair.as_str() {
        "eq" => IcmpKind::Eq,
        "ne" => IcmpKind::Ne,
        "slt" => IcmpKind::Slt,
        "sle" => IcmpKind::Sle,
        "ult" => IcmpKind::Ult,
        "ule" => IcmpKind::Ule,
        _ => unreachable!(),
    }
}

fn extract_valdef(valdef_pair: Pair<'_, Rule>) -> DefListing<'_> {
    let mut inner = valdef_pair.into_inner();

    let name_pair = inner.next().expect("valdef should have name");
    assert!(name_pair.as_rule() == Rule::valname);

    let kind_pair = inner.next().expect("valdef should have kind");
    assert!(kind_pair.as_rule() == Rule::valkind);
    let kind = extract_value_kind(kind_pair);

    DefListing {
        span: name_pair.as_span(),
        kind,
    }
}

fn extract_name_signature(sig_pair: Pair<'_, Rule>) -> (&str, Signature) {
    let mut inner = sig_pair.into_inner();

    let name_pair = inner.next().expect("parsed signature should have name");
    assert!(name_pair.as_rule() == Rule::funcname);
    // The first character here is the '@'.
    let name = &name_pair.as_str()[1..];

    let (ret_type_pair, param_type_pair) = {
        let next = inner.next().expect("expected return type or parameters");
        match next.as_rule() {
            Rule::r#type => {
                let param_type_pair = inner.next().expect("expected parameter types");
                assert!(param_type_pair.as_rule() == Rule::param_types);
                (Some(next), param_type_pair)
            }
            Rule::param_types => (None, next),
            rule => {
                unreachable!("unexpected rule in function signature: {rule:?}")
            }
        }
    };

    let ret_type = ret_type_pair.map(extract_type);
    let param_types: Vec<_> = param_type_pair.into_inner().map(extract_type).collect();

    (
        name,
        Signature {
            ret_type,
            param_types,
        },
    )
}

fn extract_value_kind(kind_pair: Pair<'_, Rule>) -> DepValueKind {
    let kind_str = kind_pair.as_str();

    let mut inner = kind_pair.into_inner();
    if let Some(child) = inner.next() {
        return DepValueKind::Value(extract_type(child));
    }

    match kind_str {
        "ctrl" => DepValueKind::Control,
        "phisel" => DepValueKind::PhiSelector,
        _ => unreachable!(),
    }
}

fn extract_type(type_pair: Pair<'_, Rule>) -> Type {
    assert!(type_pair.as_rule() == Rule::r#type);
    match type_pair.as_str() {
        "i32" => Type::I32,
        "i64" => Type::I64,
        "f64" => Type::F64,
        "ptr" => Type::Ptr,
        _ => unreachable!("unexpected type name"),
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use super::*;

    #[track_caller]
    fn check_module(module: &Module, expected: Expect) {
        expected.assert_eq(&module.to_string());
    }

    #[test]
    fn parse_extfunc_signature() {
        let module = parse_module(
            "
            extfunc @func1:i32(i64)
            extfunc @func2(i64, ptr, f64)
            extfunc @func3()
            extfunc @func4:ptr()
            extfunc @func5:i32(i32, i64)",
        )
        .expect("failed to parse module");

        check_module(
            &module,
            expect![[r#"
                extfunc @func1:i32(i64)
                extfunc @func2(i64, ptr, f64)
                extfunc @func3()
                extfunc @func4:ptr()
                extfunc @func5:i32(i32, i64)

            "#]],
        );
    }

    #[test]
    fn parse_loop_graph() {
        let module = parse_module(
            "
            func @sum_to_n:i32(i32) {
                %0:ctrl, %1:val(i32) = entry
                %10:val(i32) = iconst 1
                %2:val(i32) = iconst 0
                %3:val(i32) = icmp eq %1, %2
                %4:ctrl, %5:ctrl = brcond %0, %3
                %13:val(i32) = icmp eq %11, %2
                %14:ctrl, %15:ctrl = brcond %6, %13
                %16:ctrl, %17:phisel = region %4, %14
                %6:ctrl, %7:phisel = region %5, %15
                %8:val(i32) = phi %7, %1, %11
                %11:val(i32) = isub %8, %10
                %9:val(i32) = phi %7, %2, %12
                %12:val(i32) = iadd %9, %8
                %18:val(i32) = phi %17, %2, %12
                return %16, %18
            }",
        )
        .expect("failed to parse module");

        check_module(
            &module,
            expect![[r#"

            func @sum_to_n:i32(i32) {
                %0:ctrl, %1:val(i32) = entry
                %2:val(i32) = iconst 1
                %3:val(i32) = iconst 0
                %4:val(i32) = icmp eq %1, %3
                %5:ctrl, %6:ctrl = brcond %0, %4
                %7:val(i32) = icmp eq %15, %3
                %8:ctrl, %9:ctrl = brcond %12, %7
                %10:ctrl, %11:phisel = region %5, %8
                %12:ctrl, %13:phisel = region %6, %9
                %14:val(i32) = phi %13, %1, %15
                %15:val(i32) = isub %14, %2
                %16:val(i32) = phi %13, %3, %17
                %17:val(i32) = iadd %16, %14
                %18:val(i32) = phi %11, %3, %17
                return %10, %18
            }
        "#]],
        );
    }
}

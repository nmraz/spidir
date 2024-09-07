#![cfg_attr(not(test), no_std)]

extern crate alloc;

use core::str::FromStr;

use alloc::{
    borrow::{Cow, ToOwned},
    boxed::Box,
    vec::Vec,
};

use fx_utils::FxHashMap;
use ir::{
    function::{FunctionBody, FunctionData, FunctionMetadata, Signature},
    module::{Function, Module},
    node::{BitwiseF64, DepValueKind, FunctionRef, IcmpKind, MemSize, NodeKind, Type},
    valgraph::DepValue,
};
use itertools::Itertools;
use pest::{
    error::{Error, ErrorVariant},
    iterators::{Pair, Pairs},
    Parser, Span,
};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct IrParser;

struct DefListing<'a> {
    span: Span<'a>,
    kind: DepValueKind,
}

struct NodeListing<'a> {
    kind: NodeKind,
    defs: Vec<DefListing<'a>>,
    uses: Vec<Span<'a>>,
}

struct ParsedFunction<'a> {
    name_span: Span<'a>,
    sig: Signature,
    graph_pair: Pair<'a, Rule>,
}

type FunctionNames<'a> = FxHashMap<Cow<'a, str>, FunctionRef>;

struct PendingFunction<'a> {
    id: Function,
    graph_pair: Pair<'a, Rule>,
}

pub fn parse_module(input: &str) -> Result<Module, Box<Error<Rule>>> {
    let parsed = IrParser::parse(Rule::module, input)?
        .next()
        .expect("expected top-level module node");

    let mut module = Module::new();
    let mut pending_functions = Vec::new();
    let mut function_names = FunctionNames::default();

    for item in parsed.into_inner() {
        match item.as_rule() {
            Rule::extfunc => {
                let (name_span, sig) = extract_name_signature(
                    item.into_inner()
                        .next()
                        .expect("external function should contain signature"),
                );
                let name = name_from_span(&name_span);
                if function_names.contains_key(&name) {
                    return Err(Box::new(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: "function redefined".to_owned(),
                        },
                        name_span,
                    )));
                }

                let function = module.extern_functions.push(FunctionMetadata {
                    name: name.clone().into_owned(),
                    sig,
                });
                function_names.insert(name, FunctionRef::External(function));
            }
            Rule::func => {
                let parsed = extract_function(item)?;
                let name = name_from_span(&parsed.name_span);
                if function_names.contains_key(&name) {
                    return Err(Box::new(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: "function redefined".to_owned(),
                        },
                        parsed.name_span,
                    )));
                }

                let function = module.functions.push(FunctionData {
                    metadata: FunctionMetadata {
                        name: name.clone().into_owned(),
                        sig: parsed.sig,
                    },
                    body: FunctionBody::new_invalid(),
                });

                function_names.insert(name, FunctionRef::Internal(function));
                pending_functions.push(PendingFunction {
                    id: function,
                    graph_pair: parsed.graph_pair,
                });
            }
            Rule::EOI => {}
            _ => unreachable!("expected a top-level module item"),
        }
    }

    for func in pending_functions {
        let func_data = &mut module.functions[func.id];
        extract_body(func.graph_pair, &function_names, &mut func_data.body)?;
    }

    Ok(module)
}

fn extract_function(func_pair: Pair<'_, Rule>) -> Result<ParsedFunction<'_>, Box<Error<Rule>>> {
    let mut inner = func_pair.into_inner();
    let sig_pair = inner.next().expect("function should have signature");
    assert!(sig_pair.as_rule() == Rule::signature);

    let (name, sig) = extract_name_signature(sig_pair);
    let graph_pair = inner.next().expect("function should have graph");

    Ok(ParsedFunction {
        name_span: name,
        sig,
        graph_pair,
    })
}

fn extract_body(
    graph_pair: Pair<'_, Rule>,
    function_names: &FunctionNames<'_>,
    body: &mut FunctionBody,
) -> Result<(), Box<Error<Rule>>> {
    let mut value_map = FxHashMap::<Cow<'_, str>, DepValue>::default();

    let graph_start = graph_pair.as_span().start_pos();
    let node_listings = graph_pair
        .into_inner()
        .map(|pair| extract_node(pair, function_names, body))
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

    let graph = &mut body.graph;

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
            let name = name_from_span(&name_span);
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
            let input_name = name_from_span(input_span);
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

    body.entry = *nodes.first().expect("nodes should be nonempty");
    Ok(())
}

fn extract_node<'a>(
    node_pair: Pair<'a, Rule>,
    function_names: &FunctionNames<'_>,
    body: &mut FunctionBody,
) -> Result<NodeListing<'a>, Box<Error<Rule>>> {
    let mut inner = node_pair.into_inner().peekable();
    let defs = inner
        .peeking_take_while(|pair| pair.as_rule() == Rule::valdef)
        .map(extract_valdef)
        .collect();

    let kind = extract_node_kind(
        inner.next().expect("node should have kind"),
        function_names,
        body,
    )?;

    let uses = inner
        .map(|pair| {
            assert!(pair.as_rule() == Rule::valname);
            pair.as_span()
        })
        .collect();

    Ok(NodeListing { kind, defs, uses })
}

fn extract_node_kind(
    node_kind_pair: Pair<'_, Rule>,
    function_names: &FunctionNames<'_>,
    body: &mut FunctionBody,
) -> Result<NodeKind, Box<Error<Rule>>> {
    match node_kind_pair.as_str() {
        "entry" => Ok(NodeKind::Entry),
        "return" => Ok(NodeKind::Return),
        "region" => Ok(NodeKind::Region),
        "unreachable" => Ok(NodeKind::Unreachable),
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
        "srem" => Ok(NodeKind::Srem),
        "urem" => Ok(NodeKind::Urem),
        "iext" => Ok(NodeKind::Iext),
        "itrunc" => Ok(NodeKind::Itrunc),
        "ptroff" => Ok(NodeKind::PtrOff),
        "brcond" => Ok(NodeKind::BrCond),
        _ => extract_special_node_kind(node_kind_pair, function_names, body),
    }
}

fn extract_special_node_kind(
    node_kind_pair: Pair<'_, Rule>,
    function_names: &FunctionNames<'_>,
    body: &mut FunctionBody,
) -> Result<NodeKind, Box<Error<Rule>>> {
    let special_pair = node_kind_pair
        .into_inner()
        .next()
        .expect("expected special node rule");
    let rule = special_pair.as_rule();
    let mut inner = special_pair.into_inner();

    let kind = match rule {
        Rule::iconst_nodekind => NodeKind::IConst(parse_from_str(
            &inner.next().unwrap(),
            "invalid integer literal",
        )?),
        Rule::fconst_nodekind => NodeKind::FConst(BitwiseF64(parse_from_str(
            &inner.next().unwrap(),
            "invalid floating-point literal",
        )?)),
        Rule::sfill_nodekind => NodeKind::Sfill(parse_from_str(
            &inner.next().unwrap(),
            "invalid fill width",
        )?),
        Rule::icmp_nodekind => NodeKind::Icmp(extract_icmpkind(inner.next().unwrap())),
        Rule::load_nodekind => NodeKind::Load(extract_mem_size(inner.next().unwrap())),
        Rule::store_nodekind => NodeKind::Store(extract_mem_size(inner.next().unwrap())),
        Rule::stackslot_nodekind => {
            let size = parse_from_str(&inner.next().unwrap(), "invalid stack slot size")?;
            let align = parse_from_str(&inner.next().unwrap(), "invalid stack slot align")?;
            NodeKind::StackSlot { size, align }
        }
        Rule::call_nodekind => {
            let name_span = inner.next().unwrap().as_span();
            let name = name_from_span(&name_span);
            let funcref = *function_names.get(&name).ok_or_else(|| {
                Box::new(Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: "undefined function".to_owned(),
                    },
                    name_span,
                ))
            })?;
            NodeKind::Call(funcref)
        }
        Rule::callind_nodekind => {
            let sig_pair = inner.next().unwrap();
            let sig = extract_callind_signature(sig_pair);
            let sig = body.call_ind_sigs.push(sig);
            NodeKind::CallInd(sig)
        }
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

fn extract_mem_size(mem_size_pair: Pair<'_, Rule>) -> MemSize {
    match mem_size_pair.as_str() {
        "1" => MemSize::S1,
        "2" => MemSize::S2,
        "4" => MemSize::S4,
        "8" => MemSize::S8,
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

fn extract_name_signature(sig_pair: Pair<'_, Rule>) -> (Span<'_>, Signature) {
    let mut inner = sig_pair.into_inner();

    let name_pair = inner.next().expect("parsed signature should have name");
    assert!(name_pair.as_rule() == Rule::funcname);
    let name = name_pair.as_span();
    let sig = extract_signature(inner);

    (name, sig)
}

fn extract_callind_signature(sig_pair: Pair<'_, Rule>) -> Signature {
    assert!(sig_pair.as_rule() == Rule::callind_signature);
    extract_signature(sig_pair.into_inner())
}

fn extract_signature(mut pairs: Pairs<'_, Rule>) -> Signature {
    let (ret_type_pair, param_type_pair) = {
        let next = pairs.next().expect("expected return type or parameters");
        match next.as_rule() {
            Rule::r#type => {
                let param_type_pair = pairs.next().expect("expected parameter types");
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
    Signature {
        ret_type,
        param_types,
    }
}

fn name_from_span<'a>(span: &Span<'a>) -> Cow<'a, str> {
    let ident = &span.as_str()[1..];
    if ident.starts_with('"') {
        let quoted = &ident[1..ident.len() - 1];
        quoted.replace("\\\"", "\"").into()
    } else {
        ident.into()
    }
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

fn parse_from_str<T: FromStr>(pair: &Pair<'_, Rule>, message: &str) -> Result<T, Box<Error<Rule>>> {
    pair.as_str().parse().map_err(|_| {
        Box::new(Error::new_from_span(
            ErrorVariant::CustomError {
                message: message.to_owned(),
            },
            pair.as_span(),
        ))
    })
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use super::*;

    #[track_caller]
    fn check_module(module: &Module, expected: Expect) {
        expected.assert_eq(&module.to_string());
    }

    #[track_caller]
    fn check_parse_error(input: &str, expected: Expect) {
        let error = match parse_module(input) {
            Err(error) => error,
            Ok(_) => panic!("expected parse error"),
        };

        expected.assert_eq(&error.to_string());
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
        .unwrap();

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
    fn parse_func_signature() {
        let module = parse_module(
            "
            func @func1(i32) {
                %0:ctrl, %1:i32 = entry
            }

            func @func2() {
                %0:ctrl = entry
            }

            func @func3:ptr(i64, ptr, f64) {
                %0:ctrl, %1:i64, %2:ptr, %3:f64 = entry
                return %0, %2
            }",
        )
        .unwrap();

        check_module(
            &module,
            expect![[r#"

                func @func1(i32) {
                    %0:ctrl, %1:i32 = entry
                }

                func @func2() {
                    %0:ctrl = entry
                }

                func @func3:ptr(i64, ptr, f64) {
                    %0:ctrl, %1:i64, %2:ptr, %3:f64 = entry
                    return %0, %2
                }
            "#]],
        );
    }

    #[test]
    fn parse_quoted_ident() {
        let module = parse_module(
            r#"
            extfunc @"System.Test+Lol System.Test::Do(Lol[])"()
            extfunc @"embedded\backslash"()
            extfunc @"embedded\"quote"()

            func @"a function"() {
                %0:ctrl = entry
                return %0
            }
            "#,
        )
        .unwrap();

        check_module(
            &module,
            expect![[r#"
                extfunc @"System.Test+Lol System.Test::Do(Lol[])"()
                extfunc @"embedded\backslash"()
                extfunc @"embedded\"quote"()

                func @"a function"() {
                    %0:ctrl = entry
                    return %0
                }
            "#]],
        );
    }

    #[test]
    fn parse_quoted_val_name() {
        let module = parse_module(
            r#"
            func @func() {
                %"control value":ctrl = entry
                return %"control value"
            }
            "#,
        )
        .unwrap();

        check_module(
            &module,
            expect![[r#"

                func @func() {
                    %0:ctrl = entry
                    return %0
                }
            "#]],
        );
    }

    #[test]
    fn parse_simple_function() {
        let module = parse_module(
            "
            func @func:i32(i32) {
                %0:ctrl, %1:i32 = entry
                return %0, %1
            }",
        )
        .unwrap();
        check_module(
            &module,
            expect![[r#"

            func @func:i32(i32) {
                %0:ctrl, %1:i32 = entry
                return %0, %1
            }
        "#]],
        );
    }

    #[test]
    fn node_kind_roundtrip() {
        let node_strs = [
            "entry",
            "return",
            "region",
            "unreachable",
            "phi",
            "iconst 5",
            "iadd",
            "isub",
            "and",
            "or",
            "xor",
            "shl",
            "lshr",
            "ashr",
            "imul",
            "sdiv",
            "udiv",
            "srem",
            "urem",
            "iext",
            "itrunc",
            "sfill 16",
            "sfill 31",
            "icmp eq",
            "icmp ne",
            "icmp slt",
            "icmp sle",
            "icmp ult",
            "icmp ule",
            "fconst 2.71828",
            "ptroff",
            "load.1",
            "load.2",
            "load.4",
            "load.8",
            "store.1",
            "store.2",
            "store.4",
            "store.8",
            "brcond",
            "callind ()",
            "callind i32()",
            "callind i32(ptr)",
            "callind i32(ptr, i64)",
            "callind (ptr)",
        ];

        for node_str in node_strs {
            let module_str = format!(
                "func @func() {{
                    {node_str}
                }}"
            );
            let roundtripped = parse_module(&module_str).unwrap().to_string();
            assert!(roundtripped.contains(node_str));
        }
    }

    #[test]
    fn parse_mixed_node_kinds() {
        let module = parse_module(
            "
            func @func:i32(ptr, i32) {
                %0:ctrl, %1:ptr, %2:i32 = entry
                %3:ctrl, %4:phisel = region %0
                %5:i32 = phi %4, %2
                %6:i32 = iconst 1324
                %7:i32 = iadd %5, %6
                %8:i32 = isub %7, %6
                %9:i32 = and %8, %6
                %10:i32 = or %9, %6
                %11:i32 = xor %10, %6
                %12:i32 = shl %11, %6
                %13:i32 = lshr %12, %6
                %14:i32 = ashr %13, %6
                %15:i32 = imul %14, %6
                %16:ctrl, %17:i32 = sdiv %3, %15, %6
                %18:ctrl, %19:i32 = udiv %16, %17, %6
                %29:ctrl, %30:f64 = load.8 %18, %1
                %20:i64 = iext %19
                %21:i32 = itrunc %20
                %22:i32 = icmp eq %21, %6
                %23:i32 = icmp ne %22, %6
                %24:i32 = icmp slt %23, %6
                %25:i32 = icmp sle %24, %6
                %26:i32 = icmp ult %25, %6
                %27:i32 = icmp ule %26, %6
                %28:f64 = fconst 3.1415
                %31:ctrl = store.8 %29, %28, %1
                %32:ctrl, %33:ctrl = brcond %31, %27
                return %32, %27
                return %33, %2
            }",
        )
        .unwrap();

        check_module(
            &module,
            expect![[r#"

                func @func:i32(ptr, i32) {
                    %0:ctrl, %1:ptr, %2:i32 = entry
                    %3:ctrl, %4:phisel = region %0
                    %5:i32 = phi %4, %2
                    %6:i32 = iconst 1324
                    %7:i32 = iadd %5, %6
                    %8:i32 = isub %7, %6
                    %9:i32 = and %8, %6
                    %10:i32 = or %9, %6
                    %11:i32 = xor %10, %6
                    %12:i32 = shl %11, %6
                    %13:i32 = lshr %12, %6
                    %14:i32 = ashr %13, %6
                    %15:i32 = imul %14, %6
                    %16:ctrl, %17:i32 = sdiv %3, %15, %6
                    %18:ctrl, %19:i32 = udiv %16, %17, %6
                    %20:ctrl, %21:f64 = load.8 %18, %1
                    %22:i64 = iext %19
                    %23:i32 = itrunc %22
                    %24:i32 = icmp eq %23, %6
                    %25:i32 = icmp ne %24, %6
                    %26:i32 = icmp slt %25, %6
                    %27:i32 = icmp sle %26, %6
                    %28:i32 = icmp ult %27, %6
                    %29:i32 = icmp ule %28, %6
                    %30:f64 = fconst 3.1415
                    %31:ctrl = store.8 %20, %30, %1
                    %32:ctrl, %33:ctrl = brcond %31, %29
                    return %32, %29
                    return %33, %2
                }
            "#]],
        );
    }

    #[test]
    fn parse_function_out_of_order() {
        let module = parse_module(
            "
            func @add:i32(i32, i32) {
                %0:ctrl, %1:i32, %2:i32 = entry
                return %0, %3
                %3:i32 = iadd %1, %2
            }",
        )
        .unwrap();
        check_module(
            &module,
            expect![[r#"

            func @add:i32(i32, i32) {
                %0:ctrl, %1:i32, %2:i32 = entry
                %3:i32 = iadd %1, %2
                return %0, %3
            }
        "#]],
        );
    }

    #[test]
    fn parse_loop_graph() {
        let module = parse_module(
            "
            func @sum_to_n:i32(i32) {
                %0:ctrl, %1:i32 = entry
                %10:i32 = iconst 1
                %2:i32 = iconst 0
                %3:i32 = icmp eq %1, %2
                %4:ctrl, %5:ctrl = brcond %0, %3
                %13:i32 = icmp eq %11, %2
                %14:ctrl, %15:ctrl = brcond %6, %13
                %16:ctrl, %17:phisel = region %4, %14
                %6:ctrl, %7:phisel = region %5, %15
                %8:i32 = phi %7, %1, %11
                %11:i32 = isub %8, %10
                %9:i32 = phi %7, %2, %12
                %12:i32 = iadd %9, %8
                %18:i32 = phi %17, %2, %12
                return %16, %18
            }",
        )
        .unwrap();

        check_module(
            &module,
            expect![[r#"

                func @sum_to_n:i32(i32) {
                    %0:ctrl, %1:i32 = entry
                    %2:i32 = iconst 1
                    %3:i32 = iconst 0
                    %4:i32 = icmp eq %1, %3
                    %5:ctrl, %6:ctrl = brcond %0, %4
                    %12:ctrl, %13:phisel = region %6, %9
                    %14:i32 = phi %13, %1, %15
                    %17:i32 = iadd %16, %14
                    %16:i32 = phi %13, %3, %17
                    %15:i32 = isub %14, %2
                    %7:i32 = icmp eq %15, %3
                    %8:ctrl, %9:ctrl = brcond %12, %7
                    %10:ctrl, %11:phisel = region %5, %8
                    %18:i32 = phi %11, %3, %17
                    return %10, %18
                }
            "#]],
        );
    }

    #[test]
    fn parse_extern_call() {
        let module = parse_module(
            "
            extfunc @f:i32(i32)

            func @caller:i32() {
                %0:ctrl = entry
                %1:i32 = iconst 5
                %2:ctrl, %3:i32 = call @f %0, %1
                return %2, %3
            }",
        )
        .unwrap();
        check_module(
            &module,
            expect![[r#"
                extfunc @f:i32(i32)

                func @caller:i32() {
                    %0:ctrl = entry
                    %1:i32 = iconst 5
                    %2:ctrl, %3:i32 = call @f %0, %1
                    return %2, %3
                }
            "#]],
        );
    }

    #[test]
    fn parse_intern_call() {
        let module = parse_module(
            "
            func @f:i32(i32) {
                %0:ctrl, %1:i32 = entry
                %2:i32 = iconst 1
                %3:i32 = iadd %1, %2
                return %0, %3
            }

            func @caller:i32() {
                %0:ctrl = entry
                %1:i32 = iconst 5
                %2:ctrl, %3:i32 = call @f %0, %1
                return %2, %3
            }",
        )
        .unwrap();
        check_module(
            &module,
            expect![[r#"

                func @f:i32(i32) {
                    %0:ctrl, %1:i32 = entry
                    %2:i32 = iconst 1
                    %3:i32 = iadd %1, %2
                    return %0, %3
                }

                func @caller:i32() {
                    %0:ctrl = entry
                    %1:i32 = iconst 5
                    %2:ctrl, %3:i32 = call @f %0, %1
                    return %2, %3
                }
            "#]],
        );
    }

    #[test]
    fn parse_mutual_calls() {
        let module = parse_module(
            "
            func @f:i32(i32) {
                %entry:ctrl, %p1:i32 = entry
                %zero:i32 = iconst 0
                %cmp:i32 = icmp eq %p1, %zero
                %is_zero:ctrl, %is_nonzero:ctrl = brcond %entry, %cmp
                return %is_zero, %zero
                %call_ctrl:ctrl, %call_val:i32 = call @g %is_nonzero, %p1
                return %call_ctrl, %call_val
            }

            func @g:i32(i32) {
                %entry:ctrl, %p1:i32 = entry
                %zero:i32 = iconst 0
                %cmp:i32 = icmp eq %p1, %zero
                %is_zero:ctrl, %is_nonzero:ctrl = brcond %entry, %cmp
                return %is_zero, %zero
                %one:i32 = iconst 1
                %sub:i32 = isub %p1, %one
                %call_ctrl:ctrl, %call_val:i32 = call @f %is_nonzero, %sub
                return %call_ctrl, %call_val
            }",
        )
        .unwrap();
        check_module(
            &module,
            expect![[r#"

                func @f:i32(i32) {
                    %0:ctrl, %1:i32 = entry
                    %2:i32 = iconst 0
                    %3:i32 = icmp eq %1, %2
                    %4:ctrl, %5:ctrl = brcond %0, %3
                    return %4, %2
                    %6:ctrl, %7:i32 = call @g %5, %1
                    return %6, %7
                }

                func @g:i32(i32) {
                    %0:ctrl, %1:i32 = entry
                    %6:i32 = iconst 1
                    %7:i32 = isub %1, %6
                    %2:i32 = iconst 0
                    %3:i32 = icmp eq %1, %2
                    %4:ctrl, %5:ctrl = brcond %0, %3
                    return %4, %2
                    %8:ctrl, %9:i32 = call @f %5, %7
                    return %8, %9
                }
            "#]],
        );
    }

    #[test]
    fn parse_callind() {
        let module = parse_module(
            "
            func @call_virt:i32(ptr) {
                %ent:ctrl, %p:ptr = entry
                %ret:ctrl, %val:i32 = callind i32() %ent, %p
                return %ret, %val
            }",
        )
        .unwrap();
        check_module(
            &module,
            expect![[r#"

            func @call_virt:i32(ptr) {
                %0:ctrl, %1:ptr = entry
                %2:ctrl, %3:i32 = callind i32() %0, %1
                return %2, %3
            }
        "#]],
        );
    }

    #[test]
    fn parse_stack_slots() {
        let module = parse_module(
            "
            func @with_slots(i32, f64) {
                %0:ctrl, %1:i32, %2:f64 = entry
                %4:ptr = stackslot 8:8
                %3:ptr = stackslot 4:4
                %5:ctrl = store.4 %0, %1, %3
                %6:ctrl = store.8 %5, %2, %4
                return %6
            }",
        )
        .unwrap();

        check_module(
            &module,
            expect![[r#"

                func @with_slots(i32, f64) {
                    %0:ctrl, %1:i32, %2:f64 = entry
                    %3:ptr = stackslot 8:8
                    %4:ptr = stackslot 4:4
                    %5:ctrl = store.4 %0, %1, %4
                    %6:ctrl = store.8 %5, %2, %3
                    return %6
                }
            "#]],
        );
    }

    #[test]
    fn parse_iconst_out_of_range() {
        check_parse_error(
            "
            func @func:i32() {
                %0:ctrl = entry
                %1:i32 = iconst 123456789123456789123456789
                return %0, %1
            }",
            expect![[r#"
                 --> 4:33
                  |
                4 |                 %1:i32 = iconst 123456789123456789123456789
                  |                                 ^-------------------------^
                  |
                  = invalid integer literal"#]],
        );
    }

    #[test]
    fn parse_sfill_width_out_of_range() {
        check_parse_error(
            "
            func @func:i32(i32) {
                %0:ctrl, %1:i32 = entry
                %2:i32 = sfill 256 %1
                return %0, %2
            }
            ",
            expect![[r#"
                 --> 4:32
                  |
                4 |                 %2:i32 = sfill 256 %1
                  |                                ^-^
                  |
                  = invalid fill width"#]],
        );
    }

    #[test]
    fn parse_load_invalid_size() {
        check_parse_error(
            "
            func @func:i32(ptr) {
                %0:ctrl, %1:ptr = entry
                %2:ctrl, %3:i32 = load.3 %0, %1
                return %2, %3
            }
            ",
            expect![[r#"
                 --> 4:40
                  |
                4 |                 %2:ctrl, %3:i32 = load.3 %0, %1
                  |                                        ^---
                  |
                  = expected memsize"#]],
        );
    }

    #[test]
    fn parse_load_no_size() {
        check_parse_error(
            "
            func @func:i32(ptr) {
                %0:ctrl, %1:ptr = entry
                %2:ctrl, %3:i32 = load. %0, %1
                return %2, %3
            }
            ",
            expect![[r#"
                 --> 4:40
                  |
                4 |                 %2:ctrl, %3:i32 = load. %0, %1
                  |                                        ^---
                  |
                  = expected memsize"#]],
        );
    }

    #[test]
    fn parse_load_spaced_size() {
        check_parse_error(
            "
            func @func:i32(ptr) {
                %0:ctrl, %1:ptr = entry
                %2:ctrl, %3:i32 = load . 4 %0, %1
                return %2, %3
            }
            ",
            expect![[r#"
                 --> 4:35
                  |
                4 |                 %2:ctrl, %3:i32 = load . 4 %0, %1
                  |                                   ^---
                  |
                  = expected nodekind"#]],
        );
    }

    #[test]
    fn parse_stack_slot_size_out_of_range() {
        check_parse_error(
            "
            func @with_slots(i32, f64) {
                %0:ptr = stackslot 123456789123456789123456789:4
            }",
            expect![[r#"
                 --> 3:36
                  |
                3 |                 %0:ptr = stackslot 123456789123456789123456789:4
                  |                                    ^-------------------------^
                  |
                  = invalid stack slot size"#]],
        );
    }

    #[test]
    fn parse_stack_slot_align_out_of_range() {
        check_parse_error(
            "
            func @with_slots(i32, f64) {
                %0:ptr = stackslot 4:123456789123456789123456789
            }",
            expect![[r#"
                 --> 3:38
                  |
                3 |                 %0:ptr = stackslot 4:123456789123456789123456789
                  |                                      ^-------------------------^
                  |
                  = invalid stack slot align"#]],
        );
    }

    #[test]
    fn parse_empty_graph() {
        check_parse_error(
            "
        func @func() {
        }",
            expect![[r#"
                 --> 3:9
                  |
                3 |         }
                  |         ^---
                  |
                  = no nodes in graph"#]],
        );
    }

    #[test]
    fn parse_redefined_value() {
        check_parse_error(
            "
            func @func(i32) {
                %0:ctrl, %1:i32 = entry
                %1:i32 = iconst 5
            }",
            expect![[r#"
                 --> 4:17
                  |
                4 |                 %1:i32 = iconst 5
                  |                 ^^
                  |
                  = value redefined"#]],
        );
    }

    #[test]
    fn parse_undefined_value() {
        check_parse_error(
            "
            func @func:i32() {
                %0:ctrl = entry
                return %0, %1
            }",
            expect![[r#"
                 --> 4:28
                  |
                4 |                 return %0, %1
                  |                            ^^
                  |
                  = undefined value"#]],
        );
    }

    #[test]
    fn parse_quoted_name_unescaped_quote() {
        check_parse_error(
            r#"
            extfunc @"embedded"quote"()"#,
            expect![[r#"
                 --> 2:21
                  |
                2 |             extfunc @"embedded"quote"()
                  |                     ^---
                  |
                  = expected signature"#]],
        );
    }

    #[test]
    fn parse_duplicate_extern_functions() {
        check_parse_error(
            "
            extfunc @func()
            extfunc @func:i32()",
            expect![[r#"
                 --> 3:21
                  |
                3 |             extfunc @func:i32()
                  |                     ^---^
                  |
                  = function redefined"#]],
        );
    }

    #[test]
    fn parse_duplicate_extern_intern_functions() {
        check_parse_error(
            "
            extfunc @func()
            func @func:i32() {
                %0:ctrl = entry
                %1:i32 = iconst 1
                return %0, %1
            }",
            expect![[r#"
                 --> 3:18
                  |
                3 |             func @func:i32() {
                  |                  ^---^
                  |
                  = function redefined"#]],
        );
    }

    #[test]
    fn parse_duplicate_intern_functions() {
        check_parse_error(
            "
            func @func:i32() {
                %0:ctrl = entry
                %1:i32 = iconst 1
                return %0, %1
            }
            func @func:i32() {
                %0:ctrl = entry
                %1:i32 = iconst 1
                return %0, %1
            }",
            expect![[r#"
                 --> 7:18
                  |
                7 |             func @func:i32() {
                  |                  ^---^
                  |
                  = function redefined"#]],
        );
    }

    #[test]
    fn parse_undefined_function() {
        check_parse_error(
            "
            func @func:i32() {
                %0:ctrl = entry
                %1:ctrl, %2:i32 = call @myotherfunc %0
                return %1, %2
            }",
            expect![[r#"
                 --> 4:40
                  |
                4 |                 %1:ctrl, %2:i32 = call @myotherfunc %0
                  |                                        ^----------^
                  |
                  = undefined function"#]],
        );
    }

    #[test]
    fn parse_callind_no_signature() {
        check_parse_error(
            "
            func @func:i32(ptr) {
                %c:ctrl, %p:ptr = entry
                %r:ctrl, %v:i32 = callind %p
                return %r, %v
            }
            ",
            expect![[r#"
                 --> 4:43
                  |
                4 |                 %r:ctrl, %v:i32 = callind %p
                  |                                           ^---
                  |
                  = expected type"#]],
        );
    }
}

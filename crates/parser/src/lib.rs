#![cfg_attr(not(test), no_std)]

extern crate alloc;

use alloc::{borrow::ToOwned, boxed::Box, vec::Vec};

use ir::{
    module::{ExternFunctionData, Module, Signature},
    node::Type,
};
use pest::{error::Error, iterators::Pair, Parser};
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
            Rule::func => {}
            Rule::EOI => {}
            _ => unreachable!("expected a top-level module item"),
        }
    }

    Ok(module)
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
}

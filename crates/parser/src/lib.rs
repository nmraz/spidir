#![cfg_attr(not(test), no_std)]

extern crate alloc;

use alloc::boxed::Box;

use pest::{error::Error, Parser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct IrParser;

pub fn parse_module(input: &str) -> Result<(), Box<Error<Rule>>> {
    let parsed = IrParser::parse(Rule::module, input)?
        .next()
        .expect("expected top-level module node");

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::parse_module;

    #[test]
    fn parse_simple_module() {
        parse_module(
            "
            extfunc @my_ext_func:i32(i64)
            func @my_func:i32(i64) {
                %0:ctrl, %1:val(i64) = entry
                %2:ctrl, %3:val(i32) = call extfunc @my_ext_func %0, %1
                return %2, %3
            }",
        )
        .expect("failed to parse module");
    }
}

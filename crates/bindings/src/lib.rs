#![cfg_attr(not(test), no_std)]

extern crate alloc;

mod builder;
mod codegen;
mod module;
mod opt;
mod types;
mod x64;

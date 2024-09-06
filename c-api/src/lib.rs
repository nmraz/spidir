#![no_std]

extern crate alloc;

mod builder;
mod codegen;
mod log;
mod module;
mod types;

#[cfg(not(test))]
mod platform;

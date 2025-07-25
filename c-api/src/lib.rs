#![no_std]

extern crate alloc;

// Make sure to explicitly import all defined symbols, because we don't actually use `bindings` in
// any of the Rust code.
extern crate bindings;

mod log;
mod platform;

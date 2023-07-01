#![cfg_attr(not(test), no_std)]

extern crate alloc;

pub mod builder;
pub mod module;
pub mod node;
pub mod valgraph;
pub mod valwalk;
pub mod verify;
pub mod write;

#[cfg(test)]
mod test_utils;

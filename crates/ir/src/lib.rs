#![cfg_attr(not(test), no_std)]

extern crate alloc;

pub mod builder;
pub mod cache;
pub mod domtree;
pub mod function;
pub mod loops;
pub mod module;
pub mod node;
pub mod schedule;
pub mod valgraph;
pub mod valwalk;
pub mod verify;
pub mod write;

#[cfg(test)]
mod test_utils;

#![cfg_attr(not(test), no_std)]

extern crate alloc;

pub mod blockorder;
pub mod cfg;
pub mod emit;
pub mod frame;
pub mod isel;
pub mod lir;
pub mod machine;
pub mod regalloc;
pub mod schedule;
pub mod target;

mod num_utils;

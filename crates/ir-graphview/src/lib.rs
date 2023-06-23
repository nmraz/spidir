#![cfg_attr(not(test), no_std)]

use core::fmt;

use ir::{
    valgraph::{Node, ValGraph},
    valwalk::LiveNodeInfo,
};

pub fn dump_ir_graphviz(graph: &ValGraph, entry: Node, w: &mut dyn fmt::Write) {
    for node in LiveNodeInfo::compute(graph, entry).reverse_postorder(graph) {
        todo!();
    }
}

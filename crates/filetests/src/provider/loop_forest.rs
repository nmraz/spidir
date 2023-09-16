use std::fmt::Write;

use anyhow::Result;
use ir::{domtree::DomTree, loop_forest::LoopForest, module::Module};

use crate::utils::write_graph_with_trailing_comments;

use super::{update_per_func_output, TestProvider, Updater};

pub struct LoopForestProvider;
impl TestProvider for LoopForestProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            let graph = &func.graph;
            let domtree = DomTree::compute(graph, func.entry);
            let loop_forest = LoopForest::compute(graph, &domtree);

            write_graph_with_trailing_comments(&mut output, module, func, |s, node| {
                if let Some(domtree_node) = domtree.get_tree_node(node) {
                    if let Some(containing_loop) = loop_forest.containing_loop(domtree_node) {
                        if domtree_node == loop_forest.loop_header(containing_loop) {
                            write!(
                                s,
                                "loop header: {}; root: {}; ",
                                containing_loop.as_u32(),
                                loop_forest.root_loop(containing_loop).as_u32()
                            )
                            .unwrap();
                            if let Some(parent_loop) = loop_forest.loop_parent(containing_loop) {
                                write!(s, "parent: {}; ", parent_loop.as_u32())?;
                            }
                        } else {
                            write!(s, "containing loop: {}; ", containing_loop.as_u32()).unwrap();
                        }
                    }
                }

                write!(s, "x").unwrap();
                Ok(())
            })
            .unwrap();
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, output_str: &str) -> Result<()> {
        update_per_func_output(updater, output_str)
    }
}

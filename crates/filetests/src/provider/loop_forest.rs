use std::fmt::Write;

use anyhow::{ensure, Result};
use ir::{domtree, loop_forest::LoopForest, module::Module};

use crate::utils::write_graph_with_trailing_comments;

use super::{update_per_func_output, TestProvider, Updater};

pub struct LoopForestProvider;
impl TestProvider for LoopForestProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            let graph = &func.graph;
            let domtree = domtree::compute(graph, func.entry);
            let loop_forest = LoopForest::compute(graph, &domtree);

            write_graph_with_trailing_comments(&mut output, module, func, |s, node| {
                if let Some(domtree_node) = domtree.get_tree_node(node) {
                    if let Some(containing_loop) = loop_forest.containing_loop(domtree_node) {
                        write!(s, "loop {}; ", containing_loop.as_u32()).unwrap();
                        if domtree_node == loop_forest.loop_header(containing_loop) {
                            let root_loop = loop_forest.root_loop(containing_loop);
                            write!(
                                s,
                                "header; depth {}; root {}; ",
                                loop_forest.loop_depth(containing_loop),
                                loop_forest.root_loop(containing_loop).as_u32()
                            )
                            .unwrap();
                            if let Some(parent_loop) = loop_forest.loop_parent(containing_loop) {
                                ensure!(loop_forest.root_loop(parent_loop) == root_loop);
                                write!(s, "parent {}; ", parent_loop.as_u32()).unwrap();
                            }
                        }

                        for loop_node in loop_forest.loop_ancestors(containing_loop) {
                            if loop_forest.is_latch(graph, &domtree, loop_node, domtree_node) {
                                write!(s, "latch {}; ", loop_node.as_u32()).unwrap();
                            }
                        }
                    }
                }

                write!(s, "x").unwrap();
                Ok(())
            })?;
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, output_str: &str) -> Result<()> {
        update_per_func_output(updater, output_str)
    }
}

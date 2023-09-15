use std::fmt::{self, Write};

use anyhow::Result;
use ir::{
    domtree::DomTree,
    loop_forest::LoopForest,
    module::Module,
    valgraph::{Node, ValGraph},
    write::{write_annotated_graph, write_annotated_node, AnnotateGraph},
};

use super::{update_per_func_output, TestProvider, Updater};

pub struct LoopForestProvider;
impl TestProvider for LoopForestProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            let graph = &func.graph;
            let domtree = DomTree::compute(graph, func.entry);
            let loop_forest = LoopForest::compute(graph, &domtree);
            let mut annotator = LoopAnnotator {
                domtree: &domtree,
                loop_forest: &loop_forest,
            };
            writeln!(output, "function `{}`:", func.name).unwrap();
            write_annotated_graph(&mut output, &mut annotator, module, graph, func.entry, 0)
                .unwrap();
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, output_str: &str) -> Result<()> {
        update_per_func_output(updater, output_str)
    }
}

struct LoopAnnotator<'a> {
    domtree: &'a DomTree,
    loop_forest: &'a LoopForest,
}

impl AnnotateGraph<String> for LoopAnnotator<'_> {
    fn write_node(
        &mut self,
        s: &mut String,
        module: &Module,
        graph: &ValGraph,
        node: Node,
    ) -> fmt::Result {
        write_annotated_node(s, self, module, graph, node)?;

        if let Some(domtree_node) = self.domtree.get_tree_node(node) {
            if let Some(containing_loop) = self.loop_forest.containing_loop(domtree_node) {
                write!(s, "  # ")?;
                if domtree_node == self.loop_forest.loop_header(containing_loop) {
                    write!(
                        s,
                        "loop header: {}, root: {}",
                        containing_loop.as_u32(),
                        self.loop_forest.root_loop(containing_loop).as_u32()
                    )?;
                    if let Some(parent_loop) = self.loop_forest.loop_parent(containing_loop) {
                        write!(s, ", parent: {}", parent_loop.as_u32())?;
                    }
                } else {
                    write!(s, "containing loop: {}", containing_loop.as_u32())?;
                }
            }
        }

        Ok(())
    }
}

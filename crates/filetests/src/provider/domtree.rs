use std::fmt::Write;

use anyhow::{Ok, Result};
use cranelift_entity::SecondaryMap;
use ir::{domtree::DomTree, module::Module, valwalk::LiveNodeInfo};
use itertools::Itertools;

use crate::utils::write_graph_with_trailing_comments;

use super::{update_per_func_output, TestProvider, Updater};

pub struct DomTreeProvider;
impl TestProvider for DomTreeProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            let graph = &func.graph;

            let domtree = DomTree::compute(graph, func.entry);
            let mut rpo_nums = SecondaryMap::new();
            for (i, &node) in LiveNodeInfo::compute(graph, func.entry)
                .reverse_postorder(graph)
                .iter()
                .enumerate()
            {
                rpo_nums[node] = i;
            }

            write_graph_with_trailing_comments(&mut output, module, func, |s, node| {
                let tree_node = domtree.get_tree_node(node);
                let idom = tree_node.and_then(|tree_node| domtree.idom(tree_node));
                let children =
                    tree_node.map_or(&[] as &[_], |tree_node| domtree.children(tree_node));

                write!(s, "{}; ", rpo_nums[node]).unwrap();

                if let Some(idom) = idom {
                    write!(s, "idom {}; ", rpo_nums[domtree.get_cfg_node(idom)]).unwrap();
                }

                if !children.is_empty() {
                    write!(
                        s,
                        "children {}; ",
                        children
                            .iter()
                            .map(|&child| rpo_nums[domtree.get_cfg_node(child)])
                            .format(", ")
                    )
                    .unwrap();
                }

                write!(s, "x").unwrap();
            });
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, output_str: &str) -> Result<()> {
        update_per_func_output(updater, output_str)
    }
}

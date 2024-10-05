use std::fmt::Write;

use anyhow::{Ok, Result};
use cranelift_entity::SecondaryMap;
use ir::{domtree::DomTree, module::Module};
use itertools::Itertools;

use crate::{
    provider::{update_per_func_output, SimpleTestProvider, Updater},
    utils::write_body_with_trailing_comments,
};

pub struct DomTreeProvider;
impl SimpleTestProvider for DomTreeProvider {
    fn output_for(&self, module: Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            let body = &func.body;

            let domtree = DomTree::compute(&body.graph, body.entry);
            let mut rpo_nums = SecondaryMap::new();
            for (i, &node) in body
                .compute_live_nodes()
                .reverse_postorder(&body.graph)
                .iter()
                .enumerate()
            {
                rpo_nums[node] = i;
            }

            write_body_with_trailing_comments(&mut output, &module, func, |s, node| {
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

    fn update(&self, updater: &mut Updater<'_>, output_str: &str) -> Result<()> {
        update_per_func_output(updater, output_str)
    }
}

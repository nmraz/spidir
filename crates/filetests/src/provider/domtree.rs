use std::fmt::Write;

use anyhow::{Ok, Result};
use cranelift_entity::SecondaryMap;
use ir::{domtree::DomTree, module::Module, valwalk::LiveNodeInfo, write::display_node};
use itertools::Itertools;

use super::{update_per_func_output, TestProvider, Updater};

pub struct DomTreeProvider;
impl TestProvider for DomTreeProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            writeln!(output, "function `{}`:", func.name).unwrap();

            let graph = &func.graph;

            let domtree = DomTree::compute(graph, func.entry);
            let mut rpo_nums = SecondaryMap::new();

            let lines: Vec<_> = LiveNodeInfo::compute(graph, func.entry)
                .reverse_postorder(graph)
                .iter()
                .enumerate()
                .map(|(rpo_num, &node)| {
                    rpo_nums[node] = rpo_num;
                    let s = display_node(module, graph, node).to_string();
                    let tree_node = domtree.get_tree_node(node);
                    (
                        node,
                        tree_node.map_or(&[] as &[_], |node| domtree.children(node)),
                        tree_node.and_then(|node| domtree.idom(node)),
                        s,
                    )
                })
                .collect();

            let longest_node_length = lines.iter().map(|(.., line)| line.len()).max().unwrap();

            for (node, children, idom, line) in lines {
                write!(output, "{line:longest_node_length$}").unwrap();
                write!(output, "  # {}", rpo_nums[node]).unwrap();

                if let Some(idom) = idom {
                    write!(output, "; idom {}", rpo_nums[domtree.get_cfg_node(idom)]).unwrap();
                }

                if !children.is_empty() {
                    write!(
                        output,
                        "; children {}",
                        children
                            .iter()
                            .map(|&child| rpo_nums[domtree.get_cfg_node(child)])
                            .format(", ")
                    )
                    .unwrap();
                }

                writeln!(output).unwrap();
            }
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, output_str: &str) -> Result<()> {
        update_per_func_output(updater, output_str)
    }
}

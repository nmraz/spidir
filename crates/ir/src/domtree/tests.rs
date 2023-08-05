use core::fmt::Write;

use cranelift_entity::SecondaryMap;
use expect_test::{expect, Expect};

use crate::{
    builder::{BuilderExt, SimpleBuilder},
    module::Module,
    test_utils::{create_const32, create_entry, create_loop_graph, create_region, create_return},
    valgraph::DepValue,
    valwalk::LiveNodeInfo,
    write::write_node,
};

use super::*;

#[track_caller]
fn check_idoms(graph: &ValGraph, entry: Node, expected: Expect) {
    let module = Module::new();

    let domtree = compute(graph, entry);
    let mut rpo_nums = SecondaryMap::new();

    let lines: Vec<_> = LiveNodeInfo::compute(graph, entry)
        .reverse_postorder(graph)
        .iter()
        .enumerate()
        .map(|(rpo_num, &node)| {
            rpo_nums[node] = rpo_num;
            let mut s = String::new();
            write_node(&mut s, &module, graph, node).unwrap();
            (node, domtree.idom(node), s)
        })
        .collect();

    let longest_node_length = lines
        .iter()
        .map(|(_node, _idom, line)| line.len())
        .max()
        .unwrap();

    let mut printed = String::new();
    for (node, idom, line) in lines {
        write!(printed, "{line:longest_node_length$}").unwrap();
        write!(printed, "  # {}", rpo_nums[node]).unwrap();
        if let Some(idom) = idom {
            write!(printed, ", idom {}", rpo_nums[idom]).unwrap();
        }
        writeln!(printed).unwrap();
    }

    expected.assert_eq(&printed);
}

fn create_graph() -> (ValGraph, Node, DepValue) {
    let mut graph = ValGraph::new();
    let (entry, ctrl, []) = create_entry(&mut graph, []);
    (graph, entry, ctrl)
}

fn create_brcond(graph: &mut ValGraph, ctrl: DepValue) -> (DepValue, DepValue) {
    let cond = create_const32(graph);
    let brcond = SimpleBuilder(graph).build_brcond(ctrl, cond);
    (brcond.true_ctrl, brcond.false_ctrl)
}

#[test]
fn straight_line_idoms() {
    let (mut graph, entry, ctrl) = create_graph();
    let ctrl = create_region(&mut graph, [ctrl]);
    let ctrl = create_region(&mut graph, [ctrl]);
    let ctrl = create_region(&mut graph, [ctrl]);
    create_return(&mut graph, [ctrl]);

    check_idoms(
        &graph,
        entry,
        expect![[r#"
            %0:ctrl = entry                 # 0
            %1:ctrl, %2:phisel = region %0  # 1, idom 0
            %3:ctrl, %4:phisel = region %1  # 2, idom 1
            %5:ctrl, %6:phisel = region %3  # 3, idom 2
            return %5                       # 4, idom 3
        "#]],
    );
}

#[test]
fn diamond_idoms() {
    let (mut graph, entry, ctrl) = create_graph();
    let (actrl, bctrl) = create_brcond(&mut graph, ctrl);
    let actrl = create_region(&mut graph, [actrl]);
    let bctrl = create_region(&mut graph, [bctrl]);
    let ctrl = create_region(&mut graph, [actrl, bctrl]);
    create_return(&mut graph, [ctrl]);

    check_idoms(
        &graph,
        entry,
        expect![[r#"
            %0:ctrl = entry                     # 0
            %1:i32 = iconst 5                   # 1
            %2:ctrl, %3:ctrl = brcond %0, %1    # 2, idom 0
            %4:ctrl, %5:phisel = region %2      # 3, idom 2
            %6:ctrl, %7:phisel = region %3      # 4, idom 2
            %8:ctrl, %9:phisel = region %4, %6  # 5, idom 2
            return %8                           # 6, idom 5
        "#]],
    );
}

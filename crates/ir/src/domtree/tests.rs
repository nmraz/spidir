use core::fmt::Write;

use cranelift_entity::SecondaryMap;
use expect_test::{expect, Expect};
use itertools::Itertools;

use crate::{
    builder::{BuilderExt, SimpleBuilder},
    module::Module,
    test_utils::{create_const32, create_entry, create_loop_graph, create_region, create_return},
    valgraph::DepValue,
    valwalk::LiveNodeInfo,
    write::display_node,
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
            let s = display_node(&module, graph, node).to_string();
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

    let mut printed = String::new();
    for (node, children, idom, line) in lines {
        write!(printed, "{line:longest_node_length$}").unwrap();
        write!(printed, "  # {}", rpo_nums[node]).unwrap();

        if let Some(idom) = idom {
            write!(printed, "; idom {}", rpo_nums[domtree.get_cfg_node(idom)]).unwrap();
        }

        if !children.is_empty() {
            write!(
                printed,
                "; children {}",
                children
                    .iter()
                    .map(|&child| rpo_nums[domtree.get_cfg_node(child)])
                    .format(", ")
            )
            .unwrap();
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
            %0:ctrl = entry                 # 0; children 1
            %1:ctrl, %2:phisel = region %0  # 1; idom 0; children 2
            %3:ctrl, %4:phisel = region %1  # 2; idom 1; children 3
            %5:ctrl, %6:phisel = region %3  # 3; idom 2; children 4
            return %5                       # 4; idom 3
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
            %0:ctrl = entry                     # 0; children 2
            %1:i32 = iconst 5                   # 1
            %2:ctrl, %3:ctrl = brcond %0, %1    # 2; idom 0; children 4, 5, 3
            %4:ctrl, %5:phisel = region %2      # 3; idom 2
            %6:ctrl, %7:phisel = region %3      # 4; idom 2
            %8:ctrl, %9:phisel = region %4, %6  # 5; idom 2; children 6
            return %8                           # 6; idom 5
        "#]],
    );
}

#[test]
fn sdom_not_idom() {
    let (mut graph, entry, ctrl) = create_graph();
    let (actrl, bctrl) = create_brcond(&mut graph, ctrl);
    let (cctrl, dctrl) = create_brcond(&mut graph, actrl);
    let ectrl = create_region(&mut graph, [bctrl, dctrl]);
    let fctrl = create_region(&mut graph, [cctrl, ectrl]);
    create_return(&mut graph, [fctrl]);

    check_idoms(
        &graph,
        entry,
        expect![[r#"
            %0:ctrl = entry                      # 0; children 3
            %4:i32 = iconst 5                    # 1
            %1:i32 = iconst 5                    # 2
            %2:ctrl, %3:ctrl = brcond %0, %1     # 3; idom 0; children 5, 6, 4
            %5:ctrl, %6:ctrl = brcond %2, %4     # 4; idom 3
            %7:ctrl, %8:phisel = region %3, %6   # 5; idom 3
            %9:ctrl, %10:phisel = region %5, %7  # 6; idom 3; children 7
            return %9                            # 7; idom 6
        "#]],
    );
}

#[test]
fn loop_idoms() {
    let (graph, entry) = create_loop_graph();
    check_idoms(
        &graph,
        entry,
        expect![[r#"
            %0:ctrl, %1:i32 = entry                # 0; children 4
            %10:i32 = iconst 1                     # 1
            %2:i32 = iconst 0                      # 2
            %3:i32 = icmp eq %1, %2                # 3
            %4:ctrl, %5:ctrl = brcond %0, %3       # 4; idom 0; children 8, 7
            %13:i32 = icmp eq %11, %2              # 5
            %14:ctrl, %15:ctrl = brcond %6, %13    # 6; idom 8
            %16:ctrl, %17:phisel = region %4, %14  # 7; idom 4; children 14
            %6:ctrl, %7:phisel = region %5, %15    # 8; idom 4; children 6
            %8:i32 = phi %7, %1, %11               # 9
            %11:i32 = isub %8, %10                 # 10
            %9:i32 = phi %7, %2, %12               # 11
            %12:i32 = iadd %9, %8                  # 12
            %18:i32 = phi %17, %2, %12             # 13
            return %16, %18                        # 14; idom 7
        "#]],
    );
}

#[test]
fn basic_cfg_reachable() {
    let (mut graph, entry, ctrl) = create_graph();
    let (actrl, bctrl) = create_brcond(&mut graph, ctrl);
    let actrl = create_region(&mut graph, [actrl]);
    let bctrl = create_region(&mut graph, [bctrl]);
    let dead_region = create_region(&mut graph, []);
    let ctrl = create_region(&mut graph, [actrl, bctrl, dead_region]);
    let ret = create_return(&mut graph, [ctrl]);

    let domtree = compute(&graph, entry);
    assert_eq!(domtree.root(), domtree.get_tree_node(entry).unwrap());
    assert!(domtree.is_cfg_reachable(entry));
    assert!(domtree.is_cfg_reachable(graph.value_def(actrl).0));
    assert!(domtree.is_cfg_reachable(graph.value_def(bctrl).0));
    assert!(domtree.is_cfg_reachable(ret));
    assert!(!domtree.is_cfg_reachable(graph.value_def(dead_region).0));
}

#[test]
fn straight_line_query() {
    let (mut graph, entry, ctrl0) = create_graph();
    let ctrl1 = create_region(&mut graph, [ctrl0]);
    let ctrl2 = create_region(&mut graph, [ctrl1]);
    let ctrl3 = create_region(&mut graph, [ctrl2]);
    let ret = create_return(&mut graph, [ctrl3]);

    let reg1 = graph.value_def(ctrl1).0;
    let reg2 = graph.value_def(ctrl2).0;
    let reg3 = graph.value_def(ctrl3).0;

    let domtree = compute(&graph, entry);

    assert!(domtree.cfg_dominates(entry, entry));
    assert!(!domtree.cfg_strictly_dominates(entry, entry));
    assert!(domtree.cfg_dominates(entry, ret));
    assert!(domtree.cfg_strictly_dominates(entry, ret));
    assert!(!domtree.cfg_dominates(ret, entry));

    assert!(domtree.cfg_dominates(entry, reg1));
    assert!(domtree.cfg_dominates(entry, reg2));
    assert!(domtree.cfg_dominates(entry, reg3));

    assert!(!domtree.cfg_dominates(reg3, reg1));
    assert!(!domtree.cfg_dominates(reg2, reg1));
}

#[test]
fn diamond_query() {
    let (mut graph, entry, ctrl) = create_graph();
    let (actrl, bctrl) = create_brcond(&mut graph, ctrl);
    let actrl = create_region(&mut graph, [actrl]);
    let bctrl = create_region(&mut graph, [bctrl]);
    let join_ctrl = create_region(&mut graph, [actrl, bctrl]);
    let ret = create_return(&mut graph, [join_ctrl]);

    let anode = graph.value_def(actrl).0;
    let bnode = graph.value_def(bctrl).0;
    let join = graph.value_def(join_ctrl).0;

    let domtree = compute(&graph, entry);

    assert!(domtree.cfg_dominates(entry, join));
    assert!(domtree.cfg_dominates(entry, ret));
    assert!(domtree.cfg_dominates(entry, anode));
    assert!(domtree.cfg_dominates(entry, bnode));
    assert!(!domtree.cfg_dominates(anode, ret));
    assert!(!domtree.cfg_dominates(bnode, ret));
}

use crate::{
    builder::{BuilderExt, SimpleBuilder},
    test_utils::{create_const32, create_entry, create_region, create_return},
    valgraph::DepValue,
};

use super::*;

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
fn basic_cfg_reachable() {
    let (mut graph, entry, ctrl) = create_graph();
    let (actrl, bctrl) = create_brcond(&mut graph, ctrl);
    let actrl = create_region(&mut graph, [actrl]);
    let bctrl = create_region(&mut graph, [bctrl]);
    let dead_region = create_region(&mut graph, []);
    let ctrl = create_region(&mut graph, [actrl, bctrl, dead_region]);
    let ret = create_return(&mut graph, [ctrl]);

    let domtree = DomTree::compute(&graph, entry);
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

    let domtree = DomTree::compute(&graph, entry);

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

    let domtree = DomTree::compute(&graph, entry);

    assert!(domtree.cfg_dominates(entry, join));
    assert!(domtree.cfg_dominates(entry, ret));
    assert!(domtree.cfg_dominates(entry, anode));
    assert!(domtree.cfg_dominates(entry, bnode));
    assert!(!domtree.cfg_dominates(anode, ret));
    assert!(!domtree.cfg_dominates(bnode, ret));
}

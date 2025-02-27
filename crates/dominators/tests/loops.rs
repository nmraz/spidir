use std::fmt::Write;

use cranelift_entity::SecondaryMap;
use dominators::{
    domtree::{DomTree, DomTreeNode},
    loops::{Loop, LoopForest},
};
use entity_set::DenseEntitySet;
use expect_test::expect;
use graphmock::{Graph, Node, graph};
use itertools::Itertools;

macro_rules! test_loops {
    ($name:ident, $graph:literal, $expected:expr) => {
        #[test]
        fn $name() {
            let g = graph($graph);
            $expected.assert_eq(&stringify_loops(&g));
        }
    };
}

fn stringify_loops(g: &Graph) -> String {
    let domtree = DomTree::compute(g, g.entry());
    let lf = LoopForest::compute(g, &domtree);

    let mut root_loop = Vec::new();
    let mut loop_classes: SecondaryMap<Loop, Vec<DomTreeNode>> = SecondaryMap::new();

    let mut loop_children: SecondaryMap<Loop, Vec<Loop>> = SecondaryMap::new();
    let mut outermost_loops = Vec::new();
    let mut visited_loops = DenseEntitySet::new();

    for node in domtree.preorder() {
        if let Some(l) = lf.containing_loop(node) {
            loop_classes[l].push(node);
            if !visited_loops.contains(l) {
                if let Some(parent) = lf.loop_parent(l) {
                    loop_children[parent].push(l);
                } else {
                    outermost_loops.push(l);
                }
                visited_loops.insert(l);
            }
        } else {
            root_loop.push(node);
        }
    }

    let mut output = String::new();
    writeln!(
        output,
        "{}",
        root_loop
            .iter()
            .format_with(" ", |&node, f| f(&g.name(domtree.get_cfg_node(node))))
    )
    .unwrap();

    let mut stack = outermost_loops;
    stack.reverse();
    while let Some(l) = stack.pop() {
        let header = lf.loop_header(l);
        write!(
            output,
            "{:indent$}h:{}",
            "",
            g.name(domtree.get_cfg_node(header)),
            indent = 2 * lf.loop_depth(l) as usize,
        )
        .unwrap();

        let line = loop_classes[l].iter().format_with(" ", |&node, f| {
            if node == header {
                return Ok(());
            }
            if lf.is_latch(g, &domtree, l, node) {
                f(&"l:")?;
            }
            f(&g.name(domtree.get_cfg_node(node)))
        });
        writeln!(output, "{}", line).unwrap();

        stack.extend(loop_children[l].iter().rev());
    }

    output
}

fn graph_loops(input: &str) -> (Graph, DomTree<Node>, LoopForest) {
    let g = graph(input);
    let domtree = DomTree::compute(&g, g.entry());
    let lf = LoopForest::compute(&g, &domtree);
    (g, domtree, lf)
}

fn domtree_node(g: &Graph, domtree: &DomTree<Node>, name: &str) -> DomTreeNode {
    domtree.get_tree_node(g.node(name)).unwrap()
}

#[test]
fn simple_query() {
    let (g, domtree, lf) = graph_loops(
        "
        a -> b
        b -> c
        c -> b, e
        ",
    );

    assert!(
        lf.containing_loop(domtree_node(&g, &domtree, "a"))
            .is_none()
    );

    assert!(
        lf.containing_loop(domtree_node(&g, &domtree, "e"))
            .is_none()
    );

    let b = domtree_node(&g, &domtree, "b");
    let l = lf.containing_loop(b).unwrap();
    assert!(lf.loop_parent(l).is_none());
    assert_eq!(lf.loop_depth(l), 1);
    assert_eq!(lf.loop_header(l), b);
    let c = domtree_node(&g, &domtree, "c");
    assert_eq!(lf.containing_loop(c), Some(l));
    assert!(lf.is_latch(&g, &domtree, l, c));
    assert!(!lf.is_latch(&g, &domtree, l, b));
}

#[test]
fn triple_nested_loop_query() {
    let (g, domtree, lf) = graph_loops(
        "
        entry -> h1
        h1 -> h2
        h2 -> h3
        h3 -> l23
        l23 -> h2, h3, l1
        l1 -> h1, exit
        ",
    );

    assert!(
        lf.containing_loop(domtree_node(&g, &domtree, "entry"))
            .is_none()
    );
    assert!(
        lf.containing_loop(domtree_node(&g, &domtree, "exit"))
            .is_none()
    );

    let h1 = domtree_node(&g, &domtree, "h1");
    let l1 = lf.containing_loop(h1).unwrap();
    let h2 = domtree_node(&g, &domtree, "h2");
    let l2 = lf.containing_loop(h2).unwrap();
    let h3 = domtree_node(&g, &domtree, "h3");
    let l3 = lf.containing_loop(h3).unwrap();

    assert_ne!(l1, l2);
    assert_ne!(l2, l3);
    assert_ne!(l1, l3);

    assert_eq!(lf.loop_depth(l1), 1);
    assert_eq!(lf.loop_depth(l2), 2);
    assert_eq!(lf.loop_depth(l3), 3);

    assert_eq!(lf.loop_ancestors(l3).collect::<Vec<_>>(), vec![l3, l2, l1]);

    assert_eq!(lf.loop_header(l1), h1);
    assert_eq!(lf.loop_header(l2), h2);
    assert_eq!(lf.loop_header(l3), h3);

    assert!(lf.is_latch(&g, &domtree, l1, domtree_node(&g, &domtree, "l1")));
    assert!(lf.is_latch(&g, &domtree, l2, domtree_node(&g, &domtree, "l23")));
    assert!(lf.is_latch(&g, &domtree, l3, domtree_node(&g, &domtree, "l23")));
    assert!(!lf.is_latch(&g, &domtree, l1, domtree_node(&g, &domtree, "l23")));
    assert!(!lf.is_latch(&g, &domtree, l2, domtree_node(&g, &domtree, "l1")));
}

test_loops! {
    simple,
    "a -> b
    b -> c
    c -> b, e",
    expect![[r#"
        a e
          h:b l:c
    "#]]
}

test_loops! {
    self_loop,
    "a -> b
    b -> b",
    expect![[r#"
        a
          h:b
    "#]]
}

test_loops! {
    multi_backedge,
    "entry -> header
    header -> a, b
    a, b -> header
    a -> exit",
    expect![[r#"
        entry exit
          h:header l:a l:b
    "#]]
}

test_loops! {
    multiple,
    "a -> b
    b -> c
    c -> b, e
    e -> f
    f -> g
    g -> f, h",
    expect![[r#"
        a e h
          h:b l:c
          h:f l:g
    "#]]
}

test_loops! {
    two_in_one,
    "entry -> outer
    outer -> a
    a -> b
    b -> c
    c -> b, e
    e -> f
    f -> g
    g -> f, h
    h -> exit, outer",
    expect![[r#"
        entry exit
          h:outer a e l:h
            h:b l:c
            h:f l:g
    "#]]
}

test_loops! {
    triple_nested,
    "entry -> h1
    h1 -> h2
    h2 -> h3
    h3 -> l3
    l3 -> h3, l2
    l2 -> h2, l1
    l1 -> h1, exit",
    expect![[r#"
        entry exit
          h:h1 l:l1
            h:h2 l:l2
              h:h3 l:l3
    "#]]
}

test_loops! {
    triple_nested_shared_latch,
    "entry -> h1
    h1 -> h2
    h2 -> h3
    h3 -> l23
    l23 -> h2, h3, l1
    l1 -> h1, exit",
    expect![[r#"
        entry exit
          h:h1 l:l1
            h:h2
              h:h3 l:l23
    "#]]
}

test_loops! {
    irreducible,
    "entry -> c1, c2
    c1 -> c2, exit
    c2 -> c1, exit
    exit -> ret",
    expect![[r#"
        entry c1 exit ret c2
    "#]]
}

test_loops! {
    irreducible_complex,
    "entry -> a, b
    a -> c, d
    b -> c, d
    c -> a, exit
    d -> b, exit",
    expect![[r#"
        entry a c exit d b
    "#]]
}

test_loops! {
    irreducible_body,
    "entry -> header
    header -> c1, c2
    c1 -> c2, header
    c2 -> c1, header",
    expect![[r#"
        entry
          h:header l:c1 l:c2
    "#]]
}

test_loops! {
    irreducible_cycle_enclosing_loops,
    "entry -> a, b
    a -> a, c
    b -> b, c
    c -> a, b, exit",
    expect![[r#"
        entry c exit
          h:a
          h:b
    "#]]
}

test_loops! {
    irreducible_union_of_loops,
    "entry -> a, b
    a -> b, c
    b -> a, d
    c -> a, exit
    d -> b, exit",
    expect![[r#"
        entry exit
          h:a l:c
          h:b l:d
    "#]]
}

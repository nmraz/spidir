use std::fmt::Write;

use dominators::domtree::DomTree;
use expect_test::expect;
use graphmock::{Graph, Node, graph};
use graphwalk::WalkPhase;

macro_rules! test_domtree {
    ($name:ident, $graph:literal, $expected:expr) => {
        #[test]
        fn $name() {
            let g = graph($graph);
            $expected.assert_eq(&stringify_domtree(&g));
        }
    };
}

fn stringify_domtree(g: &Graph) -> String {
    let mut output = String::new();
    let domtree = DomTree::compute(g, g.entry());
    let mut postorder = domtree.postorder();

    let mut indent = 0;
    while let Some((phase, node)) = postorder.next_event() {
        match phase {
            WalkPhase::Pre => {
                let node = domtree.get_cfg_node(node);
                writeln!(
                    output,
                    "{:indent$}{}",
                    "",
                    g.name(node),
                    indent = 2 * indent,
                )
                .unwrap();
                indent += 1;
            }
            WalkPhase::Post => {
                indent -= 1;
            }
        };
    }

    output
}

fn graph_domtree(input: &str) -> (Graph, DomTree<Node>) {
    let g = graph(input);
    let domtree = DomTree::compute(&g, g.entry());
    (g, domtree)
}

#[test]
fn straight_line_query() {
    let (g, domtree) = graph_domtree(
        "
        a -> b
        b -> c
        c -> d
        ",
    );

    assert_eq!(domtree.root(), domtree.get_tree_node(g.node("a")).unwrap());

    assert!(domtree.is_cfg_reachable(g.node("a")));
    assert!(domtree.is_cfg_reachable(g.node("d")));
    assert!(domtree.cfg_dominates(g.node("a"), g.node("a")));
    assert!(domtree.cfg_dominates(g.node("a"), g.node("b")));
    assert!(domtree.cfg_dominates(g.node("b"), g.node("c")));
    assert!(domtree.cfg_dominates(g.node("a"), g.node("c")));
    assert!(domtree.cfg_dominates(g.node("a"), g.node("d")));
    assert!(!domtree.cfg_dominates(g.node("b"), g.node("a")));
    assert!(!domtree.cfg_strictly_dominates(g.node("b"), g.node("b")));
}

#[test]
fn diamond_query() {
    let (g, domtree) = graph_domtree(
        "
        a -> b, c
        b, c -> d
        ",
    );

    assert!(domtree.is_cfg_reachable(g.node("b")));
    assert!(domtree.is_cfg_reachable(g.node("c")));
    assert!(domtree.is_cfg_reachable(g.node("d")));
    assert!(domtree.cfg_dominates(g.node("a"), g.node("b")));
    assert!(domtree.cfg_dominates(g.node("a"), g.node("c")));
    assert!(!domtree.cfg_dominates(g.node("b"), g.node("d")));
    assert!(!domtree.cfg_dominates(g.node("c"), g.node("d")));
}

#[test]
fn unreachable_subgraph_query() {
    let (g, domtree) = graph_domtree(
        "entry -> exit
        a -> b, c
        b, c -> exit",
    );

    assert!(domtree.is_cfg_reachable(g.node("exit")));
    assert!(!domtree.is_cfg_reachable(g.node("a")));
    assert!(!domtree.is_cfg_reachable(g.node("b")));
    assert!(!domtree.is_cfg_reachable(g.node("c")));
}

test_domtree! {
    straigt_line,
    "a -> b
    b -> c
    c -> d",
    expect![[r#"
        a
          b
            c
              d
    "#]]
}

test_domtree! {
    diamond,
    "a -> b, c
    b, c -> d",
    expect![[r#"
        a
          b
          d
          c
    "#]]
}

test_domtree! {
    sdom_not_idom,
    "a -> b, c
    b -> c, d
    c -> d
    d -> e",
    expect![[r#"
        a
          b
          d
            e
          c
    "#]]
}

test_domtree! {
    large_sdom_bucket,
    "a -> a1, a2, a3, a4, a5, a6, a7, a8, a9
    a1 -> b1
    a2 -> b2
    a3 -> b3
    a4 -> b4
    a5 -> b5
    a6 -> b6
    a7 -> b7
    a8 -> b8
    a9 -> b9
    b1, b2, b3, b4, b5, b6, b7, b8, b9 -> c",
    expect![[r#"
        a
          a1
            b1
          a2
            b2
          a3
            b3
          a4
            b4
          a5
            b5
          a6
            b6
          a7
            b7
          a8
            b8
          c
          a9
            b9
    "#]]
}

test_domtree! {
    long_eval_path,
    // At least one of these long "snakes" will result in a long eval path, depending on the order
    // in which children are visited in the DFS. If the first child is visited first, `e` will be
    // lower-numbered than the `d` chain and result in a long `eval` path when computing its
    // semidominator. If the second child is visited first, the same thing will happen with `c` and
    // the `b` path.
    // Currently, the second case occurs: `c` is visited first in the preorder, as is evident by the
    // fact that it comes after `b1` in the tree (the tree is constructed in reverse preorder).
    "a -> b1, c
    b1 -> b2
    b2 -> b3
    b3 -> b4
    b4 -> b5
    b5 -> b6
    b6 -> b7
    b7 -> b8
    b8 -> b9
    b9 -> b10
    b10 -> c
    c -> e, d1
    d1 -> d2
    d2 -> d3
    d3 -> d4
    d4 -> d5
    d5 -> d6
    d6 -> d7
    d7 -> d8
    d8 -> d9
    d9 -> d10
    d10 -> e",
    expect![[r#"
        a
          b1
            b2
              b3
                b4
                  b5
                    b6
                      b7
                        b8
                          b9
                            b10
          c
            e
            d1
              d2
                d3
                  d4
                    d5
                      d6
                        d7
                          d8
                            d9
                              d10
    "#]]
}

test_domtree! {
    lt_paper,
    "R -> A, B, C
    B -> A, D, E
    A -> D
    E -> H
    D -> L
    L -> H
    H -> E, K
    C -> F, G
    F -> I
    G -> I, J
    J -> I
    I -> K
    K -> I, R",
    expect![[r#"
        R
          A
          D
            L
          H
          E
          B
          K
          I
          C
            F
            G
              J
    "#]]
}

test_domtree! {
    rustc_bucket_bug,
    "0 -> 1
    1 -> 2
    2 -> 3
    3 -> 4

    1 -> 5
    5 -> 6

    0 -> 7

    7 -> 2
    5 -> 3",
    expect![[r#"
        0
          1
            5
              6
          3
            4
          2
          7
    "#]]
}

test_domtree! {
    unreachable_subgraph,
    "entry -> exit
    a -> b, c
    b, c -> exit",
    expect![[r#"
        entry
          exit
    "#]]
}

test_domtree! {
    simple_loop,
    "a -> b
    b -> c
    c -> b, d",
    expect![[r#"
        a
          b
            c
              d
    "#]]
}

test_domtree! {
    irreducible,
    "entry -> c1, c2
    c1 -> c2, exit
    c2 -> c1, exit
    exit -> ret",
    expect![[r#"
        entry
          c1
          exit
            ret
          c2
    "#]]
}

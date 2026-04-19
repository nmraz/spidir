use std::fmt::Write;

use dominators::postdomtree::PostDomTree;
use expect_test::expect;
use graphmock::{Graph, graph};
use graphwalk::dfs::WalkPhase;

macro_rules! test_postdomtree {
    ($name:ident, $graph:literal, $expected:expr) => {
        #[test]
        fn $name() {
            let g = graph($graph);
            $expected.assert_eq(&stringify_postdomtree(&g));
        }
    };
}

fn stringify_postdomtree(g: &Graph) -> String {
    let mut output = String::new();
    let domtree = PostDomTree::compute(g, g.entry());
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

test_postdomtree! {
    straight_line,
    "a -> b
    b -> c
    c -> d",
    expect![[r#"
        d
          c
            b
              a
    "#]]
}

test_postdomtree! {
    diamond,
    "a -> b, c
    b, c -> d",
    expect![[r#"
        d
          b
          a
          c
    "#]]
}

test_postdomtree! {
    unreachable_subgraph,
    "entry -> exit
    a -> b, c
    b, c -> exit",
    expect![[r#"
        exit
          entry
    "#]]
}

test_postdomtree! {
    simple_loop,
    "a -> b
    b -> c
    c -> b, d",
    expect![[r#"
        d
          c
            b
              a
    "#]]
}

test_postdomtree! {
    split,
    "a -> b
    b -> c
    b -> d",
    expect![[r#"
        c
        b
          a
        d
    "#]]
}

test_postdomtree! {
    multi_exit,
    "a -> b, c, d
    b, c -> e
    c, d -> f",
    expect![[r#"
        e
          b
        c
        a
        f
          d
    "#]]
}

test_postdomtree! {
    multi_backedge_loop_with_exit,
    "entry -> header
    header -> a, b
    a, b -> header
    a -> exit",
    expect![[r#"
        exit
          a
            header
              entry
              b
    "#]]
}

test_postdomtree! {
    tight_infinite_loop,
    "a -> b
    b -> b",
    expect![[r#"
        b
          a
    "#]]
}

test_postdomtree! {
    split_with_infinite_loop,
    "a -> b, c
    b -> b",
    expect![[r#"
        b
        a
        c
    "#]]
}

test_postdomtree! {
    irreducible_cycle,
    "entry -> c1, c2
    c1 -> c2, exit
    c2 -> c1, exit
    exit -> ret",
    expect![[r#"
        ret
          exit
            entry
            c1
            c2
    "#]]
}

// Note: the root here is an arbitrary representative from the cycle.
test_postdomtree! {
    infinite_irreducible_cycle,
    "entry -> c1, c2
    c1 -> c2
    c2 -> c1",
    expect![[r#"
        c1
          entry
          c2
    "#]]
}

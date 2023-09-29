use expect_test::expect;
use graphmock::Graph;
use graphwalk::entity_preorder;
use itertools::Itertools;

macro_rules! test_preorder {
    ($name:ident, $graph:literal, $expected:expr) => {
        #[test]
        fn $name() {
            let g = graphmock::graph($graph);
            $expected.assert_eq(&collect_preorder(&g));
        }
    };
}

fn collect_preorder(g: &Graph) -> String {
    let preorder = entity_preorder(g, [g.entry()]);
    preorder.map(|node| g.name(node)).format(" ").to_string()
}

test_preorder! {
    straight_line,
    "a -> b
    b -> c
    c -> d",
    expect!["a b c d"]
}

test_preorder! {
    diamond,
    "a -> b, c
    b, c -> d",
    expect!["a c d b"]
}

test_preorder! {
    straight_line_skips,
    "a -> b, c
    b -> c, d
    c -> d
    d -> e",
    expect!["a c d e b"]
}

test_preorder! {
    simple_loop,
    "a -> b
    b -> c
    c -> b, e",
    expect!["a b c e"]
}

test_preorder! {
    loop_diamond,
    "a -> b
    b -> c, d
    c, d -> e
    e -> b, f",
    expect!["a b d e f c"]
}

use std::fmt::Write;

use entity_utils::set::DenseEntitySet;
use expect_test::{Expect, expect};
use graphmock::{Graph, Node};
use graphwalk::scc::Condensation;

use itertools::Itertools;

macro_rules! test_sccs {
    ($name:ident, $graph:literal, $expected:expr) => {
        #[test]
        fn $name() {
            let g = graphmock::graph($graph);
            check_sccs(&g, g.nodes(), $expected);
        }
    };
}

macro_rules! test_sccs_rev {
    ($name:ident, $graph:literal, $expected:expr) => {
        #[test]
        fn $name() {
            let g = graphmock::graph($graph);
            check_sccs(&g, g.nodes().rev(), $expected);
        }
    };
}

fn check_sccs(g: &Graph, roots: impl IntoIterator<Item = Node>, expected: Expect) {
    let condensation = Condensation::compute(&g, roots);

    let mut dump = String::new();

    for scc in condensation.scc_postorder() {
        let members = condensation.scc_members(scc);
        for &member in members {
            assert_eq!(condensation.node_scc(member), Some(scc));
        }

        writeln!(
            dump,
            "{scc}: {}",
            members.iter().map(|&member| g.name(member)).format(", ")
        )
        .unwrap();
    }

    writeln!(dump).unwrap();

    let mut seen_sccs = DenseEntitySet::new();
    for scc in condensation.scc_postorder() {
        let preds = condensation.scc_preds(scc);
        let succs = condensation.scc_succs(scc);

        // Enforce that this is indeed a DAG postorder.
        for &succ in succs {
            assert!(seen_sccs.contains(succ));
        }
        seen_sccs.insert(scc);
        for &pred in preds {
            assert!(!seen_sccs.contains(pred));
        }

        if !succs.is_empty() {
            writeln!(dump, "{scc} -> {}", succs.iter().format(", ")).unwrap();
        }
    }

    expected.assert_eq(&dump);
}

// Try different visitation orders to make sure the discovered SCCs aren't affected.

test_sccs! {
    straight_line,
    "a -> b
    b -> c
    c -> d",
    expect![[r#"
        scc0: d
        scc1: c
        scc2: b
        scc3: a

        scc1 -> scc0
        scc2 -> scc1
        scc3 -> scc2
    "#]]
}

test_sccs_rev! {
    straight_line_rev,
    "a -> b
    b -> c
    c -> d",
    expect![[r#"
        scc0: d
        scc1: c
        scc2: b
        scc3: a

        scc1 -> scc0
        scc2 -> scc1
        scc3 -> scc2
    "#]]
}

test_sccs! {
    diamond,
    "a -> b, c
    b, c -> d",
    expect![[r#"
        scc0: d
        scc1: c
        scc2: b
        scc3: a

        scc1 -> scc0
        scc2 -> scc0
        scc3 -> scc1, scc2
    "#]]
}

test_sccs! {
    tarjan_paper,
    "1 -> 2
    2 -> 3, 8
    3 -> 4, 7
    4 -> 5
    5 -> 3, 6
    7 -> 4, 6
    8 -> 1, 7",
    expect![[r#"
        scc0: 6
        scc1: 4, 7, 3, 5
        scc2: 2, 1, 8

        scc1 -> scc0
        scc2 -> scc1
    "#]]
}

test_sccs_rev! {
    tarjan_paper_rev,
    "1 -> 2
    2 -> 3, 8
    3 -> 4, 7
    4 -> 5
    5 -> 3, 6
    7 -> 4, 6
    8 -> 1, 7",
    expect![[r#"
        scc0: 6
        scc1: 3, 5, 4, 7
        scc2: 8, 2, 1

        scc1 -> scc0
        scc2 -> scc1
    "#]]
}

test_sccs! {
    large_cycle,
    "a -> b
    b -> c
    c -> d
    d -> e
    e -> a",
    expect![[r#"
        scc0: d, c, b, a, e

    "#]]
}

test_sccs! {
    self_loop,
    "a -> a",
    expect![[r"
        scc0: a

    "]]
}

test_sccs! {
    simple_loop,
    "a -> b
    b -> c
    c -> b, e",
    expect![[r#"
        scc0: e
        scc1: b, c
        scc2: a

        scc1 -> scc0
        scc2 -> scc1
    "#]]
}

test_sccs! {
    simple_loop_multi_entry,
    "a -> c
    b -> c
    c -> d
    d -> c, f",
    expect![[r#"
        scc0: f
        scc1: c, d
        scc2: b
        scc3: a

        scc1 -> scc0
        scc2 -> scc1
        scc3 -> scc1
    "#]]
}

test_sccs_rev! {
    simple_loop_multi_entry_rev,
    "a -> c
    b -> c
    c -> d
    d -> c, f",
    expect![[r#"
        scc0: f
        scc1: d, c
        scc2: a
        scc3: b

        scc1 -> scc0
        scc2 -> scc1
        scc3 -> scc1
    "#]]
}

test_sccs! {
    simple_loop_diamond,
    "a -> b
    b -> c, d
    c, d -> e
    e -> b, f",
    expect![[r#"
        scc0: f
        scc1: c, d, b, e
        scc2: a

        scc1 -> scc0
        scc2 -> scc1
    "#]]
}

test_sccs! {
    simple_loop_multi_exit,
    "a -> b
    b -> c, d
    c, d -> b
    c, d -> e",
    expect![[r#"
        scc0: e
        scc1: c, b, d
        scc2: a

        scc1 -> scc0
        scc2 -> scc1
    "#]]
}

test_sccs! {
    multiple_loops,
    "a -> b
    b -> c
    c -> b, e
    e -> f
    f -> g
    g -> f, h",
    expect![[r#"
        scc0: h
        scc1: f, g
        scc2: e
        scc3: b, c
        scc4: a

        scc1 -> scc0
        scc2 -> scc1
        scc3 -> scc2
        scc4 -> scc3
    "#]]
}

test_sccs! {
    nested_loops,
    "entry -> outer
    outer -> a
    a -> b
    b -> c
    c -> b, e
    e -> exit, outer",
    expect![[r#"
        scc0: exit
        scc1: c, b, a, outer, e
        scc2: entry

        scc1 -> scc0
        scc2 -> scc1
    "#]]
}

test_sccs! {
    irreducible_cycle,
    "entry -> c1, c2
    c1 -> c2, exit
    c2 -> c1, exit
    exit -> ret",
    expect![[r#"
        scc0: ret
        scc1: exit
        scc2: c1, c2
        scc3: entry

        scc1 -> scc0
        scc2 -> scc1
        scc3 -> scc2
    "#]]
}

test_sccs! {
    irreducible_cycle_complex,
    "entry -> a, b
    a -> c, d
    b -> c, d
    c -> a, exit
    d -> b, exit",
    expect![[r#"
        scc0: exit
        scc1: a, c, b, d
        scc2: entry

        scc1 -> scc0
        scc2 -> scc1
    "#]]
}

test_sccs! {
    loop_irreducible_body,
    "entry -> header
    header -> c1, c2
    c1 -> c2, header
    c2 -> c1, header",
    expect![[r#"
        scc0: c1, header, c2
        scc1: entry

        scc1 -> scc0
    "#]]
}

test_sccs! {
    irreducible_cycle_enclosing_loops,
    "entry -> a, b
    a -> a, c
    b -> b, c
    c -> a, b, exit",
    expect![[r#"
        scc0: exit
        scc1: a, b, c
        scc2: entry

        scc1 -> scc0
        scc2 -> scc1
    "#]]
}

test_sccs! {
    irreducible_union_of_loops,
    "entry -> a, b
    a -> b, c
    b -> a, d
    c -> a, exit
    d -> b, exit",
    expect![[r#"
        scc0: exit
        scc1: c, a, b, d
        scc2: entry

        scc1 -> scc0
        scc2 -> scc1
    "#]]
}

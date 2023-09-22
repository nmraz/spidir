use cranelift_entity::EntitySet;
use expect_test::expect;
use graphmock::{Graph, Node};
use graphwalk::{PostOrder, WalkPhase};
use itertools::Itertools;
use std::fmt::Write;

macro_rules! test_postorder {
    ($name:ident, $graph:literal, $expected_full:expr, $expected_rpo:expr) => {
        #[test]
        fn $name() {
            let g = graphmock::graph($graph);
            let (full, rpo) = collect_postorder(&g);
            $expected_full.assert_eq(&full);
            $expected_rpo.assert_eq(&rpo);
        }
    };
}

fn collect_postorder(g: &Graph) -> (String, String) {
    let mut postorder = PostOrder::<_, EntitySet<Node>>::new(g, [g.entry()]);

    let mut full = String::new();
    let mut indent = 0;
    while let Some((phase, node)) = postorder.next_event() {
        let (phase, indent) = match phase {
            WalkPhase::Pre => {
                let cur_indent = indent;
                indent += 1;
                ("ent", cur_indent)
            }
            WalkPhase::Post => {
                indent -= 1;
                ("ex", indent)
            }
        };
        writeln!(full, "{:indent$}{phase}:{}", "", g.name(node)).unwrap();
    }

    let mut rpo: Vec<_> = PostOrder::<_, EntitySet<Node>>::new(g, [g.entry()]).collect();
    rpo.reverse();
    let rpo = rpo.iter().map(|&node| g.name(node)).format(" ").to_string();

    (full, rpo)
}

test_postorder! {
    straight_line,
    "a -> b
    b -> c
    c -> d",
    expect![[r#"
        ent:a
         ent:b
          ent:c
           ent:d
           ex:d
          ex:c
         ex:b
        ex:a
    "#]],
    expect!["a b c d"]
}

test_postorder! {
    diamond,
    "a -> b, c
    b, c -> d",
    expect![[r#"
        ent:a
         ent:c
          ent:d
          ex:d
         ex:c
         ent:b
         ex:b
        ex:a
    "#]],
    expect!["a b c d"]
}

test_postorder! {
    straight_line_skips,
    "a -> b, c
    b -> c, d
    c -> d
    d -> e",
    expect![[r#"
        ent:a
         ent:c
          ent:d
           ent:e
           ex:e
          ex:d
         ex:c
         ent:b
         ex:b
        ex:a
    "#]],
    expect!["a b c d e"]
}

test_postorder! {
    simple_loop,
    "a -> b
    b -> c
    c -> b, e",
    expect![[r#"
        ent:a
         ent:b
          ent:c
           ent:e
           ex:e
          ex:c
         ex:b
        ex:a
    "#]],
    expect!["a b c e"]
}

test_postorder! {
    loop_diamond,
    "a -> b
    b -> c, d
    c, d -> e
    e -> b, f",
    expect![[r#"
        ent:a
         ent:b
          ent:d
           ent:e
            ent:f
            ex:f
           ex:e
          ex:d
          ent:c
          ex:c
         ex:b
        ex:a
    "#]],
    expect!["a b c d e f"]
}

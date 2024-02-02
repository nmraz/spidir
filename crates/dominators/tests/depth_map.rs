use dominators::{depth_map::DepthMap, domtree::DomTree, loops::LoopForest};
use graphmock::{graph, Graph, Node};

macro_rules! test_depth_map {
    ($name:ident, $graph:literal, { $( $node:literal : ($dom_depth:literal, $loop_depth:literal) ),* $(,)? }) => {
        #[test]
        fn $name() {
            let (g, domtree, depth_map) = graph_depth_map($graph);
            $({
                let node = domtree.get_tree_node(g.node($node)).unwrap();
                assert_eq!(depth_map.domtree_depth(node), $dom_depth);
                assert_eq!(depth_map.loop_depth(node), $loop_depth);
            })*
        }
    };
}

fn graph_depth_map(input: &str) -> (Graph, DomTree<Node>, DepthMap) {
    let g = graph(input);
    let domtree = DomTree::compute(&g, g.entry());
    let loop_forest = LoopForest::compute(&g, &domtree);
    let depth_map = DepthMap::compute(&domtree, &loop_forest);
    (g, domtree, depth_map)
}

test_depth_map! {
    straight_line,
    "a -> b
    b -> c
    c -> d",
    {
        "a": (0, 0),
        "b": (1, 0),
        "c": (2, 0),
        "d": (3, 0),
    }
}

test_depth_map! {
    two_loops_in_one,
    "entry -> outer
    outer -> a
    a -> b
    b -> c
    c -> b, e
    e -> f
    f -> g
    g -> f, h
    h -> exit, outer",
    {
        "entry": (0, 0),
        "outer": (1, 1),
        "a": (2, 1),
        "b": (3, 2),
        "e": (5, 1),
        "f": (6, 2),
        "exit": (9, 0),
    }
}

test_depth_map! {
    triple_nested,
    "entry -> h1
    h1 -> h2
    h2 -> h3
    h3 -> l3
    l3 -> h3, l2
    l2 -> h2, l1
    l1 -> h1, exit",
    {
        "entry": (0, 0),
        "h1": (1, 1),
        "h2": (2, 2),
        "h3": (3, 3),
        "l3": (4, 3),
        "l2": (5, 2),
        "l1": (6, 1),
        "exit": (7, 0),
    }
}

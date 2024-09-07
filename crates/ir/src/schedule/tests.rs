use cranelift_entity::EntitySet;
use graphwalk::PostOrderContext;

use crate::{
    builder::{BuilderExt, SimpleBuilder},
    node::Type,
    test_utils::{create_const32, create_entry, create_loop_body, create_return},
    valgraph::{Node, ValGraph},
    valwalk::{cfg_preorder, LiveNodeInfo},
};

use super::{is_pinned_node, schedule_early, schedule_late, ScheduleContext};

fn check_live_scheduled(
    graph: &ValGraph,
    live_node_info: &LiveNodeInfo,
    scheduled: &EntitySet<Node>,
    kind: &str,
) {
    for node in live_node_info
        .live_nodes()
        .keys()
        .filter(|&node| live_node_info.live_nodes().contains(node))
        .filter(|&node| !is_pinned_node(graph, node))
    {
        assert!(scheduled.contains(node), "{node} not scheduled {kind}");
    }
}

fn check_graph_scheduling(graph: &ValGraph, entry: Node) {
    let live_node_info = LiveNodeInfo::compute(graph, entry);
    let cfg_preorder: Vec<_> = cfg_preorder(graph, entry).collect();
    let ctx = ScheduleContext::new(graph, &live_node_info, &cfg_preorder);
    let mut scheduled = EntitySet::new();

    let mut scratch_postorder = PostOrderContext::new();
    schedule_early(&ctx, &mut scratch_postorder, |_ctx, node| {
        scheduled.insert(node);
    });
    check_live_scheduled(graph, &live_node_info, &scheduled, "early");

    scheduled.clear();
    schedule_late(&ctx, &mut scratch_postorder, |_ctx, node| {
        scheduled.insert(node);
    });
    check_live_scheduled(graph, &live_node_info, &scheduled, "late");
}

#[test]
fn schedule_simple_add() {
    let mut graph = ValGraph::new();
    let (entry, ctrl, [param]) = create_entry(&mut graph, [Type::I32]);
    let add = SimpleBuilder(&mut graph).build_iadd(param, param);
    create_return(&mut graph, [ctrl, add]);
    check_graph_scheduling(&graph, entry);
}

#[test]
fn schedule_loop() {
    let body = create_loop_body();
    check_graph_scheduling(&body.graph, body.entry);
}

#[test]
fn schedule_floating_data_chain() {
    let mut graph = ValGraph::new();
    let (entry, ctrl, []) = create_entry(&mut graph, []);
    let iconst = create_const32(&mut graph);
    let iext = SimpleBuilder(&mut graph).build_iext(iconst);
    let sfill = SimpleBuilder(&mut graph).build_sfill(32, iext);
    create_return(&mut graph, [ctrl, sfill]);
    check_graph_scheduling(&graph, entry);
}

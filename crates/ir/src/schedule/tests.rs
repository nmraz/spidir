use cranelift_entity::EntitySet;
use graphwalk::PostOrderContext;

use crate::{
    builder::{BuilderExt, SimpleBuilder},
    function::FunctionBody,
    node::Type,
    test_utils::create_loop_body,
    valgraph::{Node, ValGraph},
    valwalk::LiveNodeInfo,
};

use super::{is_pinned_node, schedule_early, schedule_late, ScheduleContext};

fn check_live_scheduled(
    graph: &ValGraph,
    live_node_info: &LiveNodeInfo,
    scheduled: &EntitySet<Node>,
    kind: &str,
) {
    for node in live_node_info
        .live_nodes
        .keys()
        .filter(|&node| live_node_info.live_nodes.contains(node))
        .filter(|&node| !is_pinned_node(graph, node))
    {
        assert!(scheduled.contains(node), "{node} not scheduled {kind}");
    }
}

fn check_graph_scheduling(body: &FunctionBody) {
    let live_node_info = body.compute_live_nodes();
    let cfg_preorder: Vec<_> = body.cfg_preorder().collect();
    let ctx = ScheduleContext::new(&body.graph, &live_node_info, &cfg_preorder);
    let mut scheduled = EntitySet::new();

    let mut scratch_postorder = PostOrderContext::new();
    schedule_early(&ctx, &mut scratch_postorder, |_ctx, node| {
        scheduled.insert(node);
    });
    check_live_scheduled(&body.graph, &live_node_info, &scheduled, "early");

    scheduled.clear();
    schedule_late(&ctx, &mut scratch_postorder, |_ctx, node| {
        scheduled.insert(node);
    });
    check_live_scheduled(&body.graph, &live_node_info, &scheduled, "late");
}

#[test]
fn schedule_simple_add() {
    let mut body = FunctionBody::new(&[Type::I32]);
    let ctrl = body.entry_ctrl();
    let param = body.param_value(0);

    let mut builder = SimpleBuilder(&mut body);
    let add = builder.build_iadd(param, param);
    builder.build_return(ctrl, Some(add));
    check_graph_scheduling(&body);
}

#[test]
fn schedule_loop() {
    let body = create_loop_body();
    check_graph_scheduling(&body);
}

#[test]
fn schedule_floating_data_chain() {
    let mut body = FunctionBody::new(&[]);
    let ctrl = body.entry_ctrl();

    let mut builder = SimpleBuilder(&mut body);
    let iconst = builder.build_iconst(Type::I32, 5);
    let iext = builder.build_iext(iconst);
    let sfill = builder.build_sfill(32, iext);
    builder.build_return(ctrl, Some(sfill));
    check_graph_scheduling(&body);
}

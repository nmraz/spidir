use entity_utils::set::DenseEntitySet;
use graphwalk::PostOrderContext;

use crate::{
    builder::{BuilderExt, SimpleBuilder},
    function::FunctionBody,
    node::Type,
    test_utils::create_loop_body,
    valgraph::{Node, ValGraph},
    valwalk::GraphWalkInfo,
};

use super::{ScheduleContext, is_pinned_node, schedule_early, schedule_late};

fn check_live_scheduled(
    graph: &ValGraph,
    walk_info: &GraphWalkInfo,
    scheduled: &DenseEntitySet<Node>,
    kind: &str,
) {
    for node in walk_info
        .live_nodes
        .iter()
        .filter(|&node| !is_pinned_node(graph, node))
    {
        assert!(scheduled.contains(node), "{node} not scheduled {kind}");
    }
}

fn check_graph_scheduling(body: &FunctionBody) {
    let walk_info = body.compute_full_walk_info();
    let cfg_preorder = body.compute_cfg_preorder_info();
    let ctx = ScheduleContext::new(&body.graph, &walk_info, &cfg_preorder.preorder);
    let mut scheduled = DenseEntitySet::new();

    let mut scratch_postorder = PostOrderContext::new();
    schedule_early(&ctx, &mut scratch_postorder, |_ctx, node| {
        scheduled.insert(node);
    });
    check_live_scheduled(&body.graph, &walk_info, &scheduled, "early");

    scheduled.clear();
    schedule_late(&ctx, &mut scratch_postorder, |_ctx, node| {
        scheduled.insert(node);
    });
    check_live_scheduled(&body.graph, &walk_info, &scheduled, "late");
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

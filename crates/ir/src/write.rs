use alloc::vec::Vec;
use core::fmt;

use crate::{
    module::{ExternFunctionData, FunctionData, Module},
    node::{FunctionRef, NodeKind},
    valgraph::{Node, ValGraph},
    valwalk::LiveNodeInfo,
};

pub fn write_module(w: &mut dyn fmt::Write, module: &Module) -> fmt::Result {
    for (_, extern_func) in module.extern_functions.iter() {
        write_extern_function(w, extern_func)?;
    }
    w.write_str("\n")?;

    let mut first_function = true;
    for (_, func) in module.functions.iter() {
        if !first_function {
            w.write_str("\n")?;
        }
        first_function = false;
        write_function(w, module, func)?;
    }

    Ok(())
}

pub fn write_function(
    w: &mut dyn fmt::Write,
    module: &Module,
    function: &FunctionData,
) -> fmt::Result {
    write!(w, "func @{}:{}(", function.name, function.sig.ret_type)?;

    let mut first_arg = true;
    for &arg_type in &function.sig.arg_types {
        if !first_arg {
            w.write_str(", ")?;
        }
        first_arg = false;
        write!(w, "{}", arg_type)?;
    }

    w.write_str(") {\n")?;
    write_graph(w, module, &function.valgraph, function.entry_node, 4)?;
    w.write_str("}\n")
}

pub fn write_graph(
    w: &mut dyn fmt::Write,
    module: &Module,
    graph: &ValGraph,
    entry: Node,
    indentation: u32,
) -> fmt::Result {
    let mut rpo: Vec<_> = LiveNodeInfo::compute(graph, entry)
        .postorder(graph)
        .collect();
    rpo.reverse();

    for node in rpo {
        write_node(w, module, graph, node, indentation)?;
    }

    Ok(())
}

pub fn write_node(
    w: &mut dyn fmt::Write,
    module: &Module,
    graph: &ValGraph,
    node: Node,
    indentation: u32,
) -> fmt::Result {
    for _ in 0..indentation {
        w.write_char(' ')?;
    }

    let outputs = graph.node_outputs(node);

    if !outputs.is_empty() {
        let mut first = true;
        for output in outputs {
            if !first {
                w.write_str(", ")?;
            }
            first = false;
            write!(w, "{output}:{}", graph.value_kind(output))?;
        }
        w.write_str(" = ")?;
    }

    match graph.node_kind(node) {
        NodeKind::Entry => w.write_str("entry")?,
        NodeKind::Return => w.write_str("return")?,
        NodeKind::Region => w.write_str("region")?,
        NodeKind::Phi => w.write_str("phi")?,
        NodeKind::IConst(val) => write!(w, "iconst {val}")?,
        NodeKind::Iadd => w.write_str("iadd")?,
        NodeKind::Isub => w.write_str("isub")?,
        NodeKind::And => w.write_str("and")?,
        NodeKind::Or => w.write_str("or")?,
        NodeKind::Xor => w.write_str("xor")?,
        NodeKind::Shl => w.write_str("shl")?,
        NodeKind::Lshr => w.write_str("lshr")?,
        NodeKind::Ashr => w.write_str("ashr")?,
        NodeKind::Smul => w.write_str("smul")?,
        NodeKind::Umul => w.write_str("umul")?,
        NodeKind::Sdiv => w.write_str("sdiv")?,
        NodeKind::Udiv => w.write_str("udiv")?,
        NodeKind::Icmp(kind) => write!(w, "icmp {kind}")?,
        NodeKind::FConst(val) => write!(w, "fconst {val}")?,
        NodeKind::Load => w.write_str("load")?,
        NodeKind::Store => w.write_str("store")?,
        NodeKind::BrCond => w.write_str("brcond")?,
        NodeKind::Call(func) => {
            w.write_str("call ")?;
            write_func_ref(w, module, *func)?;
        }
    };

    let mut first = true;
    for input in graph.node_inputs(node) {
        if first {
            w.write_str(" ")?;
        } else {
            w.write_str(", ")?;
        }
        first = false;
        write!(w, "%{}", input.as_u32())?;
    }
    writeln!(w)?;

    Ok(())
}

fn write_extern_function(w: &mut dyn fmt::Write, func: &ExternFunctionData) -> fmt::Result {
    write!(w, "extfunc @{}(", func.name)?;

    let mut first_arg = true;
    for &arg_type in &func.sig.arg_types {
        if !first_arg {
            w.write_str(", ")?;
        }
        first_arg = false;
        write!(w, "{arg_type}")?;
    }

    w.write_str(")\n")
}

fn write_func_ref(w: &mut dyn fmt::Write, module: &Module, func: FunctionRef) -> fmt::Result {
    match func {
        FunctionRef::Internal(func) => write!(w, "func @{}", module.functions[func].name),
        FunctionRef::External(func) => {
            write!(w, "extfunc @{}", module.extern_functions[func].name)
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::{
        module::{ExternFunctionData, FunctionData, Signature},
        node::{DepValueKind, IcmpKind, Type},
        test_utils::create_loop_graph,
    };

    use super::*;

    fn check_write_graph(graph: &ValGraph, entry: Node, expected: Expect) {
        let module = Module::new();
        let mut output = String::new();
        write_graph(&mut output, &module, graph, entry, 0).expect("failed to display graph");
        expected.assert_eq(&output);
    }

    fn check_write_function(function: &FunctionData, expected: Expect) {
        let module = Module::new();
        let mut output = String::new();
        write_function(&mut output, &module, function).expect("failed to display function");
        expected.assert_eq(&output);
    }

    fn check_write_module(module: &Module, expected: Expect) {
        let mut output = String::new();
        write_module(&mut output, module).expect("failed to display module");
        expected.assert_eq(&output);
    }

    #[test]
    fn write_node_kinds() {
        let mut module = Module::new();

        let func = module.functions.push(FunctionData::new(
            "my_func".to_owned(),
            Signature {
                ret_type: Type::I32,
                arg_types: vec![],
            },
        ));

        let extfunc = module.extern_functions.push(ExternFunctionData {
            name: "my_ext_func".to_owned(),
            sig: Signature {
                ret_type: Type::I32,
                arg_types: vec![],
            },
        });

        let check = |kind: NodeKind, expected: &str| {
            let mut graph = ValGraph::new();
            let node = graph.create_node(kind, [], []);
            let mut output = String::new();
            write_node(&mut output, &module, &graph, node, 0).expect("failed to write node");
            assert_eq!(output, expected.to_owned() + "\n");
        };

        let kinds = [
            (NodeKind::Entry, "entry"),
            (NodeKind::Return, "return"),
            (NodeKind::Region, "region"),
            (NodeKind::Phi, "phi"),
            (NodeKind::IConst(5), "iconst 5"),
            (NodeKind::Iadd, "iadd"),
            (NodeKind::Isub, "isub"),
            (NodeKind::And, "and"),
            (NodeKind::Or, "or"),
            (NodeKind::Xor, "xor"),
            (NodeKind::Shl, "shl"),
            (NodeKind::Lshr, "lshr"),
            (NodeKind::Ashr, "ashr"),
            (NodeKind::Smul, "smul"),
            (NodeKind::Umul, "umul"),
            (NodeKind::Sdiv, "sdiv"),
            (NodeKind::Udiv, "udiv"),
            (NodeKind::Icmp(IcmpKind::Eq), "icmp eq"),
            (NodeKind::Icmp(IcmpKind::Ne), "icmp ne"),
            (NodeKind::Icmp(IcmpKind::Slt), "icmp slt"),
            (NodeKind::Icmp(IcmpKind::Sle), "icmp sle"),
            (NodeKind::Icmp(IcmpKind::Ult), "icmp ult"),
            (NodeKind::Icmp(IcmpKind::Ule), "icmp ule"),
            (NodeKind::FConst(2.71), "fconst 2.71"),
            (NodeKind::Load, "load"),
            (NodeKind::Store, "store"),
            (NodeKind::BrCond, "brcond"),
            (
                NodeKind::Call(FunctionRef::Internal(func)),
                "call func @my_func",
            ),
            (
                NodeKind::Call(FunctionRef::External(extfunc)),
                "call extfunc @my_ext_func",
            ),
        ];

        for (kind, expected) in kinds {
            check(kind, expected);
        }
    }

    #[test]
    fn write_add_params_graph() {
        let mut graph = ValGraph::new();
        let entry = graph.create_node(
            NodeKind::Entry,
            [],
            [
                DepValueKind::Control,
                DepValueKind::Value(Type::I32),
                DepValueKind::Value(Type::I32),
            ],
        );
        let entry_outputs = graph.node_outputs(entry);
        let control_value = entry_outputs[0];
        let param1 = entry_outputs[1];
        let param2 = entry_outputs[2];

        let add = graph.create_node(
            NodeKind::Iadd,
            [param1, param2],
            [DepValueKind::Value(Type::I32)],
        );
        let add_res = graph.node_outputs(add)[0];
        graph.create_node(NodeKind::Return, [control_value, add_res], []);

        check_write_graph(
            &graph,
            entry,
            expect![[r#"
                %0:ctrl, %1:val(i32), %2:val(i32) = entry
                %3:val(i32) = iadd %1, %2
                return %0, %3
            "#]],
        );
    }

    #[test]
    fn write_loop_graph() {
        let (graph, entry) = create_loop_graph();

        check_write_graph(
            &graph,
            entry,
            expect![[r#"
                %0:ctrl, %1:val(i32) = entry
                %10:val(i32) = iconst 1
                %2:val(i32) = iconst 0
                %3:val(i32) = icmp eq %1, %2
                %4:ctrl, %5:ctrl = brcond %0, %3
                %13:val(i32) = icmp eq %11, %2
                %14:ctrl, %15:ctrl = brcond %6, %13
                %16:ctrl, %17:phisel = region %4, %14
                %6:ctrl, %7:phisel = region %5, %15
                %8:val(i32) = phi %7, %1, %11
                %11:val(i32) = isub %8, %10
                %9:val(i32) = phi %7, %2, %12
                %12:val(i32) = iadd %9, %8
                %18:val(i32) = phi %17, %2, %12
                return %16, %18
            "#]],
        );
    }

    #[test]
    fn write_add_params_func() {
        let mut function = FunctionData::new(
            "add_i32".to_owned(),
            Signature {
                ret_type: Type::I32,
                arg_types: vec![Type::I32, Type::I32],
            },
        );
        let graph = &mut function.valgraph;
        let entry = function.entry_node;
        let entry_outputs = graph.node_outputs(entry);
        let control_value = entry_outputs[0];
        let param1 = entry_outputs[1];
        let param2 = entry_outputs[2];

        let add = graph.create_node(
            NodeKind::Iadd,
            [param1, param2],
            [DepValueKind::Value(Type::I32)],
        );
        let add_res = graph.node_outputs(add)[0];
        graph.create_node(NodeKind::Return, [control_value, add_res], []);

        check_write_function(
            &function,
            expect![[r#"
                func @add_i32:i32(i32, i32) {
                    %0:ctrl, %1:val(i32), %2:val(i32) = entry
                    %3:val(i32) = iadd %1, %2
                    return %0, %3
                }
            "#]],
        );
    }

    #[test]
    fn write_simple_module() {
        let mut module = Module::new();
        let func = module.functions.push(FunctionData::new(
            "my_func".to_owned(),
            Signature {
                ret_type: Type::I32,
                arg_types: vec![Type::I64],
            },
        ));
        let extfunc = module.extern_functions.push(ExternFunctionData {
            name: "my_ext_func".to_owned(),
            sig: Signature {
                ret_type: Type::I32,
                arg_types: vec![Type::I64],
            },
        });

        let func_entry = module.functions[func].entry_node;
        let func_graph = &mut module.functions[func].valgraph;

        let func_entry_outputs = func_graph.node_outputs(func_entry);
        let func_entry_ctrl = func_entry_outputs[0];
        let func_param1 = func_entry_outputs[1];

        let call = func_graph.create_node(
            NodeKind::Call(FunctionRef::External(extfunc)),
            [func_entry_ctrl, func_param1],
            [DepValueKind::Control, DepValueKind::Value(Type::I32)],
        );

        let call_outputs = func_graph.node_outputs(call);
        let call_ctrl = call_outputs[0];
        let call_retval = call_outputs[1];

        func_graph.create_node(NodeKind::Return, [call_ctrl, call_retval], []);

        check_write_module(
            &module,
            expect![[r#"
                extfunc @my_ext_func(i64)

                func @my_func:i32(i64) {
                    %0:ctrl, %1:val(i64) = entry
                    %2:ctrl, %3:val(i32) = call extfunc @my_ext_func %0, %1
                    return %2, %3
                }
            "#]],
        );
    }
}

use core::fmt;

use crate::{
    module::{ExternFunctionData, FunctionData, Module, Signature},
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

pub fn write_function(w: &mut dyn fmt::Write, module: &Module, func: &FunctionData) -> fmt::Result {
    write!(w, "func @{}", func.name)?;
    write_signature(w, &func.sig)?;
    w.write_str(" {\n")?;
    write_graph(w, module, &func.graph, func.entry, 4)?;
    w.write_str("}\n")
}

pub fn write_graph(
    w: &mut dyn fmt::Write,
    module: &Module,
    graph: &ValGraph,
    entry: Node,
    indentation: u32,
) -> fmt::Result {
    for node in LiveNodeInfo::compute(graph, entry).reverse_postorder(graph) {
        write_node(w, module, graph, node, indentation)?;
        writeln!(w)?;
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
    write_indendation(w, indentation)?;

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

    write_node_kind(w, module, graph.node_kind(node))?;

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

    Ok(())
}

fn write_indendation(w: &mut dyn fmt::Write, indentation: u32) -> fmt::Result {
    for _ in 0..indentation {
        w.write_char(' ')?;
    }
    Ok(())
}

pub fn write_node_kind(
    w: &mut dyn fmt::Write,
    module: &Module,
    node_kind: &NodeKind,
) -> fmt::Result {
    match node_kind {
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
        NodeKind::Imul => w.write_str("imul")?,
        NodeKind::Sdiv => w.write_str("sdiv")?,
        NodeKind::Udiv => w.write_str("udiv")?,
        NodeKind::Icmp(kind) => write!(w, "icmp {kind}")?,
        NodeKind::FConst(val) => write!(w, "fconst {val}")?,
        NodeKind::PtrOff => w.write_str("ptroff")?,
        NodeKind::Load => w.write_str("load")?,
        NodeKind::Store => w.write_str("store")?,
        NodeKind::StackSlot { size, align } => write!(w, "stackslot {size}:{align}")?,
        NodeKind::BrCond => w.write_str("brcond")?,
        NodeKind::Call(func) => {
            w.write_str("call ")?;
            write_func_ref(w, module, *func)?;
        }
    };
    Ok(())
}

fn write_extern_function(w: &mut dyn fmt::Write, func: &ExternFunctionData) -> fmt::Result {
    write!(w, "extfunc @{}", func.name)?;
    write_signature(w, &func.sig)?;
    writeln!(w)
}

fn write_signature(w: &mut dyn fmt::Write, sig: &Signature) -> fmt::Result {
    if let Some(ret_type) = sig.ret_type {
        write!(w, ":{}", ret_type)?;
    }

    w.write_str("(")?;

    let mut first_arg = true;
    for &arg_type in &sig.param_types {
        if !first_arg {
            w.write_str(", ")?;
        }
        first_arg = false;
        write!(w, "{}", arg_type)?;
    }

    w.write_str(")")
}

fn write_func_ref(w: &mut dyn fmt::Write, module: &Module, func: FunctionRef) -> fmt::Result {
    write!(w, "@{}", module.resolve_funcref(func).name)
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::{
        builder::BuilderExt,
        module::{ExternFunctionData, FunctionData, Signature},
        node::{BitwiseF64, DepValueKind, IcmpKind, Type},
        test_utils::{create_entry, create_loop_graph, create_return},
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
        expected.assert_eq(&module.to_string());
    }

    #[test]
    fn write_node_kinds() {
        let mut module = Module::new();

        let func = module.functions.push(FunctionData::new(
            "my_func".to_owned(),
            Signature {
                ret_type: Some(Type::I32),
                param_types: vec![],
            },
        ));

        let extfunc = module.extern_functions.push(ExternFunctionData {
            name: "my_ext_func".to_owned(),
            sig: Signature {
                ret_type: Some(Type::I32),
                param_types: vec![],
            },
        });

        let check = |kind: NodeKind, expected: &str| {
            let mut graph = ValGraph::new();
            let node = graph.create_node(kind, [], []);
            let mut output = String::new();
            write_node(&mut output, &module, &graph, node, 0).expect("failed to write node");
            assert_eq!(output, expected.to_owned());
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
            (NodeKind::Imul, "imul"),
            (NodeKind::Sdiv, "sdiv"),
            (NodeKind::Udiv, "udiv"),
            (NodeKind::Icmp(IcmpKind::Eq), "icmp eq"),
            (NodeKind::Icmp(IcmpKind::Ne), "icmp ne"),
            (NodeKind::Icmp(IcmpKind::Slt), "icmp slt"),
            (NodeKind::Icmp(IcmpKind::Sle), "icmp sle"),
            (NodeKind::Icmp(IcmpKind::Ult), "icmp ult"),
            (NodeKind::Icmp(IcmpKind::Ule), "icmp ule"),
            (NodeKind::FConst(BitwiseF64(2.71)), "fconst 2.71"),
            (NodeKind::PtrOff, "ptroff"),
            (NodeKind::Load, "load"),
            (NodeKind::Store, "store"),
            (NodeKind::StackSlot { size: 8, align: 4 }, "stackslot 8:4"),
            (NodeKind::BrCond, "brcond"),
            (NodeKind::Call(FunctionRef::Internal(func)), "call @my_func"),
            (
                NodeKind::Call(FunctionRef::External(extfunc)),
                "call @my_ext_func",
            ),
        ];

        for (kind, expected) in kinds {
            check(kind, expected);
        }
    }

    #[test]
    fn write_add_params_graph() {
        let mut graph = ValGraph::new();

        let (entry, control_value, [param1, param2]) =
            create_entry(&mut graph, [Type::I32, Type::I32]);

        let add = graph.create_node(
            NodeKind::Iadd,
            [param1, param2],
            [DepValueKind::Value(Type::I32)],
        );
        let add_res = graph.node_outputs(add)[0];
        create_return(&mut graph, [control_value, add_res]);

        check_write_graph(
            &graph,
            entry,
            expect![[r#"
                %0:ctrl, %1:i32, %2:i32 = entry
                %3:i32 = iadd %1, %2
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
                %0:ctrl, %1:i32 = entry
                %10:i32 = iconst 1
                %2:i32 = iconst 0
                %3:i32 = icmp eq %1, %2
                %4:ctrl, %5:ctrl = brcond %0, %3
                %13:i32 = icmp eq %11, %2
                %14:ctrl, %15:ctrl = brcond %6, %13
                %16:ctrl, %17:phisel = region %4, %14
                %6:ctrl, %7:phisel = region %5, %15
                %8:i32 = phi %7, %1, %11
                %11:i32 = isub %8, %10
                %9:i32 = phi %7, %2, %12
                %12:i32 = iadd %9, %8
                %18:i32 = phi %17, %2, %12
                return %16, %18
            "#]],
        );
    }

    #[test]
    fn write_add_params_func() {
        let mut function = FunctionData::new(
            "add_i32".to_owned(),
            Signature {
                ret_type: Some(Type::I32),
                param_types: vec![Type::I32, Type::I32],
            },
        );
        let graph = &mut function.graph;
        let entry = function.entry;
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
        create_return(graph, [control_value, add_res]);

        check_write_function(
            &function,
            expect![[r#"
                func @add_i32:i32(i32, i32) {
                    %0:ctrl, %1:i32, %2:i32 = entry
                    %3:i32 = iadd %1, %2
                    return %0, %3
                }
            "#]],
        );
    }

    #[test]
    fn write_void_func() {
        let mut function = FunctionData::new(
            "nop".to_owned(),
            Signature {
                ret_type: None,
                param_types: vec![Type::I32],
            },
        );
        let graph = &mut function.graph;
        let control_value = graph.node_outputs(function.entry)[0];
        create_return(graph, [control_value]);
        check_write_function(
            &function,
            expect![[r#"
                func @nop(i32) {
                    %0:ctrl, %1:i32 = entry
                    return %0
                }
            "#]],
        );
    }

    #[test]
    fn write_stack_slot_func() {
        let mut function = FunctionData::new(
            "with_slots".to_owned(),
            Signature {
                ret_type: None,
                param_types: vec![Type::I32, Type::F64],
            },
        );
        let graph = &mut function.graph;
        let entry_outputs = graph.node_outputs(function.entry);
        let entry_ctrl = entry_outputs[0];
        let param32 = entry_outputs[1];
        let param64 = entry_outputs[2];

        let addr32 = graph.build_stackslot(4, 4);
        let addr64 = graph.build_stackslot(8, 8);

        let store32_ctrl = graph.build_store(entry_ctrl, param32, addr32);
        let store64_ctrl = graph.build_store(store32_ctrl, param64, addr64);
        graph.build_return(store64_ctrl, None);

        check_write_function(
            &function,
            expect![[r#"
                func @with_slots(i32, f64) {
                    %0:ctrl, %1:i32, %2:f64 = entry
                    %4:ptr = stackslot 8:8
                    %3:ptr = stackslot 4:4
                    %5:ctrl = store %0, %1, %3
                    %6:ctrl = store %5, %2, %4
                    return %6
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
                ret_type: Some(Type::I32),
                param_types: vec![Type::I64],
            },
        ));
        let extfunc = module.extern_functions.push(ExternFunctionData {
            name: "my_ext_func".to_owned(),
            sig: Signature {
                ret_type: Some(Type::I32),
                param_types: vec![Type::I64],
            },
        });

        let func_entry = module.functions[func].entry;
        let func_graph = &mut module.functions[func].graph;

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

        create_return(func_graph, [call_ctrl, call_retval]);

        check_write_module(
            &module,
            expect![[r#"
                extfunc @my_ext_func:i32(i64)

                func @my_func:i32(i64) {
                    %0:ctrl, %1:i64 = entry
                    %2:ctrl, %3:i32 = call @my_ext_func %0, %1
                    return %2, %3
                }
            "#]],
        );
    }

    #[test]
    fn write_multi_extfunc_module() {
        let mut module = Module::new();
        module.extern_functions.push(ExternFunctionData {
            name: "func1".to_owned(),
            sig: Signature {
                ret_type: Some(Type::I32),
                param_types: vec![Type::I64],
            },
        });
        module.extern_functions.push(ExternFunctionData {
            name: "func2".to_owned(),
            sig: Signature {
                ret_type: None,
                param_types: vec![Type::I64, Type::Ptr],
            },
        });
        module.extern_functions.push(ExternFunctionData {
            name: "func3".to_owned(),
            sig: Signature {
                ret_type: None,
                param_types: vec![],
            },
        });

        check_write_module(
            &module,
            expect![[r#"
                extfunc @func1:i32(i64)
                extfunc @func2(i64, ptr)
                extfunc @func3()

            "#]],
        );
    }
}

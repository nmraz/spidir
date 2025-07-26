use core::{fmt, iter};

use alloc::{borrow::Cow, format, vec::Vec};

use crate::{
    function::{FunctionBody, FunctionBorrow, FunctionMetadata, Signature},
    module::{ExternFunction, Function, Module},
    node::{FunctionRef, NodeKind},
    valgraph::{DepValue, Node, ValGraph},
};

pub trait AnnotateGraph<W: fmt::Write + ?Sized> {
    fn write_node(
        &mut self,
        w: &mut W,
        module: &Module,
        body: &FunctionBody,
        node: Node,
    ) -> fmt::Result {
        write_annotated_node(w, self, module, body, node)
    }

    fn write_node_output(&mut self, w: &mut W, graph: &ValGraph, output: DepValue) -> fmt::Result {
        write_value_def(w, graph, output)
    }

    fn write_node_kind(
        &mut self,
        mut w: &mut W,
        module: &Module,
        body: &FunctionBody,
        node_kind: &NodeKind,
    ) -> fmt::Result {
        // This is a little ugly because it can cause double-vtables if `W` is already
        // `dyn fmt::Write`, but there isn't a way to let this trait know the concrete type `W` (and
        // allow it to be unsized) and have `write_node_kind` accept type-erased writers without the
        // extra indirection.
        write_node_kind(&mut w, module, body, node_kind)
    }

    fn write_node_input(
        &mut self,
        w: &mut W,
        graph: &ValGraph,
        node: Node,
        input: usize,
    ) -> fmt::Result {
        write!(w, "{}", graph.node_inputs(node)[input])
    }
}

pub trait AnnotateModule<W: fmt::Write + ?Sized>: AnnotateGraph<W> {
    fn write_function(&mut self, w: &mut W, module: &Module, func: Function) -> fmt::Result {
        write_annotated_function(w, self, module, module.borrow_function(func))
    }

    fn write_extern_function(
        &mut self,
        mut w: &mut W,
        module: &Module,
        func: ExternFunction,
    ) -> fmt::Result {
        // As above, we need the extra indirection to allow the type `W` to be unsized but allow
        // `write_extern_function` to operate on type-erased writers.
        write_extern_function(&mut w, &module.metadata.extern_functions[func])
    }
}

struct DefaultAnnotator;

impl AnnotateGraph<dyn fmt::Write + '_> for DefaultAnnotator {}
impl AnnotateModule<dyn fmt::Write + '_> for DefaultAnnotator {}

pub fn display_node<'a>(
    module: &'a Module,
    body: &'a FunctionBody,
    node: Node,
) -> impl fmt::Display + 'a {
    DisplayNode { module, body, node }
}

struct DisplayNode<'a> {
    module: &'a Module,
    body: &'a FunctionBody,
    node: Node,
}

impl fmt::Display for DisplayNode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_node(f, self.module, self.body, self.node)
    }
}

pub fn write_module(w: &mut dyn fmt::Write, module: &Module) -> fmt::Result {
    write_annotated_module(w, &mut DefaultAnnotator, module)
}

pub fn write_annotated_module<W: fmt::Write + ?Sized>(
    w: &mut W,
    annotator: &mut (impl AnnotateModule<W> + ?Sized),
    module: &Module,
) -> fmt::Result {
    for extern_func in module.metadata.extern_functions.keys() {
        annotator.write_extern_function(w, module, extern_func)?;
    }
    w.write_str("\n")?;

    let mut first_function = true;
    for func in module.functions.keys() {
        if !first_function {
            w.write_str("\n")?;
        }
        first_function = false;
        annotator.write_function(w, module, func)?;
    }

    Ok(())
}

pub fn write_function(
    w: &mut dyn fmt::Write,
    module: &Module,
    func: FunctionBorrow<'_>,
) -> fmt::Result {
    write_annotated_function(w, &mut DefaultAnnotator, module, func)
}

pub fn write_annotated_function<W: fmt::Write + ?Sized>(
    w: &mut W,
    annotator: &mut (impl AnnotateGraph<W> + ?Sized),
    module: &Module,
    func: FunctionBorrow<'_>,
) -> fmt::Result {
    write!(w, "func {}", func.metadata)?;
    w.write_str(" {\n")?;
    write_annotated_body(w, annotator, module, func.body(), 4)?;
    w.write_str("}\n")
}

pub fn write_body(
    w: &mut dyn fmt::Write,
    module: &Module,
    body: &FunctionBody,
    indentation: u32,
) -> fmt::Result {
    write_annotated_body(w, &mut DefaultAnnotator, module, body, indentation)
}

pub fn write_annotated_body<W: fmt::Write + ?Sized>(
    w: &mut W,
    annotator: &mut (impl AnnotateGraph<W> + ?Sized),
    module: &Module,
    body: &FunctionBody,
    indentation: u32,
) -> fmt::Result {
    let mut nodes = body.compute_full_walk_info().reverse_postorder(&body.graph);

    // Split out all nodes with identity so we print them first, right after the entry. This both
    // makes the graph more readable (stack slots and such end up near the beginning) and makes
    // it easier to DAG-match in the filetests.
    let mut nodes_with_identity = Vec::new();
    nodes.retain(|&node| {
        if body.graph.node_kind(node).has_identity() {
            nodes_with_identity.push(node);
            false
        } else {
            true
        }
    });

    // Try to keep the special nodes at the beginning relatively stable.
    nodes_with_identity.sort_unstable_by_key(|&node| node.as_u32());

    // Build the final output order:
    // 1. Entry node
    // 2. Nodes with identity
    // 3. Other nodes
    let nodes = iter::once(nodes[0])
        .chain(nodes_with_identity)
        .chain(nodes[1..].iter().copied());

    for node in nodes {
        write_indendation(w, indentation)?;
        annotator.write_node(w, module, body, node)?;
        writeln!(w)?;
    }

    Ok(())
}

pub fn write_node(
    w: &mut dyn fmt::Write,
    module: &Module,
    body: &FunctionBody,
    node: Node,
) -> fmt::Result {
    write_annotated_node(w, &mut DefaultAnnotator, module, body, node)
}

pub fn write_annotated_node<W: fmt::Write + ?Sized>(
    w: &mut W,
    annotator: &mut (impl AnnotateGraph<W> + ?Sized),
    module: &Module,
    body: &FunctionBody,
    node: Node,
) -> fmt::Result {
    let graph = &body.graph;
    let outputs = graph.node_outputs(node);

    if !outputs.is_empty() {
        let mut first = true;
        for output in outputs {
            if !first {
                w.write_str(", ")?;
            }
            first = false;
            annotator.write_node_output(w, graph, output)?;
        }
        w.write_str(" = ")?;
    }

    annotator.write_node_kind(w, module, body, graph.node_kind(node))?;

    for input in 0..graph.node_inputs(node).len() {
        if input == 0 {
            w.write_str(" ")?;
        } else {
            w.write_str(", ")?;
        }
        annotator.write_node_input(w, graph, node, input)?;
    }

    Ok(())
}

fn write_indendation(w: &mut (impl fmt::Write + ?Sized), indentation: u32) -> fmt::Result {
    for _ in 0..indentation {
        w.write_char(' ')?;
    }
    Ok(())
}

#[inline]
pub fn write_value_def(
    w: &mut (impl fmt::Write + ?Sized),
    graph: &ValGraph,
    output: DepValue,
) -> fmt::Result {
    write!(w, "{output}:{}", graph.value_kind(output))
}

pub fn write_node_kind(
    w: &mut dyn fmt::Write,
    module: &Module,
    body: &FunctionBody,
    node_kind: &NodeKind,
) -> fmt::Result {
    match node_kind {
        NodeKind::Entry => w.write_str("entry")?,
        NodeKind::Return => w.write_str("return")?,
        NodeKind::Region => w.write_str("region")?,
        NodeKind::Unreachable => w.write_str("unreachable")?,
        NodeKind::Phi => w.write_str("phi")?,
        NodeKind::Iconst(val) => write!(w, "iconst {val}")?,
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
        NodeKind::Srem => w.write_str("srem")?,
        NodeKind::Urem => w.write_str("urem")?,
        NodeKind::Iext => w.write_str("iext")?,
        NodeKind::Itrunc => w.write_str("itrunc")?,
        NodeKind::Sfill(width) => write!(w, "sfill {width}")?,
        NodeKind::Icmp(kind) => write!(w, "icmp {kind}")?,
        NodeKind::Fconst64(val) => write!(w, "fconst64 {val}")?,
        NodeKind::Fadd => write!(w, "fadd")?,
        NodeKind::Fsub => write!(w, "fsub")?,
        NodeKind::Fmul => write!(w, "fmul")?,
        NodeKind::Fdiv => write!(w, "fdiv")?,
        NodeKind::Fcmp(kind) => write!(w, "fcmp {kind}")?,
        NodeKind::SintToFloat => write!(w, "sinttofloat")?,
        NodeKind::UintToFloat => write!(w, "uinttofloat")?,
        NodeKind::FloatToSint => write!(w, "floattosint")?,
        NodeKind::FloatToUint => write!(w, "floattouint")?,
        NodeKind::PtrOff => w.write_str("ptroff")?,
        NodeKind::IntToPtr => w.write_str("inttoptr")?,
        NodeKind::PtrToInt => w.write_str("ptrtoint")?,
        NodeKind::Load(size) => write!(w, "load.{}", size.as_str())?,
        NodeKind::Store(size) => write!(w, "store.{}", size.as_str())?,
        NodeKind::StackSlot { size, align } => write!(w, "stackslot {size}:{align}")?,
        NodeKind::BrCond => w.write_str("brcond")?,
        NodeKind::FuncAddr(func) => {
            w.write_str("funcaddr ")?;
            write_func_ref(w, module, *func)?;
        }
        NodeKind::Call(func) => {
            w.write_str("call ")?;
            write_func_ref(w, module, *func)?;
        }
        NodeKind::CallInd(sig) => {
            w.write_str("callind ")?;
            write_signature(w, &body.call_ind_sigs[*sig], false)?;
        }
    };
    Ok(())
}

pub fn write_extern_function(w: &mut dyn fmt::Write, metadata: &FunctionMetadata) -> fmt::Result {
    write!(w, "extfunc {metadata}")?;
    writeln!(w)
}

pub fn quote_ident(ident: &str) -> Cow<'_, str> {
    if ident.contains(|c| !is_unquoted_ident_char(c)) {
        format!(r#""{}""#, ident.replace('\"', "\\\"")).into()
    } else {
        ident.into()
    }
}

pub fn write_function_metadata(w: &mut dyn fmt::Write, metadata: &FunctionMetadata) -> fmt::Result {
    write!(w, "@{}", quote_ident(&metadata.name))?;
    write_signature(w, &metadata.sig, true)
}

fn write_signature(w: &mut dyn fmt::Write, sig: &Signature, after_name: bool) -> fmt::Result {
    if let Some(ret_type) = sig.ret_type {
        if after_name {
            write!(w, ":")?;
        }
        write!(w, "{ret_type}")?;
    }

    w.write_str("(")?;

    let mut first_arg = true;
    for &arg_type in &sig.param_types {
        if !first_arg {
            w.write_str(", ")?;
        }
        first_arg = false;
        write!(w, "{arg_type}")?;
    }

    w.write_str(")")
}

fn write_func_ref(w: &mut dyn fmt::Write, module: &Module, func: FunctionRef) -> fmt::Result {
    write!(
        w,
        "@{}",
        quote_ident(&module.metadata.resolve_funcref(func).name)
    )
}

fn is_unquoted_ident_char(c: char) -> bool {
    // Note: keep this in sync with the unquoted identifier rules in the parser
    c == '_' || c == '-' || c.is_ascii_alphanumeric()
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};

    use crate::{
        builder::{BuilderExt, SimpleBuilder},
        function::FunctionData,
        node::{BitwiseF64, FcmpKind, IcmpKind, MemSize, Type},
        test_utils::create_loop_body,
    };

    use super::*;

    fn create_func_and_metadata(name: &str, sig: Signature) -> (FunctionData, FunctionMetadata) {
        let function = FunctionData::new(&sig.param_types);
        let metadata = FunctionMetadata {
            name: name.to_owned(),
            sig,
        };
        (function, metadata)
    }

    fn check_write_body(body: &FunctionBody, expected: Expect) {
        let module = Module::new();
        let mut output = String::new();
        write_body(&mut output, &module, body, 0).expect("failed to display graph");
        expected.assert_eq(&output);
    }

    fn check_write_function(
        function: &FunctionData,
        metadata: &FunctionMetadata,
        expected: Expect,
    ) {
        let module = Module::new();
        let mut output = String::new();
        write_function(
            &mut output,
            &module,
            FunctionBorrow {
                data: function,
                metadata,
            },
        )
        .expect("failed to display function");
        expected.assert_eq(&output);
    }

    fn check_write_module(module: &Module, expected: Expect) {
        expected.assert_eq(&module.to_string());
    }

    #[test]
    fn write_node_kinds() {
        let mut module = Module::new();

        let func = module.create_function(
            "my_func".to_owned(),
            Signature {
                ret_type: Some(Type::I32),
                param_types: vec![],
            },
        );

        let extfunc = module.metadata.extern_functions.push(FunctionMetadata {
            name: "my_ext_func".to_owned(),
            sig: Signature {
                ret_type: Some(Type::I32),
                param_types: vec![],
            },
        });

        let mut body = FunctionBody::new_invalid();
        let sig1 = body.call_ind_sigs.push(Signature {
            ret_type: None,
            param_types: vec![],
        });
        let sig2 = body.call_ind_sigs.push(Signature {
            ret_type: None,
            param_types: vec![Type::I64],
        });
        let sig3 = body.call_ind_sigs.push(Signature {
            ret_type: Some(Type::I32),
            param_types: vec![Type::Ptr],
        });

        let mut check = |kind: NodeKind, expected: &str| {
            let node = body.graph.create_node(kind, [], []);
            let mut output = String::new();
            write_node(&mut output, &module, &body, node).expect("failed to write node");
            assert_eq!(output, expected.to_owned());
        };

        let kinds = [
            (NodeKind::Entry, "entry"),
            (NodeKind::Return, "return"),
            (NodeKind::Region, "region"),
            (NodeKind::Unreachable, "unreachable"),
            (NodeKind::Phi, "phi"),
            (NodeKind::Iconst(5), "iconst 5"),
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
            (NodeKind::Srem, "srem"),
            (NodeKind::Urem, "urem"),
            (NodeKind::Iext, "iext"),
            (NodeKind::Itrunc, "itrunc"),
            (NodeKind::Sfill(8), "sfill 8"),
            (NodeKind::Sfill(16), "sfill 16"),
            (NodeKind::Icmp(IcmpKind::Eq), "icmp eq"),
            (NodeKind::Icmp(IcmpKind::Ne), "icmp ne"),
            (NodeKind::Icmp(IcmpKind::Slt), "icmp slt"),
            (NodeKind::Icmp(IcmpKind::Sle), "icmp sle"),
            (NodeKind::Icmp(IcmpKind::Ult), "icmp ult"),
            (NodeKind::Icmp(IcmpKind::Ule), "icmp ule"),
            (
                NodeKind::Fconst64(BitwiseF64(2.71)),
                "fconst64 0x1.5ae147ae147aep1",
            ),
            (
                NodeKind::Fconst64(BitwiseF64(f64::INFINITY)),
                "fconst64 inf",
            ),
            (
                NodeKind::Fconst64(BitwiseF64(-f64::INFINITY)),
                "fconst64 -inf",
            ),
            (NodeKind::Fconst64(BitwiseF64(f64::NAN)), "fconst64 NaN"),
            (NodeKind::Fadd, "fadd"),
            (NodeKind::Fsub, "fsub"),
            (NodeKind::Fmul, "fmul"),
            (NodeKind::Fdiv, "fdiv"),
            (NodeKind::Fcmp(FcmpKind::Oeq), "fcmp oeq"),
            (NodeKind::Fcmp(FcmpKind::One), "fcmp one"),
            (NodeKind::Fcmp(FcmpKind::Olt), "fcmp olt"),
            (NodeKind::Fcmp(FcmpKind::Ole), "fcmp ole"),
            (NodeKind::Fcmp(FcmpKind::Ueq), "fcmp ueq"),
            (NodeKind::Fcmp(FcmpKind::Une), "fcmp une"),
            (NodeKind::Fcmp(FcmpKind::Ult), "fcmp ult"),
            (NodeKind::Fcmp(FcmpKind::Ule), "fcmp ule"),
            (NodeKind::SintToFloat, "sinttofloat"),
            (NodeKind::UintToFloat, "uinttofloat"),
            (NodeKind::FloatToSint, "floattosint"),
            (NodeKind::FloatToUint, "floattouint"),
            (NodeKind::PtrOff, "ptroff"),
            (NodeKind::IntToPtr, "inttoptr"),
            (NodeKind::PtrToInt, "ptrtoint"),
            (NodeKind::Load(MemSize::S1), "load.1"),
            (NodeKind::Load(MemSize::S2), "load.2"),
            (NodeKind::Store(MemSize::S4), "store.4"),
            (NodeKind::Store(MemSize::S8), "store.8"),
            (NodeKind::StackSlot { size: 8, align: 4 }, "stackslot 8:4"),
            (NodeKind::BrCond, "brcond"),
            (
                NodeKind::FuncAddr(FunctionRef::Internal(func)),
                "funcaddr @my_func",
            ),
            (NodeKind::Call(FunctionRef::Internal(func)), "call @my_func"),
            (
                NodeKind::Call(FunctionRef::External(extfunc)),
                "call @my_ext_func",
            ),
            (NodeKind::CallInd(sig1), "callind ()"),
            (NodeKind::CallInd(sig2), "callind (i64)"),
            (NodeKind::CallInd(sig3), "callind i32(ptr)"),
        ];

        for (kind, expected) in kinds {
            check(kind, expected);
        }
    }

    #[test]
    fn write_add_params_body() {
        let mut body = FunctionBody::new(&[Type::I32, Type::I32]);
        let ctrl = body.entry_ctrl();
        let param1 = body.param_value(0);
        let param2 = body.param_value(1);

        let mut builder = SimpleBuilder(&mut body);
        let add = builder.build_iadd(param1, param2);
        builder.build_return(ctrl, Some(add));

        check_write_body(
            &body,
            expect![[r#"
                %0:ctrl, %1:i32, %2:i32 = entry
                %3:i32 = iadd %1, %2
                return %0, %3
            "#]],
        );
    }

    #[test]
    fn write_loop_body() {
        let body = create_loop_body();

        check_write_body(
            &body,
            expect![[r#"
                %0:ctrl, %1:i32 = entry
                %10:i32 = iconst 1
                %2:i32 = iconst 0
                %3:i32 = icmp eq %1, %2
                %4:ctrl, %5:ctrl = brcond %0, %3
                %6:ctrl, %7:phisel = region %5, %15
                %8:i32 = phi %7, %1, %11
                %12:i32 = iadd %9, %8
                %9:i32 = phi %7, %2, %12
                %11:i32 = isub %8, %10
                %13:i32 = icmp eq %11, %2
                %14:ctrl, %15:ctrl = brcond %6, %13
                %16:ctrl, %17:phisel = region %4, %14
                %18:i32 = phi %17, %2, %12
                return %16, %18
            "#]],
        );
    }

    #[test]
    fn write_add_params_func() {
        let (mut function, metadata) = create_func_and_metadata(
            "add_i32",
            Signature {
                ret_type: Some(Type::I32),
                param_types: vec![Type::I32, Type::I32],
            },
        );

        let body = &mut function.body;

        let ctrl = body.entry_ctrl();
        let param1 = body.param_value(0);
        let param2 = body.param_value(1);

        let mut builder = SimpleBuilder(body);
        let add = builder.build_iadd(param1, param2);
        builder.build_return(ctrl, Some(add));

        check_write_function(
            &function,
            &metadata,
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
        let (mut function, metadata) = create_func_and_metadata(
            "nop",
            Signature {
                ret_type: None,
                param_types: vec![Type::I32],
            },
        );

        let ctrl = function.body.entry_ctrl();
        SimpleBuilder(&mut function.body).build_return(ctrl, None);

        check_write_function(
            &function,
            &metadata,
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
        let (mut function, metadata) = create_func_and_metadata(
            "with_slots",
            Signature {
                ret_type: None,
                param_types: vec![Type::I32, Type::F64],
            },
        );

        let entry_ctrl = function.body.entry_ctrl();
        let param32 = function.body.param_value(0);
        let param64 = function.body.param_value(1);

        let mut builder = SimpleBuilder(&mut function.body);

        let addr32 = builder.build_stackslot(4, 4);
        let addr64 = builder.build_stackslot(8, 8);

        let store32_ctrl = builder.build_store(MemSize::S4, entry_ctrl, param32, addr32);
        let store64_ctrl = builder.build_store(MemSize::S8, store32_ctrl, param64, addr64);
        builder.build_return(store64_ctrl, None);

        check_write_function(
            &function,
            &metadata,
            expect![[r#"
                func @with_slots(i32, f64) {
                    %0:ctrl, %1:i32, %2:f64 = entry
                    %3:ptr = stackslot 4:4
                    %4:ptr = stackslot 8:8
                    %5:ctrl = store.4 %0, %1, %3
                    %6:ctrl = store.8 %5, %2, %4
                    return %6
                }
            "#]],
        );
    }

    #[test]
    fn write_simple_module() {
        let mut module = Module::new();
        let func = module.create_function(
            "my_func".to_owned(),
            Signature {
                ret_type: Some(Type::I32),
                param_types: vec![Type::I64],
            },
        );
        let extfunc = module.metadata.extern_functions.push(FunctionMetadata {
            name: "my_ext_func".to_owned(),
            sig: Signature {
                ret_type: Some(Type::I32),
                param_types: vec![Type::I64],
            },
        });

        let body = &mut module.functions[func].body;
        let entry_ctrl = body.entry_ctrl();
        let param1 = body.param_value(0);

        let mut builder = SimpleBuilder(body);
        let call = builder.build_call(
            Some(Type::I32),
            FunctionRef::External(extfunc),
            entry_ctrl,
            &[param1],
        );
        builder.build_return(call.ctrl, call.retval);

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
        module.metadata.extern_functions.push(FunctionMetadata {
            name: "func1".to_owned(),
            sig: Signature {
                ret_type: Some(Type::I32),
                param_types: vec![Type::I64],
            },
        });
        module.metadata.extern_functions.push(FunctionMetadata {
            name: "func2".to_owned(),
            sig: Signature {
                ret_type: None,
                param_types: vec![Type::I64, Type::Ptr],
            },
        });
        module.metadata.extern_functions.push(FunctionMetadata {
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

    #[test]
    fn write_quoted_ident_module() {
        let mut module = Module::new();

        module.create_function(
            "a function".to_owned(),
            Signature {
                ret_type: None,
                param_types: vec![],
            },
        );

        module.metadata.extern_functions.push(FunctionMetadata {
            name: "System.Test+Lol System.Test::Do(Lol[])".to_owned(),
            sig: Signature {
                ret_type: None,
                param_types: vec![],
            },
        });

        check_write_module(
            &module,
            expect![[r#"
                extfunc @"System.Test+Lol System.Test::Do(Lol[])"()

                func @"a function"() {
                    %0:ctrl = entry
                }
            "#]],
        );
    }
}

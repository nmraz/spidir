use expect_test::{expect, Expect};
use ir::{module::FunctionData, verify::verify_func};
use parser::parse_module;

use crate::annotate::{ColoredAnnotator, ErrorAnnotator, PlainAnnotator};

use super::*;

fn graphviz_with_annotator(
    annotator: &mut dyn Annotate,
    module: &Module,
    func: &FunctionData,
) -> String {
    let mut graphviz = String::new();
    write_graphviz(
        &mut graphviz,
        annotator,
        module,
        &func.graph,
        func.entry,
        &[],
    )
    .unwrap();
    graphviz
}

#[track_caller]
fn check_dump_graphviz(input: &str, expected_plain: Expect, expected_colored: Expect) {
    let module = parse_module(input).unwrap();
    let func = module.functions.values().next().unwrap();
    expected_plain.assert_eq(&graphviz_with_annotator(&mut PlainAnnotator, &module, func));
    expected_colored.assert_eq(&graphviz_with_annotator(
        &mut ColoredAnnotator,
        &module,
        func,
    ));
}

#[track_caller]
fn check_dump_graphviz_verifier_errors(input: &str, expected: Expect) {
    let module = parse_module(input).unwrap();
    let func = module.functions.values().next().unwrap();
    let errors = verify_func(&module, func).unwrap_err();
    expected.assert_eq(&graphviz_with_annotator(
        &mut ErrorAnnotator::new(&func.graph, &errors),
        &module,
        func,
    ));
}

#[test]
fn dump_simple_graph() {
    check_dump_graphviz(
        "
            extfunc @my_ext_func:i32(i64)
            func @my_func:i32(i64) {
                %0:ctrl, %1:i64 = entry
                %2:ctrl, %3:i32 = call @my_ext_func %0, %1
                return %2, %3
            }",
        expect![[r#"
            digraph {
                node0 [shape=Mrecord, label="{entry | {<o0> ctrl | <o1> i64}}"]
                node1 [shape=Mrecord, label="{{<i0> | <i1>} | call @my_ext_func | {<o0> ctrl | <o1> i32}}"]
                node2 [shape=Mrecord, label="{{<i0> | <i1>} | return}"]
                node0:o0 -> node1:i0
                node0:o1 -> node1:i1
                node1:o0 -> node2:i0
                node1:o1 -> node2:i1
            }
        "#]],
        expect![[r##"
            digraph {
                node0 [shape=Mrecord, label="{entry | {<o0> ctrl | <o1> i64}}", fillcolor="#ffd3e4", style="filled"]
                node1 [shape=Mrecord, label="{{<i0> | <i1>} | call @my_ext_func | {<o0> ctrl | <o1> i32}}", fillcolor="#ffd3e4", style="filled"]
                node2 [shape=Mrecord, label="{{<i0> | <i1>} | return}", fillcolor="#ffd3e4", style="filled"]
                node0:o0 -> node1:i0 [color="#0000ff", penwidth="2"]
                node0:o1 -> node1:i1 [color="#d36805"]
                node1:o0 -> node2:i0 [color="#0000ff", penwidth="2"]
                node1:o1 -> node2:i1 [color="#d36805"]
            }
        "##]],
    );
}

#[test]
fn dump_iota_graph() {
    check_dump_graphviz(
        "
        func @iota(ptr, i64) {
            %entry_ctrl:ctrl, %arr:ptr, %n:i64 = entry
            %zero:i64 = iconst 0
            %zerocmp:i32 = icmp eq %n, %zero
            %iszero:ctrl, %isnonzero:ctrl = brcond %entry_ctrl, %zerocmp
            %loopbody:ctrl, %loopphi:phisel = region %isnonzero, %looplatch
            %i:i64 = phi %loopphi, %zero, %inext
            %three:i64 = iconst 3
            %off:i64 = shl %i, %three
            %ptr:ptr = ptroff %arr, %off
            %poststore:ctrl = store %loopbody, %i, %ptr
            %one:i64 = iconst 1
            %inext:i64 = iadd %i, %one
            %donecmp:i32 = icmp eq %inext, %n
            %loopdone:ctrl, %looplatch:ctrl = brcond %poststore, %donecmp
            %exit:ctrl, %exitphi:phisel = region %iszero, %loopdone
            return %exit
        }
        ",
        expect![[r#"
            digraph {
                node0 [shape=Mrecord, label="{entry | {<o0> ctrl | <o1> ptr | <o2> i64}}"]
                node10 [shape=Mrecord, label="{iconst 1 | {<o0> i64}}"]
                node1 [shape=Mrecord, label="{iconst 0 | {<o0> i64}}"]
                node2 [shape=Mrecord, label="{{<i0> | <i1>} | icmp eq | {<o0> i32}}"]
                node3 [shape=Mrecord, label="{{<i0> | <i1>} | brcond | {<o0> ctrl | <o1> ctrl}}"]
                node6 [shape=Mrecord, label="{iconst 3 | {<o0> i64}}"]
                node7 [shape=Mrecord, label="{{<i0> | <i1>} | shl | {<o0> i64}}"]
                node8 [shape=Mrecord, label="{{<i0> | <i1>} | ptroff | {<o0> ptr}}"]
                node9 [shape=Mrecord, label="{{<i0> | <i1> | <i2>} | store | {<o0> ctrl}}"]
                node13 [shape=Mrecord, label="{{<i0> | <i1>} | brcond | {<o0> ctrl | <o1> ctrl}}"]
                node14 [shape=Mrecord, label="{{<i0> | <i1>} | region | {<o0> ctrl | <o1> phisel}}"]
                node15 [shape=Mrecord, label="{{<i0>} | return}"]
                node4 [shape=Mrecord, label="{{<i0> | <i1>} | region | {<o0> ctrl | <o1> phisel}}"]
                node5 [shape=Mrecord, label="{{<i0> | <i1> | <i2>} | phi | {<o0> i64}}"]
                node11 [shape=Mrecord, label="{{<i0> | <i1>} | iadd | {<o0> i64}}"]
                node12 [shape=Mrecord, label="{{<i0> | <i1>} | icmp eq | {<o0> i32}}"]
                node0:o2 -> node2:i0
                node1:o0 -> node2:i1
                node0:o0 -> node3:i0
                node2:o0 -> node3:i1
                node5:o0 -> node7:i0
                node6:o0 -> node7:i1
                node0:o1 -> node8:i0
                node7:o0 -> node8:i1
                node4:o0 -> node9:i0
                node5:o0 -> node9:i1
                node8:o0 -> node9:i2
                node9:o0 -> node13:i0
                node12:o0 -> node13:i1
                node3:o0 -> node14:i0
                node13:o0 -> node14:i1
                node14:o0 -> node15:i0
                node3:o1 -> node4:i0
                node13:o1 -> node4:i1
                node4:o1 -> node5:i0
                node1:o0 -> node5:i1
                node11:o0 -> node5:i2
                node5:o0 -> node11:i0
                node10:o0 -> node11:i1
                node11:o0 -> node12:i0
                node0:o2 -> node12:i1
            }
        "#]],
        expect![[r##"
            digraph {
                node0 [shape=Mrecord, label="{entry | {<o0> ctrl | <o1> ptr | <o2> i64}}", fillcolor="#ffd3e4", style="filled"]
                node10 [shape=Mrecord, label="{iconst 1 | {<o0> i64}}", fillcolor="#ffee9b", style="filled"]
                node1 [shape=Mrecord, label="{iconst 0 | {<o0> i64}}", fillcolor="#ffee9b", style="filled"]
                node2 [shape=Mrecord, label="{{<i0> | <i1>} | icmp eq | {<o0> i32}}", fillcolor="#ffee9b", style="filled"]
                node3 [shape=Mrecord, label="{{<i0> | <i1>} | brcond | {<o0> ctrl | <o1> ctrl}}", fillcolor="#ffd3e4", style="filled"]
                node6 [shape=Mrecord, label="{iconst 3 | {<o0> i64}}", fillcolor="#ffee9b", style="filled"]
                node7 [shape=Mrecord, label="{{<i0> | <i1>} | shl | {<o0> i64}}", fillcolor="#ffee9b", style="filled"]
                node8 [shape=Mrecord, label="{{<i0> | <i1>} | ptroff | {<o0> ptr}}", fillcolor="#ffee9b", style="filled"]
                node9 [shape=Mrecord, label="{{<i0> | <i1> | <i2>} | store | {<o0> ctrl}}", fillcolor="#ffd3e4", style="filled"]
                node13 [shape=Mrecord, label="{{<i0> | <i1>} | brcond | {<o0> ctrl | <o1> ctrl}}", fillcolor="#ffd3e4", style="filled"]
                node14 [shape=Mrecord, label="{{<i0> | <i1>} | region | {<o0> ctrl | <o1> phisel}}", fillcolor="#c2c2ff", style="filled"]
                node15 [shape=Mrecord, label="{{<i0>} | return}", fillcolor="#c2c2ff", style="filled"]
                node4 [shape=Mrecord, label="{{<i0> | <i1>} | region | {<o0> ctrl | <o1> phisel}}", fillcolor="#c2c2ff", style="filled"]
                node5 [shape=Mrecord, label="{{<i0> | <i1> | <i2>} | phi | {<o0> i64}}", fillcolor="#dbdbdb", style="filled"]
                node11 [shape=Mrecord, label="{{<i0> | <i1>} | iadd | {<o0> i64}}", fillcolor="#ffee9b", style="filled"]
                node12 [shape=Mrecord, label="{{<i0> | <i1>} | icmp eq | {<o0> i32}}", fillcolor="#ffee9b", style="filled"]
                node0:o2 -> node2:i0 [color="#d36805"]
                node1:o0 -> node2:i1 [color="#d36805"]
                node0:o0 -> node3:i0 [color="#0000ff", penwidth="2"]
                node2:o0 -> node3:i1 [color="#d36805"]
                node5:o0 -> node7:i0 [color="#d36805"]
                node6:o0 -> node7:i1 [color="#d36805"]
                node0:o1 -> node8:i0 [color="#d36805"]
                node7:o0 -> node8:i1 [color="#d36805"]
                node4:o0 -> node9:i0 [color="#0000ff", penwidth="2"]
                node5:o0 -> node9:i1 [color="#d36805"]
                node8:o0 -> node9:i2 [color="#d36805"]
                node9:o0 -> node13:i0 [color="#0000ff", penwidth="2"]
                node12:o0 -> node13:i1 [color="#d36805"]
                node3:o0 -> node14:i0 [color="#0000ff", penwidth="2"]
                node13:o0 -> node14:i1 [color="#0000ff", penwidth="2"]
                node14:o0 -> node15:i0 [color="#0000ff", penwidth="2"]
                node3:o1 -> node4:i0 [color="#0000ff", penwidth="2"]
                node13:o1 -> node4:i1 [color="#0000ff", penwidth="2"]
                node4:o1 -> node5:i0 [color="#4e4e4e"]
                node1:o0 -> node5:i1 [color="#d36805"]
                node11:o0 -> node5:i2 [color="#d36805"]
                node5:o0 -> node11:i0 [color="#d36805"]
                node10:o0 -> node11:i1 [color="#d36805"]
                node11:o0 -> node12:i0 [color="#d36805"]
                node0:o2 -> node12:i1 [color="#d36805"]
            }
        "##]],
    )
}

#[test]
fn dump_quoted_func_name() {
    check_dump_graphviz(
        r#"
        extfunc @memset:ptr(ptr, i32, i64)
        extfunc @memcpy:ptr(ptr, ptr, i64)
        extfunc @gc_new:ptr(i64, i64)
        extfunc @throw(ptr)

        func @"System.Boolean System.ValueType::Equals(System.Object)":i32(ptr, ptr) {
            %0:ctrl, %1:ptr, %2:ptr = entry
            %3:ctrl, %4:phisel = region %0
            %13:i32 = iconst 0
            %26:i64 = iconst 8
            %25:i64 = iconst 106721347376192
            %9:ptr = iconst 0
            %10:i32 = icmp ne %2, %9
            %11:ctrl, %12:ctrl = brcond %3, %10
            %5:ctrl, %6:phisel = region %11
            %7:ctrl, %8:phisel = region %12
            return %7, %13
            %14:ptr = stackslot 0:1
            %15:i64 = iconst 0
            %16:ctrl = call @memcpy %5, %14, %1, %15
            %17:ctrl, %18:ptr = call @"System.Type System.Object::GetType()" %16, %14
            %23:ctrl, %24:ctrl = brcond %17, %10
            %19:ctrl, %20:phisel = region %23
            %27:ctrl, %28:ptr = call @gc_new %19, %25, %26
            %29:ctrl = call @"System.Void System.NullReferenceException::.ctor()" %27, %28
            %30:ctrl = call @throw %29, %28
            %21:ctrl, %22:phisel = region %24
            %31:ctrl, %32:ptr = call @"System.Type System.Object::GetType()" %21, %2
            return %31, %13
        }


        func @"System.Void System.Object::Finalize()"(ptr) {
            %0:ctrl, %1:ptr = entry
            %2:ctrl, %3:phisel = region %0
            return %2
        }

        func @"System.String System.Object::ToString()":ptr(ptr) {
            %0:ctrl, %1:ptr = entry
            %2:ctrl, %3:phisel = region %0
            %4:ctrl, %5:ptr = call @"System.Type System.Object::GetType()" %2, %1
            %6:ctrl, %7:ptr = call @"System.String System.Object::ToString()" %4, %5
            return %6, %7
        }

        func @"System.Type System.Object::GetType()":ptr(ptr) {
            %0:ctrl, %1:ptr = entry
        }

        func @"System.Void System.NullReferenceException::.ctor()"(ptr) {
            %0:ctrl, %1:ptr = entry
        }

        func @"System.Boolean System.Object::Equals(System.Object)":i32(ptr, ptr) {
            %0:ctrl, %1:ptr, %2:ptr = entry
            %3:ctrl, %4:phisel = region %0
            %5:i32 = icmp eq %1, %2
            return %3, %5
        }

        func @"System.Int32 System.Object::GetHashCode()":i32(ptr) {
            %0:ctrl, %1:ptr = entry
            %2:ctrl, %3:phisel = region %0
            %4:i32 = iconst 4294967295
            return %2, %4
        }

        func @"System.Int32 System.ValueType::GetHashCode()":i32(ptr) {
            %0:ctrl, %1:ptr = entry
            %2:ctrl, %3:phisel = region %0
            %4:i32 = iconst 4294967295
            return %2, %4
        }

        func @"System.String System.ValueType::ToString()":ptr(ptr) {
            %0:ctrl, %1:ptr = entry
            %2:ctrl, %3:phisel = region %0
            %4:ptr = stackslot 0:1
            %5:i64 = iconst 0
            %6:ctrl = call @memcpy %2, %4, %1, %5
            %7:ctrl, %8:ptr = call @"System.Type System.Object::GetType()" %6, %4
            %9:ctrl, %10:ptr = call @"System.String System.Object::ToString()" %7, %8
            return %9, %10
        }

        func @"System.Test+Lol System.Test::Do(Lol[])"(ptr, ptr, ptr) {
            %0:ctrl, %1:ptr, %2:ptr, %3:ptr = entry
            %4:ctrl, %5:phisel = region %0
            %19:i64 = iconst 8
            %18:i64 = iconst 106721347374592
            %28:ptr = stackslot 8:4
            %6:i32 = iconst 5
            %25:i64 = imul %19, %6
            %24:i64 = iconst 40
            %26:i64 = iadd %24, %25
            %27:ptr = ptroff %2, %26
            %11:i64 = iconst 32
            %12:ptr = ptroff %2, %11
            %13:ctrl, %14:i32 = load %4, %12
            %15:i32 = icmp slt %6, %14
            %16:ctrl, %17:ctrl = brcond %13, %15
            %7:ctrl, %8:phisel = region %16
            %29:ctrl = call @memcpy %7, %28, %27, %19
            %30:ctrl = call @memcpy %29, %1, %28, %19
            return %30
            %9:ctrl, %10:phisel = region %17
            %20:ctrl, %21:ptr = call @gc_new %9, %18, %19
            %22:ctrl = call @"System.Void System.IndexOutOfRangeException::.ctor()" %20, %21
            %23:ctrl = call @throw %22, %21
        }

        func @"System.Void System.IndexOutOfRangeException::.ctor()"(ptr) {
            %0:ctrl, %1:ptr = entry
        }
        "#,
        expect![[r#"
            digraph {
                node0 [shape=Mrecord, label="{entry | {<o0> ctrl | <o1> ptr | <o2> ptr}}"]
                node1 [shape=Mrecord, label="{{<i0>} | region | {<o0> ctrl | <o1> phisel}}"]
                node2 [shape=Mrecord, label="{iconst 0 | {<o0> i32}}"]
                node3 [shape=Mrecord, label="{iconst 8 | {<o0> i64}}"]
                node4 [shape=Mrecord, label="{iconst 106721347376192 | {<o0> i64}}"]
                node5 [shape=Mrecord, label="{iconst 0 | {<o0> ptr}}"]
                node6 [shape=Mrecord, label="{{<i0> | <i1>} | icmp ne | {<o0> i32}}"]
                node7 [shape=Mrecord, label="{{<i0> | <i1>} | brcond | {<o0> ctrl | <o1> ctrl}}"]
                node8 [shape=Mrecord, label="{{<i0>} | region | {<o0> ctrl | <o1> phisel}}"]
                node9 [shape=Mrecord, label="{{<i0>} | region | {<o0> ctrl | <o1> phisel}}"]
                node10 [shape=Mrecord, label="{{<i0> | <i1>} | return}"]
                node11 [shape=Mrecord, label="{stackslot 0:1 | {<o0> ptr}}"]
                node12 [shape=Mrecord, label="{iconst 0 | {<o0> i64}}"]
                node13 [shape=Mrecord, label="{{<i0> | <i1> | <i2> | <i3>} | call @memcpy | {<o0> ctrl}}"]
                node14 [shape=Mrecord, label="{{<i0> | <i1>} | call @\"System.Type System.Object::GetType()\" | {<o0> ctrl | <o1> ptr}}"]
                node15 [shape=Mrecord, label="{{<i0> | <i1>} | brcond | {<o0> ctrl | <o1> ctrl}}"]
                node16 [shape=Mrecord, label="{{<i0>} | region | {<o0> ctrl | <o1> phisel}}"]
                node17 [shape=Mrecord, label="{{<i0> | <i1> | <i2>} | call @gc_new | {<o0> ctrl | <o1> ptr}}"]
                node18 [shape=Mrecord, label="{{<i0> | <i1>} | call @\"System.Void System.NullReferenceException::.ctor()\" | {<o0> ctrl}}"]
                node19 [shape=Mrecord, label="{{<i0> | <i1>} | call @throw | {<o0> ctrl}}"]
                node20 [shape=Mrecord, label="{{<i0>} | region | {<o0> ctrl | <o1> phisel}}"]
                node21 [shape=Mrecord, label="{{<i0> | <i1>} | call @\"System.Type System.Object::GetType()\" | {<o0> ctrl | <o1> ptr}}"]
                node22 [shape=Mrecord, label="{{<i0> | <i1>} | return}"]
                node0:o0 -> node1:i0
                node0:o2 -> node6:i0
                node5:o0 -> node6:i1
                node1:o0 -> node7:i0
                node6:o0 -> node7:i1
                node7:o0 -> node8:i0
                node7:o1 -> node9:i0
                node9:o0 -> node10:i0
                node2:o0 -> node10:i1
                node8:o0 -> node13:i0
                node11:o0 -> node13:i1
                node0:o1 -> node13:i2
                node12:o0 -> node13:i3
                node13:o0 -> node14:i0
                node11:o0 -> node14:i1
                node14:o0 -> node15:i0
                node6:o0 -> node15:i1
                node15:o0 -> node16:i0
                node16:o0 -> node17:i0
                node4:o0 -> node17:i1
                node3:o0 -> node17:i2
                node17:o0 -> node18:i0
                node17:o1 -> node18:i1
                node18:o0 -> node19:i0
                node17:o1 -> node19:i1
                node15:o1 -> node20:i0
                node20:o0 -> node21:i0
                node0:o2 -> node21:i1
                node21:o0 -> node22:i0
                node2:o0 -> node22:i1
            }
        "#]],
        expect![[r##"
            digraph {
                node0 [shape=Mrecord, label="{entry | {<o0> ctrl | <o1> ptr | <o2> ptr}}", fillcolor="#ffd3e4", style="filled"]
                node1 [shape=Mrecord, label="{{<i0>} | region | {<o0> ctrl | <o1> phisel}}", fillcolor="#c2c2ff", style="filled"]
                node2 [shape=Mrecord, label="{iconst 0 | {<o0> i32}}", fillcolor="#ffee9b", style="filled"]
                node3 [shape=Mrecord, label="{iconst 8 | {<o0> i64}}", fillcolor="#ffee9b", style="filled"]
                node4 [shape=Mrecord, label="{iconst 106721347376192 | {<o0> i64}}", fillcolor="#ffee9b", style="filled"]
                node5 [shape=Mrecord, label="{iconst 0 | {<o0> ptr}}", fillcolor="#ffee9b", style="filled"]
                node6 [shape=Mrecord, label="{{<i0> | <i1>} | icmp ne | {<o0> i32}}", fillcolor="#ffee9b", style="filled"]
                node7 [shape=Mrecord, label="{{<i0> | <i1>} | brcond | {<o0> ctrl | <o1> ctrl}}", fillcolor="#ffd3e4", style="filled"]
                node8 [shape=Mrecord, label="{{<i0>} | region | {<o0> ctrl | <o1> phisel}}", fillcolor="#c2c2ff", style="filled"]
                node9 [shape=Mrecord, label="{{<i0>} | region | {<o0> ctrl | <o1> phisel}}", fillcolor="#c2c2ff", style="filled"]
                node10 [shape=Mrecord, label="{{<i0> | <i1>} | return}", fillcolor="#ffd3e4", style="filled"]
                node11 [shape=Mrecord, label="{stackslot 0:1 | {<o0> ptr}}", fillcolor="#ffee9b", style="filled"]
                node12 [shape=Mrecord, label="{iconst 0 | {<o0> i64}}", fillcolor="#ffee9b", style="filled"]
                node13 [shape=Mrecord, label="{{<i0> | <i1> | <i2> | <i3>} | call @memcpy | {<o0> ctrl}}", fillcolor="#ffd3e4", style="filled"]
                node14 [shape=Mrecord, label="{{<i0> | <i1>} | call @\"System.Type System.Object::GetType()\" | {<o0> ctrl | <o1> ptr}}", fillcolor="#ffd3e4", style="filled"]
                node15 [shape=Mrecord, label="{{<i0> | <i1>} | brcond | {<o0> ctrl | <o1> ctrl}}", fillcolor="#ffd3e4", style="filled"]
                node16 [shape=Mrecord, label="{{<i0>} | region | {<o0> ctrl | <o1> phisel}}", fillcolor="#c2c2ff", style="filled"]
                node17 [shape=Mrecord, label="{{<i0> | <i1> | <i2>} | call @gc_new | {<o0> ctrl | <o1> ptr}}", fillcolor="#ffd3e4", style="filled"]
                node18 [shape=Mrecord, label="{{<i0> | <i1>} | call @\"System.Void System.NullReferenceException::.ctor()\" | {<o0> ctrl}}", fillcolor="#ffd3e4", style="filled"]
                node19 [shape=Mrecord, label="{{<i0> | <i1>} | call @throw | {<o0> ctrl}}", fillcolor="#ffd3e4", style="filled"]
                node20 [shape=Mrecord, label="{{<i0>} | region | {<o0> ctrl | <o1> phisel}}", fillcolor="#c2c2ff", style="filled"]
                node21 [shape=Mrecord, label="{{<i0> | <i1>} | call @\"System.Type System.Object::GetType()\" | {<o0> ctrl | <o1> ptr}}", fillcolor="#ffd3e4", style="filled"]
                node22 [shape=Mrecord, label="{{<i0> | <i1>} | return}", fillcolor="#ffd3e4", style="filled"]
                node0:o0 -> node1:i0 [color="#0000ff", penwidth="2"]
                node0:o2 -> node6:i0 [color="#d36805"]
                node5:o0 -> node6:i1 [color="#d36805"]
                node1:o0 -> node7:i0 [color="#0000ff", penwidth="2"]
                node6:o0 -> node7:i1 [color="#d36805"]
                node7:o0 -> node8:i0 [color="#0000ff", penwidth="2"]
                node7:o1 -> node9:i0 [color="#0000ff", penwidth="2"]
                node9:o0 -> node10:i0 [color="#0000ff", penwidth="2"]
                node2:o0 -> node10:i1 [color="#d36805"]
                node8:o0 -> node13:i0 [color="#0000ff", penwidth="2"]
                node11:o0 -> node13:i1 [color="#d36805"]
                node0:o1 -> node13:i2 [color="#d36805"]
                node12:o0 -> node13:i3 [color="#d36805"]
                node13:o0 -> node14:i0 [color="#0000ff", penwidth="2"]
                node11:o0 -> node14:i1 [color="#d36805"]
                node14:o0 -> node15:i0 [color="#0000ff", penwidth="2"]
                node6:o0 -> node15:i1 [color="#d36805"]
                node15:o0 -> node16:i0 [color="#0000ff", penwidth="2"]
                node16:o0 -> node17:i0 [color="#0000ff", penwidth="2"]
                node4:o0 -> node17:i1 [color="#d36805"]
                node3:o0 -> node17:i2 [color="#d36805"]
                node17:o0 -> node18:i0 [color="#0000ff", penwidth="2"]
                node17:o1 -> node18:i1 [color="#d36805"]
                node18:o0 -> node19:i0 [color="#0000ff", penwidth="2"]
                node17:o1 -> node19:i1 [color="#d36805"]
                node15:o1 -> node20:i0 [color="#0000ff", penwidth="2"]
                node20:o0 -> node21:i0 [color="#0000ff", penwidth="2"]
                node0:o2 -> node21:i1 [color="#d36805"]
                node21:o0 -> node22:i0 [color="#0000ff", penwidth="2"]
                node2:o0 -> node22:i1 [color="#d36805"]
            }
        "##]],
    );
}

#[test]
fn dump_verifier_errors() {
    check_dump_graphviz_verifier_errors(
        r#"
        func @sum:i32(i32) {
            %0:ctrl, %1:i32 = entry
            %2:ctrl, %3:phisel = region %0
            %5:ptr = stackslot 4:4
            %4:ptr = stackslot 4:4
            %6:ctrl = store %2, %1, %4, %4
            %24:i32 = iconst 1
            %7:i32 = iconst 0
            %8:ctrl = store %6, %7, %5
            %13:i32 = icmp eq %12, %7
            %18:ctrl, %19:ctrl = brcond %11, %13
            %14:ctrl, %15:phisel = region %18
            %29:ctrl, %30:i32 = load %6, %5
            return %29, %30
            %16:ctrl, %17:phisel = region %19
            %20:ctrl, %21:i32 = load %16, %4
            %22:ctrl, %23:i32 = load %20, %5
            %25:i32 = isub %21, %5
            %27:ctrl = store %22, %25, %4
            %26:i32 = iadd %23, %21
            %28:ctrl = store %27, %26, %5
            %9:ctrl, %10:phisel = region %8, %28
            %11:ctrl, %12:i32 = load %9, %4
        }
        "#,
        expect![[r##"
            digraph {
                node0 [shape=Mrecord, label="{entry | {<o0> ctrl | <o1> i32}}", fillcolor="#ffd3e4", style="filled"]
                node1 [shape=Mrecord, label="{{<i0>} | region | {<o0> ctrl | <o1> phisel}}", fillcolor="#c2c2ff", style="filled"]
                node2 [shape=Mrecord, label="{stackslot 4:4 | {<o0> ptr}}", fillcolor="#ffee9b", style="filled"]
                node3 [shape=Mrecord, label="{stackslot 4:4 | {<o0> ptr}}", fillcolor="#ffee9b", style="filled"]
                node4 [shape=Mrecord, label="{{<i0> | <i1> | <i2> | <i3>} | store | {<o0> ctrl}}", tooltip="bad input count, expected 3&#10;control output 0 reused", color="#ff0000", penwidth="2", fillcolor="#ffd3e4", style="filled"]
                node11 [shape=Mrecord, label="{{<i0> | <i1>} | load | {<o0> ctrl | <o1> i32}}", fillcolor="#ffd3e4", style="filled"]
                node12 [shape=Mrecord, label="{{<i0> | <i1>} | return}", fillcolor="#ffd3e4", style="filled"]
                node6 [shape=Mrecord, label="{iconst 0 | {<o0> i32}}", fillcolor="#ffee9b", style="filled"]
                node7 [shape=Mrecord, label="{{<i0> | <i1> | <i2>} | store | {<o0> ctrl}}", fillcolor="#ffd3e4", style="filled"]
                node8 [shape=Mrecord, label="{{<i0> | <i1>} | icmp eq | {<o0> i32}}", fillcolor="#ffee9b", style="filled"]
                node9 [shape=Mrecord, label="{{<i0> | <i1>} | brcond | {<o0> ctrl | <o1> ctrl}}", fillcolor="#ffd3e4", style="filled"]
                node10 [shape=Mrecord, label="{{<i0>} | region | {<o0> ctrl | <o1> phisel}}", tooltip="control output 0 unused", color="#ff0000", penwidth="2", fillcolor="#c2c2ff", style="filled"]
                node13 [shape=Mrecord, label="{{<i0>} | region | {<o0> ctrl | <o1> phisel}}", fillcolor="#c2c2ff", style="filled"]
                node14 [shape=Mrecord, label="{{<i0> | <i1>} | load | {<o0> ctrl | <o1> i32}}", fillcolor="#ffd3e4", style="filled"]
                node15 [shape=Mrecord, label="{{<i0> | <i1>} | load | {<o0> ctrl | <o1> i32}}", fillcolor="#ffd3e4", style="filled"]
                node16 [shape=Mrecord, label="{{<i0> | <i1>} | isub | {<o0> i32}}", fillcolor="#ffee9b", style="filled"]
                node17 [shape=Mrecord, label="{{<i0> | <i1> | <i2>} | store | {<o0> ctrl}}", fillcolor="#ffd3e4", style="filled"]
                node18 [shape=Mrecord, label="{{<i0> | <i1>} | iadd | {<o0> i32}}", fillcolor="#ffee9b", style="filled"]
                node19 [shape=Mrecord, label="{{<i0> | <i1> | <i2>} | store | {<o0> ctrl}}", fillcolor="#ffd3e4", style="filled"]
                node20 [shape=Mrecord, label="{{<i0> | <i1>} | region | {<o0> ctrl | <o1> phisel}}", fillcolor="#c2c2ff", style="filled"]
                node21 [shape=Mrecord, label="{{<i0> | <i1>} | load | {<o0> ctrl | <o1> i32}}", fillcolor="#ffd3e4", style="filled"]
                node0:o0 -> node1:i0 [color="#0000ff", penwidth="2"]
                node1:o0 -> node4:i0 [color="#0000ff", penwidth="2"]
                node0:o1 -> node4:i1 [color="#d36805"]
                node3:o0 -> node4:i2 [color="#d36805"]
                node3:o0 -> node4:i3 [color="#d36805"]
                node4:o0 -> node11:i0 [color="#0000ff", penwidth="2"]
                node2:o0 -> node11:i1 [color="#d36805"]
                node11:o0 -> node12:i0 [color="#0000ff", penwidth="2"]
                node11:o1 -> node12:i1 [color="#d36805"]
                node4:o0 -> node7:i0 [color="#0000ff", penwidth="2"]
                node6:o0 -> node7:i1 [color="#d36805"]
                node2:o0 -> node7:i2 [color="#d36805"]
                node21:o1 -> node8:i0 [color="#d36805"]
                node6:o0 -> node8:i1 [color="#d36805"]
                node21:o0 -> node9:i0 [color="#0000ff", penwidth="2"]
                node8:o0 -> node9:i1 [color="#d36805"]
                node9:o0 -> node10:i0 [color="#0000ff", penwidth="2"]
                node9:o1 -> node13:i0 [color="#0000ff", penwidth="2"]
                node13:o0 -> node14:i0 [color="#0000ff", penwidth="2"]
                node3:o0 -> node14:i1 [color="#d36805"]
                node14:o0 -> node15:i0 [color="#0000ff", penwidth="2"]
                node2:o0 -> node15:i1 [color="#d36805"]
                node14:o1 -> node16:i0 [color="#d36805"]
                node2:o0 -> node16:i1 [tooltip="bad value kind for input 1, expected one of `i32`, got `ptr`", color="#ff0000", penwidth="2"]
                node15:o0 -> node17:i0 [color="#0000ff", penwidth="2"]
                node16:o0 -> node17:i1 [color="#d36805"]
                node3:o0 -> node17:i2 [color="#d36805"]
                node15:o1 -> node18:i0 [color="#d36805"]
                node14:o1 -> node18:i1 [color="#d36805"]
                node17:o0 -> node19:i0 [color="#0000ff", penwidth="2"]
                node18:o0 -> node19:i1 [color="#d36805"]
                node2:o0 -> node19:i2 [color="#d36805"]
                node7:o0 -> node20:i0 [color="#0000ff", penwidth="2"]
                node19:o0 -> node20:i1 [color="#0000ff", penwidth="2"]
                node20:o0 -> node21:i0 [color="#0000ff", penwidth="2"]
                node3:o0 -> node21:i1 [color="#d36805"]
            }
        "##]],
    );
}

use anyhow::Result;
use fx_utils::{FxHashMap, FxHashSet};
use ir::{
    module::{ExternFunctionData, FunctionData, Module},
    node::{FunctionRef, NodeKind},
    valwalk::walk_live_nodes,
};

use crate::utils::function_by_name;

pub fn extract_function(module: &Module, function_name: &str) -> Result<Module> {
    let (func, func_data) = function_by_name(module, function_name)?;

    let referenced_functions = collect_referenced_functions(func_data);

    let mut new_module = Module::new();
    let mut function_map = FxHashMap::default();

    for &funcref in &referenced_functions {
        if funcref == FunctionRef::Internal(func) {
            // If the function is recursive, don't insert a new extfunc for the recursive calls.
            continue;
        }

        let metadata = module.resolve_funcref(funcref);
        let new_funcref = new_module.extern_functions.push(ExternFunctionData {
            name: metadata.name.to_owned(),
            sig: metadata.sig.clone(),
        });
        function_map.insert(funcref, new_funcref);
    }

    let new_func_data = func_data.clone();
    let new_func = new_module.functions.push(new_func_data);
    let new_func_data = &mut new_module.functions[new_func];
    let live_nodes: Vec<_> = walk_live_nodes(&new_func_data.graph, new_func_data.entry).collect();

    for &node in &live_nodes {
        if let NodeKind::Call(funcref) = new_func_data.graph.node_kind_mut(node) {
            if *funcref == FunctionRef::Internal(func) {
                // Recursive calls get translated into the new module's copy of the function.
                *funcref = FunctionRef::Internal(new_func);
            } else {
                // Everything else gets externalized.
                let new_extfunc = function_map[funcref];
                *funcref = FunctionRef::External(new_extfunc);
            }
        }
    }

    Ok(new_module)
}

fn collect_referenced_functions(func_data: &FunctionData) -> FxHashSet<FunctionRef> {
    let mut functions = FxHashSet::default();

    for node in walk_live_nodes(&func_data.graph, func_data.entry) {
        if let &NodeKind::Call(funcref) = func_data.graph.node_kind(node) {
            functions.insert(funcref);
        }
    }

    functions
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};
    use parser::parse_module;

    use super::*;

    fn check_extract_function(module_str: &str, function_name: &str, expected: Expect) {
        let module = parse_module(module_str).expect("failed to parse module");
        let extracted = extract_function(&module, function_name).expect("extraction failed");
        expected.assert_eq(&extracted.to_string());
    }

    #[test]
    fn extract_simple() {
        check_extract_function(
            "
                func @a:i32() {
                    %c:ctrl = entry
                    %i:i32 = iconst 5
                    return %c, %i
                }

                func @b:i32() {
                    %c:ctrl = entry
                    %i:i32 = iconst 7
                    return %c, %i
                }
            ",
            "a",
            expect![[r#"

                func @a:i32() {
                    %0:ctrl = entry
                    %1:i32 = iconst 5
                    return %0, %1
                }
            "#]],
        );
    }

    #[test]
    fn extract_with_refs() {
        check_extract_function(
            "
                extfunc @inc:i32(i32)

                func @add:i32(i32, i32) {
                    %c:ctrl, %a:i32, %b:i32 = entry
                    %sum:i32 = iadd %a, %b
                    return %c, %sum
                }

                func @fib:i32(i32) {
                    %ent:ctrl, %n:i32 = entry
                    %zero:i32 = iconst 0
                    %one:i32 = iconst 1
                    %header:ctrl, %hphi:phisel = region %ent, %body2
                    %i:i32 = phi %hphi, %zero, %next_i
                    %prev:i32 = phi %hphi, %zero, %cur
                    %cur:i32 = phi %hphi, %one, %next
                    %is_done:i32 = icmp eq %i, %n
                    %exit:ctrl, %body0:ctrl = brcond %header, %is_done
                    %body1:ctrl, %next:i32 = call @add %body0, %prev, %cur
                    %body2:ctrl, %next_i:i32 = call @inc %body1, %i
                    return %exit, %prev
                }
            ",
            "fib",
            expect![[r#"
                extfunc @add:i32(i32, i32)
                extfunc @inc:i32(i32)

                func @fib:i32(i32) {
                    %0:ctrl, %1:i32 = entry
                    %2:i32 = iconst 0
                    %3:i32 = iconst 1
                    %8:i32 = phi %5, %3, %13
                    %7:i32 = phi %5, %2, %8
                    %12:ctrl, %13:i32 = call @add %11, %7, %8
                    %14:ctrl, %15:i32 = call @inc %12, %6
                    %4:ctrl, %5:phisel = region %0, %14
                    %6:i32 = phi %5, %2, %15
                    %9:i32 = icmp eq %6, %1
                    %10:ctrl, %11:ctrl = brcond %4, %9
                    return %10, %7
                }
            "#]],
        );
    }

    #[test]
    fn extract_with_dead_refs() {
        check_extract_function(
            "
                extfunc @inc:i32(i32)

                func @add:i32(i32, i32) {
                    %c:ctrl, %a:i32, %b:i32 = entry
                    %sum:i32 = iadd %a, %b
                    return %c, %sum
                }

                func @dead_call:i32() {
                    %c:ctrl = entry
                    %i:i32 = iconst 1
                    return %c, %i
                }

                func @fib:i32(i32) {
                    %ent:ctrl, %n:i32 = entry
                    %zero:i32 = iconst 0
                    %one:i32 = iconst 1
                    %header:ctrl, %hphi:phisel = region %ent, %body2
                    %i:i32 = phi %hphi, %zero, %next_i
                    %prev:i32 = phi %hphi, %zero, %cur
                    %cur:i32 = phi %hphi, %one, %next
                    %is_done:i32 = icmp eq %i, %n
                    %exit:ctrl, %body0:ctrl = brcond %header, %is_done
                    %body1:ctrl, %next:i32 = call @add %body0, %prev, %cur
                    %body2:ctrl, %next_i:i32 = call @inc %body1, %i
                    return %exit, %prev
                    %dead:ctrl, %dphi:phisel = region
                    %dead_exit:ctrl, %val:i32 = call @dead_call %dead
                    return %dead_exit, %val
                }
            ",
            "fib",
            expect![[r#"
                extfunc @add:i32(i32, i32)
                extfunc @inc:i32(i32)

                func @fib:i32(i32) {
                    %0:ctrl, %1:i32 = entry
                    %2:i32 = iconst 0
                    %3:i32 = iconst 1
                    %8:i32 = phi %5, %3, %13
                    %7:i32 = phi %5, %2, %8
                    %12:ctrl, %13:i32 = call @add %11, %7, %8
                    %14:ctrl, %15:i32 = call @inc %12, %6
                    %4:ctrl, %5:phisel = region %0, %14
                    %6:i32 = phi %5, %2, %15
                    %9:i32 = icmp eq %6, %1
                    %10:ctrl, %11:ctrl = brcond %4, %9
                    return %10, %7
                }
            "#]],
        );
    }

    #[test]
    fn extract_recursive() {
        check_extract_function(
            "
                extfunc @dec:i32(i32)
                extfunc @dec2:i32(i32)

                func @add:i32(i32, i32) {
                    %c:ctrl, %a:i32, %b:i32 = entry
                    %sum:i32 = iadd %a, %b
                    return %c, %sum
                }

                func @fib:i32(i32) {
                    %ent:ctrl, %n:i32 = entry
                    %zero:i32 = iconst 0
                    %one:i32 = iconst 1

                    %is_zero:i32 = icmp eq %n, %zero
                    %exit_zero:ctrl, %nonzero:ctrl = brcond %ent, %is_zero
                    return %exit_zero, %zero

                    %is_one:i32 = icmp eq %n, %one
                    %exit_one:ctrl, %do_rec:ctrl = brcond %nonzero, %is_one
                    return %exit_one, %one

                    %rec0:ctrl, %n_2:i32 = call @dec2 %do_rec, %n
                    %rec1:ctrl, %f_n_2:i32 = call @fib %rec0, %n_2

                    %rec2:ctrl, %n_1:i32 = call @dec %rec1, %n
                    %rec3:ctrl, %f_n_1:i32 = call @fib %rec2, %n_1

                    %exit:ctrl, %result:i32 = call @add %rec3, %f_n_2, %f_n_1
                    return %exit, %result
                }
            ",
            "fib",
            expect![[r#"
                extfunc @add:i32(i32, i32)
                extfunc @dec:i32(i32)
                extfunc @dec2:i32(i32)

                func @fib:i32(i32) {
                    %0:ctrl, %1:i32 = entry
                    %3:i32 = iconst 1
                    %7:i32 = icmp eq %1, %3
                    %2:i32 = iconst 0
                    %4:i32 = icmp eq %1, %2
                    %5:ctrl, %6:ctrl = brcond %0, %4
                    return %5, %2
                    %8:ctrl, %9:ctrl = brcond %6, %7
                    return %8, %3
                    %10:ctrl, %11:i32 = call @dec2 %9, %1
                    %12:ctrl, %13:i32 = call @fib %10, %11
                    %14:ctrl, %15:i32 = call @dec %12, %1
                    %16:ctrl, %17:i32 = call @fib %14, %15
                    %18:ctrl, %19:i32 = call @add %16, %13, %17
                    return %18, %19
                }
            "#]],
        );
    }
}

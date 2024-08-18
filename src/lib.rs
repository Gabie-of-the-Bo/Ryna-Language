use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

extern crate nom;

#[macro_use]
extern crate lazy_static;

pub mod cache;
pub mod patterns;
pub mod object;
pub mod operations;
pub mod functions;
pub mod types;
pub mod interfaces;
pub mod annotations;
pub mod context;
pub mod docs;

pub mod debug;
pub mod parser;
pub mod macros;
pub mod inference;
pub mod checks;
pub mod compilation;
pub mod optimization;
pub mod execution;
pub mod translation;
pub mod serialization;

pub mod config;

#[path = "algorithms/regex_ext.rs"]
pub mod regex_ext;

#[path = "algorithms/html_ext.rs"]
pub mod html_ext;

#[path = "algorithms/integer_ext.rs"]
pub mod integer_ext;

#[path = "algorithms/git.rs"]
pub mod git;

#[path = "algorithms/profiling.rs"]
pub mod profiling;

#[path = "algorithms/formats.rs"]
pub mod formats;

#[path = "structures/graph.rs"]
pub mod graph;

#[path = "structures/variable_map.rs"]
pub mod variable_map;

#[path = "structures/id_mapper.rs"]
pub mod id_mapper;

#[path = "structures/precedence_cache.rs"]
pub mod precedence_cache;

#[path = "structures/mut_cell.rs"]
pub mod mut_cell;

#[cfg(test)]
mod integration {
    use std::fs::read_to_string;
    use crate::compilation::RynaError;
    use crate::context::standard_ctx;
    use crate::config::{precompile_ryna_module_with_config, compute_project_hash};
    use glob::glob;

    fn integration_test(file_path: &str) {
        let file = read_to_string(file_path).expect("Unable to locate file");
        let mut ctx = standard_ctx();
        ctx.optimize = true;

        if let Err(err) = ctx.parse_and_execute_ryna_module(&file) {
            err.emit();
        }
    }

    fn integration_test_batch(glob_path: &str) {
        for file_path in glob(glob_path).expect("Invalid glob") {
            let file = read_to_string(file_path.unwrap()).expect("Unable to locate file");

            if file.starts_with("// ") {
                // Negative test
                let expected_msg = &file.lines().next().unwrap()[3..];

                let result = std::panic::catch_unwind(|| {
                    let mut ctx = standard_ctx();
                    ctx.optimize = true;

                    ctx.parse_and_execute_ryna_module(&file)
        
                }).unwrap_or_else(|err| {
                    Err(RynaError::execution_error(panic_message::panic_message(&err).to_owned()))
                });
        
                if let Err(err) = result {
                    let exp_chars = expected_msg.chars().collect::<Vec<_>>();
                    let err_chars = err.message.chars().collect::<Vec<_>>();
                    let mut exp_idx = 0;
                    let mut err_idx = 0;
        
                    while exp_idx < exp_chars.len() && err_idx < err_chars.len() {
                        exp_idx += (err_chars[err_idx] == exp_chars[exp_idx]) as usize;
                        err_idx += 1;
                    }
        
                    if exp_idx != exp_chars.len() {
                        panic!("Error message was different from expected:\n - Expected: {}\n - Got: {}", expected_msg, err.message);
                    }
        
                } else {
                    panic!("Test did not fail!");
                }
        
            } else {
                // Positive test
                let mut ctx = standard_ctx();

                if let Err(err) = ctx.parse_and_execute_ryna_module(&file) {
                    err.emit();
                }        
            }
        }
    }

    fn module_test(module_path: &str) {
        let path_str = &module_path.to_string();
        let (_, all_mods, files) = compute_project_hash(path_str, None, true, false).unwrap();
        let err = precompile_ryna_module_with_config(path_str, all_mods, files, true, false, true);

        if let Err(err) = &err {
            err.emit();
        }        

        let (mut ctx, lines) = err.unwrap();

        match ctx.compiled_form(&lines) {
            Ok(mut code) => {
                ctx.optimize_instructions(&mut code);

                for (idx, i) in code.iter().enumerate() {
                    println!("{:<3} {}", idx, i.to_string(&ctx));
                }

                if let Err(err) = ctx.execute_compiled_code::<false>(&code.into_iter().map(|i| i.instruction).collect::<Vec<_>>(), &[]) {
                    err.emit();
                }
            },

            Err(err) => err.emit()
        };
    }

    #[test]
    fn naive_primality() {
        integration_test("test/primality.ryna");
    }

    #[test]
    fn mapped_iterator() {
        integration_test("test/mapped_iterator.ryna");
    }

    #[test]
    fn dice() {
        integration_test("test/dice.ryna");
    }

    #[test]
    fn ints_custom_syntax() {
        integration_test("test/ints.ryna");
    }

    #[test]
    fn random() {
        integration_test("test/random.ryna");
    }

    #[test]
    fn tuples() {
        integration_test("test/tuples.ryna");
    }

    #[test]
    fn array_access() {
        integration_test("test/array_access.ryna");
    }

    #[test]
    fn array_init() {
        integration_test("test/array_init.ryna");
    }

    #[test]
    fn list_comprehension() {
        integration_test("test/list_comprehension.ryna");
    }

    #[test]
    fn map_array() {
        integration_test("test/map_array.ryna");
    }

    #[test]
    fn array_transform() {
        integration_test("test/array_transform.ryna");
    }

    #[test]
    fn e_approximation() {
        integration_test("test/e_approximation.ryna");
    }

    #[test]
    fn basic_alias() {
        integration_test("test/basic_alias.ryna");
    }

    #[test]
    fn adt_list() {
        integration_test("test/adt_list.ryna");
    }

    #[test]
    fn adt_generic_list() {
        integration_test("test/adt_generic_list.ryna");
    }

    #[test]
    fn adt_bin_tree() {
        integration_test("test/adt_bin_tree.ryna");
    }

    #[test]
    fn numeric_interface() {
        integration_test("test/numeric_interface.ryna");
    }

    #[test]
    fn parametric_interface() {
        integration_test("test/parametric_interface.ryna");
    }

    #[test]
    fn peano_arithmetic() {
        integration_test("test/peano_arithmetic.ryna");
    }

    #[test]
    fn short_circuit() {
        integration_test("test/short_circuit.ryna");
    }

    #[test]
    fn file_manip() {
        integration_test("test/file_manip.ryna");
    }

    #[test]
    fn string_manip() {
        integration_test("test/string_manip.ryna");
    }

    #[test]
    fn ambiguous_impl() {
        integration_test("test/ambiguous_impl.ryna");
    }

    #[test]
    fn ternary() {
        integration_test("test/ternary.ryna");
    }

    #[test]
    fn adt_assignment() {
        integration_test("test/adt_assignment.ryna");
    }

    #[test]
    fn bitwise() {
        integration_test("test/bitwise.ryna");
    }

    #[test]
    fn rdl_macros() {
        integration_test("test/rdl_macros.ryna");
    }

    #[test]
    fn do_blocks() {
        integration_test("test/do_blocks.ryna");
    }

    #[test]
    fn break_loops() {
        integration_test("test/break_loops.ryna");
    }

    #[test]
    fn continue_loops() {
        integration_test("test/continue_loops.ryna");
    }

    #[test]
    fn lambda_capture() {
        integration_test("test/lambda_capture.ryna");
    }

    #[test]
    fn unicode() {
        integration_test("test/unicode.ryna");
    }

    #[test]
    fn implicit_lambda() {
        integration_test("test/implicit_lambda.ryna");
    }

    #[test]
    fn exp_floats() {
        integration_test("test/exp_floats.ryna");
    }

    #[test]
    fn moving() {
        integration_test_batch("test/batches/moving/*.ryna");
    }

    #[test]
    fn interfaces() {
        integration_test_batch("test/batches/interfaces/*.ryna");
    }

    #[test]
    fn macros() {
        integration_test_batch("test/batches/macros/*.ryna");
    }

    #[test]
    fn stack() {
        integration_test_batch("test/batches/stack/*.ryna");
    }

    #[test]
    fn sum() {
        module_test("test/modules/sum");
    }

    #[test]
    fn prime_check() {
        module_test("test/modules/prime_check");
    }

    #[test]
    fn prime_streaming() {
        module_test("test/modules/prime_streaming");
    }

    #[test]
    fn props_test() {
        module_test("test/modules/props_test");
    }

    #[test]
    fn math_ops() {
        module_test("test/modules/math_ops");
    }

    #[test]
    fn syntax_test() {
        module_test("test/modules/syntax_test");
    }

    #[test]
    fn hash_extensions() {
        module_test("test/modules/hash_extensions");
    }

    #[test]
    fn hash_structs_test() {
        module_test("test/modules/hash_structs_test");
    }

    #[test]
    fn json_test() {
        module_test("test/modules/json_test");
    }

    #[test]
    fn set_syntax_test() {
        module_test("test/modules/set_syntax_test");
    }

    #[test]
    fn array_algorithms() {
        module_test("test/modules/array_algorithms");
    }

    #[test]
    fn iterators_test() {
        module_test("test/modules/iterators_test");
    }

    #[test]
    fn macro_code_ex() {
        module_test("test/modules/macro_code_ex");
    }

    #[test]
    fn bf_embed() {
        module_test("test/modules/bf_embed");
    }

    #[test]
    fn match_test() {
        module_test("test/modules/match_test");
    }
}
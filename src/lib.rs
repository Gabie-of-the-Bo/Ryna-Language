#[macro_use] extern crate impl_ops;
extern crate nom;

pub mod number;
pub mod cache;
pub mod patterns;
pub mod object;
pub mod operations;
pub mod functions;
pub mod types;
pub mod interfaces;
pub mod context;

pub mod debug;
pub mod parser;
pub mod macros;
pub mod inference;
pub mod checks;
pub mod compilation;
pub mod execution;
pub mod translation;
pub mod serialization;

pub mod config;

#[path = "stdlib/math.rs"]
pub mod math;

#[path = "structures/graph.rs"]
pub mod graph;

#[cfg(test)]
mod integration {
    use std::fs::read_to_string;
    use crate::context::standard_ctx;
    use crate::config::{precompile_nessa_module_with_config, compute_project_hash};

    fn integration_test(file_path: &str) {
        let file = read_to_string(file_path).expect("Unable to locate file");
        let mut ctx = standard_ctx();

        if let Err(err) = ctx.parse_and_execute_nessa_module(&file) {
            err.emit();
        }
    }

    fn module_test(module_path: &str) {
        let path_str = &module_path.to_string();
        let (_, all_mods, files) = compute_project_hash(path_str).unwrap();
        let err = precompile_nessa_module_with_config(path_str, all_mods, files);

        if let Err(err) = &err {
            err.emit();
        }        

        let (mut ctx, lines) = err.unwrap();

        match ctx.compiled_form(&lines) {
            Ok(code) => {
                if let Err(err) = ctx.execute_compiled_code::<false>(&code.into_iter().map(|i| i.instruction).collect::<Vec<_>>()) {
                    err.emit();
                }
            },

            Err(err) => err.emit()
        };
    }

    #[test]
    fn naive_primality() {
        integration_test("test/primality.nessa");
    }

    #[test]
    fn mapped_iterator() {
        integration_test("test/mapped_iterator.nessa");
    }

    #[test]
    fn dice() {
        integration_test("test/dice.nessa");
    }

    #[test]
    fn ints_custom_syntax() {
        integration_test("test/ints.nessa");
    }

    #[test]
    fn random() {
        integration_test("test/random.nessa");
    }

    #[test]
    fn tuples() {
        integration_test("test/tuples.nessa");
    }

    #[test]
    fn array_access() {
        integration_test("test/array_access.nessa");
    }

    #[test]
    fn array_init() {
        integration_test("test/array_init.nessa");
    }

    #[test]
    fn list_comprehension() {
        integration_test("test/list_comprehension.nessa");
    }

    #[test]
    fn map_array() {
        integration_test("test/map_array.nessa");
    }

    #[test]
    fn array_transform() {
        integration_test("test/array_transform.nessa");
    }

    #[test]
    fn e_approximation() {
        integration_test("test/e_approximation.nessa");
    }

    #[test]
    fn basic_alias() {
        integration_test("test/basic_alias.nessa");
    }

    #[test]
    fn adt_list() {
        integration_test("test/adt_list.nessa");
    }

    #[test]
    fn adt_generic_list() {
        integration_test("test/adt_generic_list.nessa");
    }

    #[test]
    fn adt_bin_tree() {
        integration_test("test/adt_bin_tree.nessa");
    }

    #[test]
    fn numeric_interface() {
        integration_test("test/numeric_interface.nessa");
    }

    #[test]
    fn parametric_interface() {
        integration_test("test/parametric_interface.nessa");
    }

    #[test]
    fn peano_arithmetic() {
        integration_test("test/peano_arithmetic.nessa");
    }

    #[test]
    fn short_circuit() {
        integration_test("test/short_circuit.nessa");
    }

    #[test]
    fn file_manip() {
        integration_test("test/file_manip.nessa");
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
}
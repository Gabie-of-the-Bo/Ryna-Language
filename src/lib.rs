#[macro_use] extern crate lazy_static;
#[macro_use] extern crate impl_ops;
extern crate nom;

pub mod number;
pub mod patterns;
pub mod object;
pub mod operations;
pub mod functions;
pub mod types;
pub mod context;

pub mod parser;
pub mod inference;
pub mod checks;
pub mod compilation;
pub mod execution;

pub mod config;

#[path = "stdlib/math.rs"]
pub mod math;

#[path = "structures/graph.rs"]
pub mod graph;

#[cfg(test)]
mod integration {
    use std::fs::read_to_string;
    use crate::context::standard_ctx;
    use crate::config::precompile_nessa_module_with_config;

    fn integration_test(file_path: &str) {
        let mut file = read_to_string(file_path).expect("Unable to locate file");
        file.push('\n');

        let mut ctx = standard_ctx();

        let result = ctx.parse_and_execute_nessa_module(&file);

        result.unwrap();
    }

    fn module_test(module_path: &str) {
        let (mut ctx, lines) = precompile_nessa_module_with_config(&module_path.to_string()).unwrap();

        let program = ctx.compiled_form(&lines).unwrap();

        for (idx, i) in program.iter().enumerate() {
            println!("{:<3} {}", idx, i.to_string());
        }

        ctx.execute_compiled_code(&program.into_iter().map(|i| i.instruction).collect()).unwrap();
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
    fn map_array() {
        integration_test("test/map_array.nessa");
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
}
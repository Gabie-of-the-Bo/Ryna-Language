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

#[cfg(test)]
mod integration {
    use std::fs::read_to_string;
    use crate::context::standard_ctx;

    fn integration_text(file_path: &str) {
        let mut file = read_to_string(file_path).expect("Unable to locate file");
        file.push('\n');

        let mut ctx = standard_ctx();

        let result = ctx.parse_and_execute_nessa_module(&file);

        result.unwrap();
    }

    #[test]
    fn naive_primality() {
        integration_text("test/primality.nessa");
    }

    #[test]
    fn mapped_iterator() {
        integration_text("test/mapped_iterator.nessa");
    }

    #[test]
    fn dice() {
        integration_text("test/dice.nessa");
    }
}
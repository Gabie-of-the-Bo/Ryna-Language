#[macro_use] extern crate lazy_static;
#[macro_use] extern crate impl_ops;
extern crate nom;

mod number;
mod patterns;
mod object;
mod operations;
mod functions;
mod variables;
mod types;
mod context;

mod parser;
mod compilation;
mod execution;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate impl_ops;
extern crate nom;

mod number;
mod patterns;
mod object;
mod operations;
mod functions;
mod types;
mod context;

mod parser;
mod inference;
mod compilation;
mod execution;
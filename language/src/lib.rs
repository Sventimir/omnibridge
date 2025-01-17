#[cfg(test)]
#[macro_use]
extern crate proptest;

pub mod ast;

#[cfg(not(test))]
mod compiler;
#[cfg(test)]
pub mod compiler;

mod core_lisp;
pub mod env;

mod expr;
mod full_lisp;
mod instr;
pub mod parser;
pub mod program;
mod sexp;
pub mod src_location;
pub mod typed;
mod var;

pub use compiler::compile;
pub use core_lisp::CoreLisp;
pub use expr::Expr;
pub use full_lisp::Lisp;
pub use parser::parse;
pub use sexp::{int, nil, pair, IntoSexp, Sexp, SexpError};

pub mod interpreter;

#[cfg(test)]
mod test;
pub mod test_utils;

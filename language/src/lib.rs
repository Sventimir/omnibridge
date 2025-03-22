#[cfg(test)]
#[macro_use]
extern crate proptest;

pub mod ast;

#[cfg(not(test))]
mod compiler;
#[cfg(test)]
pub mod compiler;

pub mod builtin_instr;
pub mod builtin_type;
pub mod constraint;
pub mod env;
pub mod interpreter;
pub mod parser;
pub mod src_location;
pub mod type_error;

mod core_lisp;
mod expr;
mod full_lisp;
mod sexp;
mod type_checker;
mod type_var;

pub use compiler::compile;
pub use core_lisp::CoreLisp;
pub use expr::Expr;
pub use full_lisp::Lisp;
pub use parser::parse;
pub use sexp::{int, nil, pair, IntoSexp, Sexp, SexpError};

#[cfg(test)]
mod test;
pub mod test_utils;

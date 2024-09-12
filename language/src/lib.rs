#[cfg(test)]
#[macro_use]
extern crate quickcheck;

pub mod ast;
mod compiler;
mod core_lisp;
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
pub use full_lisp::Lisp;
pub use parser::parse;
pub use sexp::{int, nil, pair, IntoSexp, Sexp, SexpError};

#[cfg(test)]
mod test;

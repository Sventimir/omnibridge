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
mod source_meta;
mod type_checker;
mod type_var;

pub use builtin_instr::BuiltinInstr;
pub use builtin_type::BuiltinType;
pub use compiler::{CompilationError, compile};
pub use core_lisp::CoreLisp;
pub use expr::Expr;
pub use full_lisp::Lisp;
pub use interpreter::Program;
pub use parser::parse;
pub use sexp::{int, nil, pair, IntoSexp, Sexp, SexpError};
pub use source_meta::Meta;

#[cfg(test)]
mod test;
pub mod test_utils;

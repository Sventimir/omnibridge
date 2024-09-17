use crate::Expr;
use std::fmt::Debug;

pub mod bool;

pub trait Instr: Debug {
    fn exec(&mut self);
    fn result_as_sexp(&self) -> Expr;
}

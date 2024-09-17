use crate::{var::Var, Expr};
use std::fmt::Debug;

pub mod bool;

pub trait Instr: Debug {
    fn exec(&mut self);
    fn result_var(&self) -> Var;
    fn result_as_sexp(&self) -> Expr;
}

use crate::typed::Expr;


pub mod bool;

pub trait Instr {
    fn exec(&mut self);
    fn result_as_sexp(&self) -> Expr;
}

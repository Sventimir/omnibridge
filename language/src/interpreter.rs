use std::{any::Any, sync::Arc};

use crate::{Expr, IntoSexp, Sexp, nil, type_var::PrimType};

pub enum NextStep {
    Forward,
    JumpTo(usize),
    Return,
    AddReturn,
}

struct Interpreter {
    stack: Vec<Arc<dyn Any>>,
    returns: Vec<usize>,
    cursor: usize,
}

impl Interpreter {
    fn new() -> Self {
        Interpreter {
            stack: Vec::new(),
            returns: Vec::new(),
            cursor: 0,
        }
    }

    fn exec<I>(&mut self, instr: &I)
    where
        I: Instr,
    {
        let args = self.stack.split_off(self.stack.len() - instr.arity());
        let (result, advance) = instr.eval(args.as_slice());
        match advance {
            NextStep::Forward => self.cursor += 1,
            NextStep::JumpTo(n) => self.cursor = n,
            NextStep::Return => self.cursor = self.returns.pop().unwrap(),
            NextStep::AddReturn => {
                self.returns.push(self.cursor);
                self.cursor += 1;
            }
        }
        match result {
            Some(v) => self.stack.push(v),
            None => (),
        }
    }
}

pub trait Instr {
    fn arity(&self) -> usize;
    fn eval(&self, args: &[Arc<dyn Any>]) -> (Option<Arc<dyn Any>>, NextStep);

    fn push_nat(v: u64) -> Self;
    fn push_int(v: i64) -> Self;
    fn push_float(v: f64) -> Self;
    fn push_str(v: String) -> Self;
    fn push_sexp(v: Expr) -> Self;
}

#[derive(Clone)]
pub struct Program<T, I> {
    pub instr: Vec<I>,
    pub ret: T,
}

impl<T: PrimType, I: Instr> Program<T, I> {
    pub fn execute(&self) -> Vec<Arc<dyn Any>>
    where
        I: Instr,
    {
        let mut interp = Interpreter::new();
        while interp.cursor < self.instr.len() {
            interp.exec(&self.instr[interp.cursor]);
        }
        interp.stack.clone()
    }

    pub fn eval<S: Sexp>(&self) -> S {
        let stack = self.execute();
        stack.last().map(|v| self.ret.repr(v.clone())).unwrap_or(nil())
    }
}

impl<T, I: IntoSexp> IntoSexp for Program<T, I> {
    fn into_sexp<S: Sexp>(self) -> S {
        self.instr.into_sexp()
    }
}

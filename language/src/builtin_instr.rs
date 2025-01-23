use std::{any::Any, sync::Arc};

use crate::{interpreter::{Instr, NextStep}, Expr};


pub enum BuiltinInstr {
    Push(Arc<dyn Any>),
    Not,
    And(usize),
    Or(usize),
    Add(usize),
    Mul(usize),
    Eq
}

impl Instr for BuiltinInstr {
    type Value = Arc<dyn Any>;

    fn arity(&self) -> usize {
        match self {
            BuiltinInstr::Push(_) => 0,
            BuiltinInstr::Not => 1,
            BuiltinInstr::Eq => 2,
            BuiltinInstr::And(arity)
            | BuiltinInstr::Or(arity)
            | BuiltinInstr::Add(arity)
            | BuiltinInstr::Mul(arity) => 
                *arity,
        }
    }

    fn eval(&self, args: &[Self::Value]) -> (Option<Self::Value>, NextStep) {
        match self {
            BuiltinInstr::Push(v) => (Some(v.clone()), NextStep::Forward),
            BuiltinInstr::Not => (
                Some(Arc::new(!args[0].downcast_ref::<bool>().unwrap())),
                NextStep::Forward,
            ),
            BuiltinInstr::And(_) => (
                Some(Arc::new(args.iter().fold(true, |l, r| l & r.downcast_ref().unwrap()))),
                NextStep::Forward,
            ),
            BuiltinInstr::Or(_) =>(
                Some(Arc::new(args.iter().fold(false, |l, r| l | r.downcast_ref().unwrap()))),
                NextStep::Forward,
            ),
            BuiltinInstr::Add(_) => (
                Some(Arc::new(args.iter().fold(0 as f64, |l, r| l + r.downcast_ref::<f64>().unwrap()))),
                NextStep::Forward,
            ),
            BuiltinInstr::Mul(_) => (
                Some(Arc::new(args.iter().fold(1 as f64, |l, r| l * r.downcast_ref::<f64>().unwrap()))),
                NextStep::Forward,
            ),
            BuiltinInstr::Eq => (
                Some(Arc::new(args[0].downcast_ref::<f64>() == args[1].downcast_ref::<f64>())),
                NextStep::Forward,
            ),
        }
    }

    fn push_nat(v: u64) -> Self {
        BuiltinInstr::Push(Arc::new(v))
    }

    fn push_int(v: i64) -> Self {
        BuiltinInstr::Push(Arc::new(v))
    }

    fn push_float(v: f64) -> Self {
        BuiltinInstr::Push(Arc::new(v))
    }

    fn push_str(v: String) -> Self {
        BuiltinInstr::Push(Arc::new(v))
    }

    fn push_sexp(v: Expr) -> Self {
        BuiltinInstr::Push(Arc::new(v))
    }
}


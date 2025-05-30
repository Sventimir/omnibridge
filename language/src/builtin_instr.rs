use std::{any::Any, sync::Arc};

use crate::{
    interpreter::{Instr, NextStep},
    Expr, IntoSexp, Sexp,
};

#[derive(Clone)]
pub enum BuiltinInstr {
    Push(Arc<dyn Any>),
    Not,
    And(usize),
    Or(usize),
    AddNat(usize),
    AddInt(usize),
    AddFlt(usize),
    MulNat(usize),
    MulInt(usize),
    MulFlt(usize),
    EqNat,
    EqInt,
    EqFlt,
    EqBool,
    EqString,
    Conv(fn(Arc<dyn Any>) -> Arc<dyn Any>),
}

impl Instr for BuiltinInstr {
    type Value = Arc<dyn Any>;

    fn arity(&self) -> usize {
        match self {
            BuiltinInstr::Push(_) => 0,
            BuiltinInstr::Not => 1,
            BuiltinInstr::EqNat
            | BuiltinInstr::EqInt
            | BuiltinInstr::EqFlt
            | BuiltinInstr::EqBool
            | BuiltinInstr::EqString => 2,
            BuiltinInstr::And(arity)
            | BuiltinInstr::Or(arity)
            | BuiltinInstr::AddNat(arity)
            | BuiltinInstr::AddInt(arity)
            | BuiltinInstr::AddFlt(arity)
            | BuiltinInstr::MulNat(arity)
            | BuiltinInstr::MulInt(arity)
            | BuiltinInstr::MulFlt(arity) => *arity,
            BuiltinInstr::Conv(_) => 1,
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
                Some(Arc::new(
                    args.iter().fold(true, |l, r| l & r.downcast_ref().unwrap()),
                )),
                NextStep::Forward,
            ),
            BuiltinInstr::Or(_) => (
                Some(Arc::new(
                    args.iter()
                        .fold(false, |l, r| l | r.downcast_ref().unwrap()),
                )),
                NextStep::Forward,
            ),
            BuiltinInstr::AddNat(_) => (
                Some(Arc::new(
                    args.iter()
                        .fold(0 as u64, |l, r| l + r.downcast_ref::<u64>().unwrap()),
                )),
                NextStep::Forward,
            ),
            BuiltinInstr::AddInt(_) => (
                Some(Arc::new(
                    args.iter()
                        .fold(0 as i64, |l, r| l + r.downcast_ref::<i64>().unwrap()),
                )),
                NextStep::Forward,
            ),
            BuiltinInstr::AddFlt(_) => (
                Some(Arc::new(
                    args.iter()
                        .fold(0 as f64, |l, r| l + r.downcast_ref::<f64>().unwrap()),
                )),
                NextStep::Forward,
            ),
            BuiltinInstr::MulNat(_) => (
                Some(Arc::new(
                    args.iter()
                        .fold(0 as u64, |l, r| l * r.downcast_ref::<u64>().unwrap()),
                )),
                NextStep::Forward,
            ),
            BuiltinInstr::MulInt(_) => (
                Some(Arc::new(
                    args.iter()
                        .fold(0 as i64, |l, r| l * r.downcast_ref::<i64>().unwrap()),
                )),
                NextStep::Forward,
            ),
            BuiltinInstr::MulFlt(_) => (
                Some(Arc::new(
                    args.iter()
                        .fold(0 as f64, |l, r| l * r.downcast_ref::<f64>().unwrap()),
                )),
                NextStep::Forward,
            ),
            BuiltinInstr::EqNat => (
                Some(Arc::new(
                    args[0].downcast_ref::<u64>() == args[1].downcast_ref::<u64>(),
                )),
                NextStep::Forward,
            ),
            BuiltinInstr::EqInt => (
                Some(Arc::new(
                    args[0].downcast_ref::<i64>() == args[1].downcast_ref::<i64>(),
                )),
                NextStep::Forward,
            ),
            BuiltinInstr::EqFlt => (
                Some(Arc::new(
                    args[0].downcast_ref::<f64>() == args[1].downcast_ref::<f64>(),
                )),
                NextStep::Forward,
            ),
            BuiltinInstr::EqBool => (
                Some(Arc::new(
                    args[0].downcast_ref::<bool>() == args[1].downcast_ref::<bool>(),
                )),
                NextStep::Forward,
            ),
            BuiltinInstr::EqString => (
                Some(Arc::new(
                    args[0].downcast_ref::<String>() == args[1].downcast_ref::<String>(),
                )),
                NextStep::Forward,
            ),
            BuiltinInstr::Conv(f) => (Some(f(args[0].clone())), NextStep::Forward),
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

impl IntoSexp for BuiltinInstr {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            BuiltinInstr::Push(_) => S::symbol("push".to_string()),
            BuiltinInstr::Not => S::symbol("not".to_string()),
            BuiltinInstr::And(arity) => {
                S::list(vec![S::symbol("and".to_string()), S::nat(arity as u64)])
            }
            BuiltinInstr::Or(arity) => {
                S::list(vec![S::symbol("or".to_string()), S::nat(arity as u64)])
            }
            BuiltinInstr::AddNat(arity) => {
                S::list(vec![S::symbol("add-nat".to_string()), S::nat(arity as u64)])
            }
            BuiltinInstr::AddInt(arity) => {
                S::list(vec![S::symbol("add-int".to_string()), S::nat(arity as u64)])
            }
            BuiltinInstr::AddFlt(arity) => S::list(vec![
                S::symbol("add-float".to_string()),
                S::nat(arity as u64),
            ]),
            BuiltinInstr::MulNat(arity) => {
                S::list(vec![S::symbol("mul-nat".to_string()), S::nat(arity as u64)])
            }
            BuiltinInstr::MulInt(arity) => {
                S::list(vec![S::symbol("mul-int".to_string()), S::nat(arity as u64)])
            }
            BuiltinInstr::MulFlt(arity) => S::list(vec![
                S::symbol("mul-float".to_string()),
                S::nat(arity as u64),
            ]),
            BuiltinInstr::EqNat => S::symbol("eq-nat".to_string()),
            BuiltinInstr::EqInt => S::symbol("eq-int".to_string()),
            BuiltinInstr::EqFlt => S::symbol("eq-float".to_string()),
            BuiltinInstr::EqBool => S::symbol("eq-bool".to_string()),
            BuiltinInstr::EqString => S::symbol("eq-string".to_string()),
            BuiltinInstr::Conv(_) => S::symbol("conv".to_string()),
        }
    }
}

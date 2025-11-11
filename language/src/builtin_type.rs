use std::{any::Any, cmp::Ordering, sync::Arc};

use crate::{
    Expr, IntoSexp, Sexp, sexp::nil, type_error::TypeError, type_var::{PrimType, TypeEnv, TypeVar}
};

#[derive(Clone, Debug)]
pub enum BuiltinType {
    Bool,
    Nat,
    Int,
    Float,
    String,
    Expr,
    Nil,
    Fun {
        args: Vec<TypeVar<BuiltinType>>,
        ret: Box<TypeVar<BuiltinType>>,
    },
}

impl PartialEq for BuiltinType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (BuiltinType::Bool, BuiltinType::Bool)
            | (BuiltinType::Nat, BuiltinType::Nat)
            | (BuiltinType::Int, BuiltinType::Int)
            | (BuiltinType::Float, BuiltinType::Float)
            | (BuiltinType::String, BuiltinType::String)
            | (BuiltinType::Expr, BuiltinType::Expr)
            | (BuiltinType::Nil, BuiltinType::Nil) => true,
            (
                BuiltinType::Fun {
                    args: argl,
                    ret: retl,
                },
                BuiltinType::Fun {
                    args: argr,
                    ret: retr,
                },
            ) => {
                if argl.len() != argr.len() {
                    return false;
                }
                for (arg_l, arg_r) in argl.iter().zip(argr.iter()) {
                    if *arg_l != *arg_r {
                        return false;
                    }
                }
                **retl == **retr
            }
            _ => false,
        }
    }
}

impl Eq for BuiltinType {}

impl PartialOrd for BuiltinType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BuiltinType {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (BuiltinType::Nil, BuiltinType::Nil) => Ordering::Equal,
            (BuiltinType::Nil, _) => Ordering::Less,
            (_, BuiltinType::Nil) => Ordering::Greater,
            (BuiltinType::Bool, BuiltinType::Bool) => Ordering::Equal,
            (BuiltinType::Bool, _) => Ordering::Less,
            (_, BuiltinType::Bool) => Ordering::Greater,
            (BuiltinType::Nat, BuiltinType::Nat) => Ordering::Equal,
            (BuiltinType::Nat, _) => Ordering::Less,
            (_, BuiltinType::Nat) => Ordering::Greater,
            (BuiltinType::Int, BuiltinType::Int) => Ordering::Equal,
            (BuiltinType::Int, _) => Ordering::Less,
            (_, BuiltinType::Int) => Ordering::Greater,
            (BuiltinType::Float, BuiltinType::Float) => Ordering::Equal,
            (BuiltinType::Float, _) => Ordering::Less,
            (_, BuiltinType::Float) => Ordering::Greater,
            (BuiltinType::String, BuiltinType::String) => Ordering::Equal,
            (BuiltinType::String, _) => Ordering::Less,
            (_, BuiltinType::String) => Ordering::Greater,
            (BuiltinType::Expr, BuiltinType::Expr) => Ordering::Equal,
            (BuiltinType::Expr, _) => Ordering::Less,
            (_, BuiltinType::Expr) => Ordering::Greater,
            (
                BuiltinType::Fun {
                    args: argl,
                    ret: retl,
                },
                BuiltinType::Fun {
                    args: argr,
                    ret: retr,
                },
            ) => match argl.len().cmp(&argr.len()) {
                Ordering::Equal => {
                    for (arg_l, arg_r) in argl.iter().zip(argr.iter()) {
                        match arg_l.cmp(arg_r) {
                            Ordering::Equal => continue,
                            ord => return ord,
                        }
                    }
                    retl.cmp(retr)
                }
                ord => return ord,
            },
        }
    }
}

impl PrimType for BuiltinType {
    fn nat() -> Self {
        BuiltinType::Nat
    }

    fn int() -> Self {
        BuiltinType::Int
    }

    fn float() -> Self {
        BuiltinType::Float
    }

    fn string() -> Self {
        BuiltinType::String
    }

    fn sexp() -> Self {
        BuiltinType::Expr
    }

    fn nil() -> Self {
        BuiltinType::Nil
    }

    fn repr<S: Sexp>(&self, v: Arc<dyn Any>) -> S {
        match self {
            BuiltinType::Nil => nil(),
            BuiltinType::Bool => v.downcast_ref::<bool>().expect("cast to bool failed").into_sexp(),
            BuiltinType::Nat => v.downcast_ref::<u64>().expect("cast to nat failed").into_sexp(),
            BuiltinType::Int => v.downcast_ref::<i64>().expect("cast to int failed").into_sexp(),
            BuiltinType::Float => v.downcast_ref::<f64>().expect("cast to float failed").into_sexp(),
            BuiltinType::String => v.downcast_ref::<&str>().expect("cast to string failed").into_sexp(),
            BuiltinType::Expr => v.downcast_ref::<Expr>().expect("cast to expr failed").clone().into_sexp(),
            BuiltinType::Fun { .. } => S::list(vec![
                "function".into_sexp(),
                self.clone().into_sexp(),
            ])
        }
    }

    fn fun(args: &[TypeVar<Self>], ret: TypeVar<Self>) -> Self {
        BuiltinType::Fun {
            args: args.iter().map(|arg| arg.make_ref()).collect(),
            ret: Box::new(ret.make_ref()),
        }
    }

    fn as_callable<M: Clone>(&self, arity: usize, meta: &M) -> Result<(Vec<TypeVar<Self>>, TypeVar<Self>), TypeError<M, Self>> {
        match self {
            BuiltinType::Fun { args, ret } if args.len() == arity =>
                Ok((args.iter().map(|arg| arg.make_ref()).collect(), ret.make_ref())),
            _ => {
                let mut args = Vec::with_capacity(arity);
                for _ in 1..arity {
                    args.push(TypeVar::unknown(vec![]));
                }
                Err(TypeError::Mismatch {
                    expected: BuiltinType::Fun { args, ret: Box::new(TypeVar::unknown(vec![])) },
                    found: self.clone(),
                    meta: meta.clone(),
                })
            }
        }
    }

    fn label_type_vars(&mut self, labeler: &mut crate::type_var::VarLabeler) {
        match self {
            BuiltinType::Bool
            | BuiltinType::Nat
            | BuiltinType::Int
            | BuiltinType::Float
            | BuiltinType::String
            | BuiltinType::Expr
            | BuiltinType::Nil => (),
            BuiltinType::Fun { args, ret } => {
                for arg in args {
                    arg.label(labeler)
                }
                ret.label(labeler)
            }
        }
    }

    fn unify<M: Clone, E>(&self, other: &Self, env: &E, meta: &M) -> Result<(), TypeError<M, Self>>
    where
        M: Clone,
        E: TypeEnv<Self>,
    {
        match (self, other) {
            (BuiltinType::Bool, BuiltinType::Bool)
            | (BuiltinType::Nat, BuiltinType::Nat)
            | (BuiltinType::Int, BuiltinType::Int)
            | (BuiltinType::Float, BuiltinType::Float)
            | (BuiltinType::String, BuiltinType::String)
            | (BuiltinType::Expr, BuiltinType::Expr)
            | (BuiltinType::Nil, BuiltinType::Nil) => Ok(()),
            (
                BuiltinType::Fun {
                    args: args_l,
                    ret: ret_l,
                },
                BuiltinType::Fun {
                    args: args_r,
                    ret: ret_r,
                },
            ) => {
                if args_l.len() != args_r.len() {
                    return Err(TypeError::Mismatch {
                        expected: self.clone(),
                        found: other.clone(),
                        meta: meta.clone(),
                    });
                }
                for (arg_l, arg_r) in args_l.iter().zip(args_r.iter()) {
                    arg_l.unify(arg_r, env, meta)?;
                }
                ret_l.unify(ret_r, env, meta)
            }
            _ => Err(TypeError::Mismatch {
                expected: self.clone(),
                found: other.clone(),
                meta: meta.clone(),
            }),
        }
    }
}

impl IntoSexp for BuiltinType {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            BuiltinType::Bool => S::symbol("bool".to_string()),
            BuiltinType::Nat => S::symbol("nat".to_string()),
            BuiltinType::Int => S::symbol("int".to_string()),
            BuiltinType::Float => S::symbol("float".to_string()),
            BuiltinType::String => S::symbol("string".to_string()),
            BuiltinType::Expr => S::symbol("expr".to_string()),
            BuiltinType::Nil => S::symbol("nil".to_string()),
            BuiltinType::Fun { args, ret } => {
                let args_sexp = S::list(args.iter().map(|arg| arg.clone().into_sexp()).collect());
                let ret_sexp = ret.clone().into_sexp();
                S::list(vec![S::symbol("lambda".to_string()), args_sexp, ret_sexp])
            }
        }
    }
}

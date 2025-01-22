use crate::{type_error::TypeError, type_var::{PrimType, TypeVar}};

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
    }
}

impl PartialEq for BuiltinType{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (BuiltinType::Bool, BuiltinType::Bool) 
                | (BuiltinType::Nat, BuiltinType::Nat)
                | (BuiltinType::Int, BuiltinType::Int)
                | (BuiltinType::Float, BuiltinType::Float)
                | (BuiltinType::String, BuiltinType::String)
                | (BuiltinType::Expr, BuiltinType::Expr)
                | (BuiltinType::Nil, BuiltinType::Nil) => true,
            (BuiltinType::Fun { .. }, BuiltinType::Fun { .. }) => 
                panic!("Function types are not comparable"),
            _ => false,
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

    fn fun(args: &[TypeVar<Self>], ret: TypeVar<Self>) -> Self {
        BuiltinType::Fun {
            args: args.iter().map(|arg| arg.clone()).collect(),
            ret: Box::new(ret.clone()),
        }
    }

    fn unify<M>(&self, other: &Self, meta: M) -> Result<(), TypeError<M, Self>> 
    where M: Clone 
    {
        match (self, other) {
            (BuiltinType::Bool, BuiltinType::Bool) 
                | (BuiltinType::Nat, BuiltinType::Nat)
                | (BuiltinType::Int, BuiltinType::Int)
                | (BuiltinType::Float, BuiltinType::Float)
                | (BuiltinType::String, BuiltinType::String)
                | (BuiltinType::Expr, BuiltinType::Expr)
                | (BuiltinType::Nil, BuiltinType::Nil) 
                => Ok(()),
            (BuiltinType::Fun { args: args_l, ret: ret_l },
            BuiltinType::Fun { args: args_r, ret: ret_r }) => {
                if args_l.len() != args_r.len() {
                    return Err(TypeError::Mismatch {
                        expected: self.clone(),
                        found: other.clone(),
                        meta
                    });
                }
                for (arg_l, arg_r) in args_l.iter().zip(args_r.iter()) {
                    arg_l.unify(arg_r, meta.clone())?;
                }
                ret_l.unify(ret_r, meta)
            }
            _ => Err(TypeError::Mismatch {
                expected: self.clone(),
                found: other.clone(),
                meta
            }),
        }
    }
}

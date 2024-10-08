use crate::Expr;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Bool,
    Number,
    String,
    Expr,
    Nil,
    Func(Vec<Type>, Box<Type>),
}

pub trait IType {
    fn tag() -> Type;
}

impl IType for bool {
    fn tag() -> Type {
        Type::Bool
    }
}

impl IType for () {
    fn tag() -> Type {
        Type::Nil
    }
}

impl IType for Expr {
    fn tag() -> Type {
        Type::Expr
    }
}

impl IType for f64 {
    fn tag() -> Type {
        Type::Number
    }
}

impl IType for String {
    fn tag() -> Type {
        Type::String
    }
}

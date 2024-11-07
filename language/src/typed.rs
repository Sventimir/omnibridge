use std::sync::{Arc, Mutex};

use crate::Expr;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Bool,
    Decimal,
    Int,
    Nat,
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
        Type::Decimal
    }
}

impl IType for i64 {
    fn tag() -> Type {
        Type::Int
    }
}

impl IType for u64 {
    fn tag() -> Type {
        Type::Nat
    }
}

impl IType for String {
    fn tag() -> Type {
        Type::String
    }
}

#[derive(Clone, Debug)]
pub struct TypeVar {
    resolved: Arc<Mutex<Option<Type>>>,
}

impl TypeVar {
    pub fn new() -> Self {
        TypeVar {
            resolved: Arc::new(Mutex::new(None)),
        }
    }

    pub fn resolve(&self, ty: Type) {
        let mut resolved = self.resolved.lock().unwrap();
        *resolved = Some(ty);
    }

    pub fn get(&self) -> Option<Type> {
        let resolved = self.resolved.lock().unwrap();
        resolved.clone()
    }
}

use std::{fmt::{self, Debug, Formatter}, sync::{Arc, Mutex}};

use crate::Expr;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeConstr<Param> {
    Bool,
    Decimal,
    Int,
    Nat,
    String,
    Expr,
    Nil,
    Func(Vec<Param>, Box<Param>),
}

#[derive(Clone, PartialEq, Eq)]
pub struct Type(pub TypeConstr<Type>);

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl Type {
    pub fn func(args: &[TypeConstr<Type>], ret: TypeConstr<Type>) -> Type {
        let args = 
            args.iter()
                .map(|arg| Type(arg.clone()))
                .collect();
        Self(TypeConstr::Func(args, Box::new(Type(ret))))
    }
}

pub trait IType {
    fn tag() -> Type;
}

impl IType for bool {
    fn tag() -> Type {
        Type(TypeConstr::Bool)
    }
}

impl IType for () {
    fn tag() -> Type {
        Type(TypeConstr::Nil)
    }
}

impl IType for Expr {
    fn tag() -> Type {
        Type(TypeConstr::Expr)
    }
}

impl IType for f64 {
    fn tag() -> Type {
        Type(TypeConstr::Decimal)
    }
}

impl IType for i64 {
    fn tag() -> Type {
        Type(TypeConstr::Int)
    }
}

impl IType for u64 {
    fn tag() -> Type {
        Type(TypeConstr::Nat)
    }
}

impl IType for String {
    fn tag() -> Type {
        Type(TypeConstr::String)
    }
}

#[derive(Clone, Debug)]
pub struct TypeVar {
    resolved: Arc<Mutex<Option<TypeConstr<TypeVar>>>>,
}

impl TypeVar {
    pub fn new() -> Self {
        TypeVar {
            resolved: Arc::new(Mutex::new(None)),
        }
    }

    pub fn resolve(&self, ty: TypeConstr<TypeVar>) {
        let mut resolved = self.resolved.lock().unwrap();
        *resolved = Some(ty);
    }

    pub fn get(&self) -> Option<TypeConstr<TypeVar>> {
        let resolved = self.resolved.lock().unwrap();
        resolved.clone()
    }
}

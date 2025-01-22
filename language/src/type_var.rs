use std::sync::{Arc, Mutex};

use crate::type_error::TypeError;

pub trait PrimType: Sized {
    fn nat() -> Self;
    fn int() -> Self;
    fn float() -> Self;
    fn string() -> Self;
    fn sexp() -> Self;
    fn nil() -> Self;
    fn fun(args: &[TypeVar<Self>], ret: TypeVar<Self>) -> Self;

    fn unify<M: Clone>(&self, other: &Self, meta: M) -> Result<(), TypeError<M, Self>>;
}

#[derive(Debug)]
enum ValOrRef<T> {
    Val(Option<T>),
    Ref(TypeVar<T>)
}

#[derive(Debug)]
pub struct TypeVar<T>(Arc<Mutex<ValOrRef<T>>>);

impl<T> Clone for TypeVar<T> {
    fn clone(&self) -> Self {
        TypeVar(self.0.clone())
    }
}

impl<T> TypeVar<T> {
    pub fn unknown() -> Self {
        TypeVar(Arc::new(Mutex::new(ValOrRef::Val(None))))
    }

    pub fn constant(t: T) -> Self {
        TypeVar(Arc::new(Mutex::new(ValOrRef::Val(Some(t)))))
    }


    pub fn make_ref(&self) -> Self {
        TypeVar(Arc::new(Mutex::new(ValOrRef::Ref(self.clone()))))
    }

    // If self held a value previously, that value is returned.
    pub fn set_ref(&self, other: &Self) -> Option<T> {
        let mut this = self.0.lock().unwrap();
        match &*this {
            ValOrRef::Val(_) => {
                match std::mem::replace(&mut *this, ValOrRef::Ref(other.clone())) {
                    ValOrRef::Val(t) => t,
                    _ => unreachable!()
                }
            },
            ValOrRef::Ref(var) => var.set_ref(other),
        }
    }
}

impl<T: Clone> TypeVar<T> {
    pub fn value(&self) -> Option<T> {
        match &*self.0.lock().unwrap() {
            ValOrRef::Val(t) => t.clone(),
            ValOrRef::Ref(var) => var.value(),
        }
    }
}

impl<T: Clone + PrimType> TypeVar<T> {
    pub fn unify<M>(&self, other: &Self, meta: M) -> Result<(), TypeError<M, T>> 
    where M: Clone
    {
        match self.set_ref(other) {
            None => Ok(()),
            Some(prev) => {
                match other.value() {
                    None => {
                        let mut that = other.0.lock().unwrap();
                        *that = ValOrRef::Val(Some(prev));
                        Ok(())
                    },
                    Some(t) => t.unify(&prev, meta),
                }
            }
        }
    }
}

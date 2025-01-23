use std::sync::{Arc, Mutex};

use crate::{type_error::TypeError, IntoSexp, Sexp};

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
    Val { name: String, value: Option<T> },
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
        TypeVar(Arc::new(Mutex::new(ValOrRef::Val { value: None, name: "".to_string() })))
    }

    pub fn constant(t: T) -> Self {
        TypeVar(Arc::new(Mutex::new(ValOrRef::Val { value: Some(t), name: "".to_string() })))
    }


    pub fn make_ref(&self) -> Self {
        TypeVar(Arc::new(Mutex::new(ValOrRef::Ref(self.clone()))))
    }

    // If self held a value previously, that value is returned.
    fn set_ref(&self, other: &Self) -> Option<T> {
        let mut this = self.0.lock().unwrap();
        match &*this {
            ValOrRef::Val { .. } => {
                match std::mem::replace(&mut *this, ValOrRef::Ref(other.make_ref())) {
                    ValOrRef::Val { value, .. } => value,
                    _ => unreachable!()
                }
            },
            ValOrRef::Ref(var) => var.set_ref(other),
        }
    }

    fn set_val(&self, t: T) {
        let mut this = self.0.lock().unwrap();
        match &mut *this {
            ValOrRef::Val { ref mut value, .. } => {
                *value = Some(t);
            },
            ValOrRef::Ref(var) => var.set_val(t),
        }
    }
}

impl<T: Clone> TypeVar<T> {
    pub fn value(&self) -> Option<T> {
        match &*self.0.lock().unwrap() {
            ValOrRef::Val { value: t, .. } => t.clone(),
            ValOrRef::Ref(var) => var.value(),
        }
    }

    pub fn name(&self) -> String {
        match &*self.0.lock().unwrap() {
            ValOrRef::Val { name, .. } => name.clone(),
            ValOrRef::Ref(var) => var.name(),
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
                        other.set_val(prev);
                        Ok(())
                    },
                    Some(t) => t.unify(&prev, meta),
                }
            }
        }
    }
}

impl<T: Clone + IntoSexp> IntoSexp for TypeVar<T> {
    fn into_sexp<S: Sexp>(self) -> S {
        match &*self.0.lock().unwrap() {
            ValOrRef::Val { value: Some(t), .. } => t.clone().into_sexp(),
            ValOrRef::Val { value: None, name } => {
                let mut v = "?".to_string();
                v.push_str(&name);
                S::symbol(v)
            },
            ValOrRef::Ref(var) => var.clone().into_sexp(),
        }
    }
}

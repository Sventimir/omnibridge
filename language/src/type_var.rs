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
enum VarOrRef<T> {
    Val(T),
    Var(String),
    Ref(TypeVar<T>)
}

#[derive(Debug)]
pub struct TypeVar<T>(Arc<Mutex<VarOrRef<T>>>);

impl<T> Clone for TypeVar<T> {
    fn clone(&self) -> Self {
        TypeVar(self.0.clone())
    }
}

impl<T> TypeVar<T> {
    pub fn unknown() -> Self {
        TypeVar(Arc::new(Mutex::new(VarOrRef::Var("".to_string()))))
    }

    pub fn constant(t: T) -> Self {
        TypeVar(Arc::new(Mutex::new(VarOrRef::Val(t))))
    }


    pub fn make_ref(&self) -> Self {
        TypeVar(Arc::new(Mutex::new(VarOrRef::Ref(self.clone()))))
    }

    // If self held a value previously, that value is returned.
    fn set_ref(&self, other: &Self) -> Option<T> {
        let mut this = self.0.lock().unwrap();
        match &*this {
            VarOrRef::Val(_) => {
                match std::mem::replace(&mut *this, VarOrRef::Ref(other.make_ref())) {
                    VarOrRef::Val(value) => Some(value),
                    _ => unreachable!()
                }
            },
            VarOrRef::Var(_) => {
                *this = VarOrRef::Ref(other.make_ref());
                None
            },
            VarOrRef::Ref(var) => var.set_ref(other),
        }
    }

    fn set_val(&self, t: T) {
        let mut this = self.0.lock().unwrap();
        match &mut *this {
            VarOrRef::Val(ref mut value) => {
                *value = t;
            },
            VarOrRef::Var(_) => {
                *this = VarOrRef::Val(t);
            },
            VarOrRef::Ref(var) => var.set_val(t),
        }
    }

    pub fn label(&self, label_counter: &mut u8) {
        let mut this = self.0.lock().unwrap();
        match &mut *this {
            VarOrRef::Val(_) => (),
            VarOrRef::Var(ref mut name) => {
                if name.is_empty() {
                    *name = format!("{}", *label_counter as char);
                    *label_counter += 1;
                }
            },
            VarOrRef::Ref(var) => var.label(label_counter),
        }
    }
}

impl<T: Clone> TypeVar<T> {
    pub fn value(&self) -> Option<T> {
        match &*self.0.lock().unwrap() {
            VarOrRef::Val(t) => Some(t.clone()),
            VarOrRef::Var(_) => None,
            VarOrRef::Ref(var) => var.value(),
        }
    }

    pub fn name(&self) -> String {
        match &*self.0.lock().unwrap() {
            VarOrRef::Val(_) => "<const>".to_string(),
            VarOrRef::Var(name) => name.clone(),
            VarOrRef::Ref(var) => var.name(),
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
            VarOrRef::Val(t) => t.clone().into_sexp(),
            VarOrRef::Var(name) => {
                let mut v = "?".to_string();
                v.push_str(&name);
                S::symbol(v)
            },
            VarOrRef::Ref(var) => var.clone().into_sexp(),
        }
    }
}

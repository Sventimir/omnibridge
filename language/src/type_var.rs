use std::{
    cmp::Ordering,
    sync::{Arc, Mutex},
};
use either::Either;

use crate::{type_error::TypeError, IntoSexp, Sexp};

pub trait TypedMeta {
    type Type;

    fn assign_type(&mut self, ty: TypeVar<Self::Type>);
    fn get_type(&self) -> TypeVar<Self::Type>;

    fn label_type_vars(&self, label_index: Option<&mut u8>) {
        match label_index {
            Some(index) => self.get_type().label(index),
            None => {
                let mut label_index: u8 = 'a' as u8;
                self.get_type().label(&mut label_index);
            }
        }
    }
}

pub trait TypeEnv<T> {
    fn check_constraint<M: Clone>(&self, name: &str, t: &T, meta: &M) -> Result<(), TypeError<M, T>>;
}

pub trait PrimType: Sized {
    fn nat() -> Self;
    fn int() -> Self;
    fn float() -> Self;
    fn string() -> Self;
    fn sexp() -> Self;
    fn nil() -> Self;
    fn fun(args: &[TypeVar<Self>], ret: TypeVar<Self>) -> Self;

    fn unify<M: Clone, E: TypeEnv<Self>>(&self, other: &Self, env: &E, meta: &M) -> Result<(), TypeError<M, Self>>;
}

#[derive(Debug)]
enum VarOrRef<T> {
    Val(T),
    Var(String, Vec<String>),
    Ref(TypeVar<T>),
}

#[derive(Debug)]
pub struct TypeVar<T>(Arc<Mutex<VarOrRef<T>>>);

impl<T> Clone for TypeVar<T> {
    fn clone(&self) -> Self {
        TypeVar(self.0.clone())
    }
}

impl<T> TypeVar<T> {
    pub fn unknown(constraints: &[String]) -> Self {
        TypeVar(Arc::new(Mutex::new(VarOrRef::Var("".to_string(), constraints.to_vec()))))
    }

    pub fn constant(t: T) -> Self {
        TypeVar(Arc::new(Mutex::new(VarOrRef::Val(t))))
    }

    pub fn make_ref(&self) -> Self {
        TypeVar(Arc::new(Mutex::new(VarOrRef::Ref(self.clone()))))
    }

    pub fn label(&self, label_counter: &mut u8) {
        let mut this = self.0.lock().unwrap();
        match &mut *this {
            VarOrRef::Val(_) => (),
            VarOrRef::Var(ref mut name, _) => {
                if name.is_empty() {
                    *name = format!("{}", *label_counter as char);
                    *label_counter += 1;
                }
            }
            VarOrRef::Ref(var) => var.label(label_counter),
        }
    }
}

impl<T: Clone> TypeVar<T> {
    pub fn value(&self) -> Option<T> {
        match &*self.0.lock().unwrap() {
            VarOrRef::Val(t) => Some(t.clone()),
            VarOrRef::Var(_, _) => None,
            VarOrRef::Ref(var) => var.value(),
        }
    }

    pub fn name(&self) -> String {
        match &*self.0.lock().unwrap() {
            VarOrRef::Val(_) => "<const>".to_string(),
            VarOrRef::Var(name, _) => name.clone(),
            VarOrRef::Ref(var) => var.name(),
        }
    }
}

impl<T: Clone + PrimType> TypeVar<T> {
    pub fn unify<M: Clone, E: TypeEnv<T>>(&self, other: &Self, env: &E, meta: &M) -> Result<(), TypeError<M, T>>
    where
        M: Clone,
    {
        match (self.deref(), other.deref()) {
            (Either::Left(this_t), Either::Left(that_t)) => this_t.unify(&that_t, env, meta),
            (Either::Left(t), Either::Right(constraints)) => {
                check_constraints(env, &t, &constraints, meta)?;
                other.set_val(&t);
                Ok(())
            }
            (Either::Right(constraints), Either::Left(t)) => {
                check_constraints(env, &t, &constraints, meta)?;
                self.set_val(&t);
                Ok(())
            }
            (Either::Right(_), Either::Right(mut those_constraints)) => {
                self.merge_constraints(&mut those_constraints);
                let mut that = other.0.lock().unwrap();
                *that = VarOrRef::Ref(self.clone());
                Ok(())
            }
        }
    }

    fn deref(&self) -> Either<T, Vec<String>> {
        let this = self.0.lock().unwrap();
        match &*this {
            VarOrRef::Ref(v) => v.deref(),
            VarOrRef::Val(t) => Either::Left(t.clone()),
            VarOrRef::Var(_name, constraints) => Either::Right(constraints.clone()),
        }
    }

    fn merge_constraints(&self, new_constraints: &mut Vec<String>) {
        let mut this = self.0.lock().unwrap();
        match &mut *this {
            VarOrRef::Ref(v) => v.merge_constraints(new_constraints),
            VarOrRef::Val(_) => unreachable!(),
            VarOrRef::Var(_, ref mut old_constraints) => old_constraints.append(new_constraints)
        }
    }

    fn set_val(&self, t: &T) {
        let mut this = self.0.lock().unwrap();
        *this = VarOrRef::Val(t.clone())
    }
}

fn check_constraints<E, T, M>(env: &E, t: &T, cs: &[String], meta: &M) -> Result<(), TypeError<M, T>>
    where T: Clone + PrimType,
          E: TypeEnv<T>,
          M: Clone,
{
    for c in cs {
        env.check_constraint(c, t, meta)?
    }
    Ok(())
}

impl<T: Clone + IntoSexp> IntoSexp for TypeVar<T> {
    // NOTE: Constraints are not being serialized!
    fn into_sexp<S: Sexp>(self) -> S {
        match &*self.0.lock().unwrap() {
            VarOrRef::Val(t) => t.clone().into_sexp(),
            VarOrRef::Var(name, _) => {
                let mut v = "?".to_string();
                v.push_str(&name);
                S::symbol(v)
            }
            VarOrRef::Ref(var) => var.clone().into_sexp(),
        }
    }
}

// NOTE: This treats all unresolved variables as equal. This is
// not perfect, but will will suffice to make types map keys.
impl<T: Clone + PartialEq> PartialEq for TypeVar<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value() == other.value()
    }
}

impl<T: Clone + Eq> Eq for TypeVar<T> {}

impl<T: Clone + PartialOrd> PartialOrd for TypeVar<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value().partial_cmp(&other.value())
    }
}

impl<T: Clone + Ord> Ord for TypeVar<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.value().cmp(&other.value())
    }
}

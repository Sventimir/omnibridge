use either::Either;
use std::{
    cmp::Ordering,
    collections::BTreeSet,
    sync::{Arc, Mutex},
};

use crate::{type_error::TypeError, IntoSexp, Sexp};

pub struct VarLabeler(u8);

impl VarLabeler {
    pub fn new() -> Self {
        Self('a' as u8)
    }

    pub fn next(&mut self) -> String {
        let ret = format!("{}", self.0 as char);
        self.0 += 1;
        ret
    }
}

pub trait TypedMeta {
    type Type: PrimType;

    fn assign_type(&mut self, ty: TypeExpr<Self::Type>);
    fn get_type(&self) -> TypeExpr<Self::Type>;

    fn label_type_vars(&self, labeler: &mut VarLabeler) {
        self.get_type().label_type_vars(labeler)
    }
}

pub trait TypeEnv<T> {
    fn set_default_type<M>(&self, var: &TypeVar<T>, meta: &mut M) -> Result<(), TypeError<M, T>>
    where
        M: Clone + TypedMeta<Type = T>;

    fn check_constraint<M: Clone>(
        &self,
        name: &str,
        t: &T,
        meta: &M,
    ) -> Result<(), TypeError<M, T>>;
}

pub trait PrimType: Sized {
    fn nat() -> Self;
    fn int() -> Self;
    fn float() -> Self;
    fn string() -> Self;
    fn sexp() -> Self;
    fn nil() -> Self;
    fn fun(args: &[TypeVar<Self>], ret: TypeVar<Self>) -> Self;

    fn as_callable<M: Clone>(&self, arity: usize, meta: &M) -> Result<(Vec<TypeVar<Self>>, TypeVar<Self>), TypeError<M, Self>>;
    fn label_type_vars(&mut self, labeler: &mut VarLabeler);
    fn unify<M: Clone, E: TypeEnv<Self>>(
        &self,
        other: &Self,
        env: &E,
        meta: &M,
    ) -> Result<(), TypeError<M, Self>>;
}

#[derive(Debug)]
enum VarOrRef<T> {
    Val(T),
    Var(String, BTreeSet<String>),
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
    pub fn unknown(constraints: Vec<String>) -> Self {
        TypeVar(Arc::new(Mutex::new(VarOrRef::Var(
            "".to_string(),
            constraints.into_iter().collect(),
        ))))
    }

    pub fn constant(t: T) -> Self {
        TypeVar(Arc::new(Mutex::new(VarOrRef::Val(t))))
    }

    pub fn make_ref(&self) -> Self {
        TypeVar(Arc::new(Mutex::new(VarOrRef::Ref(self.clone()))))
    }

    pub fn constraints(&self) -> BTreeSet<String> {
        let this = self.0.lock().unwrap();
        match &*this {
            VarOrRef::Val(_) => BTreeSet::new(),
            VarOrRef::Var(_, constraints) => constraints.clone(),
            VarOrRef::Ref(v) => v.constraints(),
        }
    }
}

impl<T: PrimType> TypeVar<T> {
    pub fn label(&self, labeler: &mut VarLabeler) {
        let mut this = self.0.lock().unwrap();
        match &mut *this {
            VarOrRef::Val(t) => t.label_type_vars(labeler),
            VarOrRef::Var(ref mut name, _) => {
                if name.is_empty() {
                    *name = labeler.next()
                }
            }
            VarOrRef::Ref(var) => var.label(labeler),
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
    pub fn as_callable<M: Clone>(&self, arity: usize, meta: &M) -> Result<(Vec<Self>, Self), TypeError<M, T>> {
        let mut this = self.0.lock().unwrap();
        match &mut *this {
            VarOrRef::Ref(v) => v.as_callable(arity, meta),
            VarOrRef::Val(t) => t.as_callable(arity, meta),
            VarOrRef::Var(_, _) => {
                let mut args = Vec::with_capacity(arity);
                for _ in 1..arity {
                    args.push(Self::unknown(vec![]));
                }
                let ret = Self::unknown(vec![]);
                *this = VarOrRef::Val(T::fun(&args, ret.make_ref()));
                Ok((args, ret))
            }
        }
    }

    pub fn unify<M: Clone, E: TypeEnv<T>>(
        &self,
        other: &Self,
        env: &E,
        meta: &M,
    ) -> Result<(), TypeError<M, T>>
    where
        M: Clone,
    {
        match (self.deref(), other.deref()) {
            (Either::Left(this_t), Either::Left(that_t)) => this_t.unify(&that_t, env, meta),
            (Either::Left(t), Either::Right(constraints)) => {
                check_constraints(env, &t, constraints, meta)?;
                other.set_ref(self.clone());
                Ok(())
            }
            (Either::Right(constraints), Either::Left(t)) => {
                check_constraints(env, &t, constraints, meta)?;
                self.set_ref(other.clone());
                Ok(())
            }
            (Either::Right(_), Either::Right(mut those_constraints)) => {
                self.merge_constraints(&mut those_constraints);
                other.set_ref(self.clone());
                Ok(())
            }
        }
    }

    fn deref(&self) -> Either<T, BTreeSet<String>> {
        let this = self.0.lock().unwrap();
        match &*this {
            VarOrRef::Ref(v) => v.deref(),
            VarOrRef::Val(t) => Either::Left(t.clone()),
            VarOrRef::Var(_name, constraints) => Either::Right(constraints.clone()),
        }
    }

    fn merge_constraints(&self, new_constraints: &mut BTreeSet<String>) {
        let mut this = self.0.lock().unwrap();
        match &mut *this {
            VarOrRef::Ref(v) => v.merge_constraints(new_constraints),
            VarOrRef::Val(_) => unreachable!(),
            VarOrRef::Var(_, ref mut old_constraints) => old_constraints.append(new_constraints),
        }
    }

    fn set_ref(&self, src: Self) {
        let mut this = self.0.lock().unwrap();
        match &*this {
            VarOrRef::Ref(v) => v.set_ref(src),
            _ => *this = VarOrRef::Ref(src),
        }
    }

    pub fn set_val(&self, t: T) {
        let mut this = self.0.lock().unwrap();
        match &*this {
            VarOrRef::Ref(v) => v.set_val(t),
            _ => *this = VarOrRef::Val(t),
        }
    }
}

fn check_constraints<E, T, M>(
    env: &E,
    t: &T,
    cs: BTreeSet<String>,
    meta: &M,
) -> Result<(), TypeError<M, T>>
where
    T: Clone + PrimType,
    E: TypeEnv<T>,
    M: Clone,
{
    for c in cs.iter() {
        env.check_constraint(c, t, meta)?
    }
    Ok(())
}

impl<T: Clone + IntoSexp> IntoSexp for TypeVar<T> {
    fn into_sexp<S: Sexp>(self) -> S {
        match &*self.0.lock().unwrap() {
            VarOrRef::Val(t) => t.clone().into_sexp(),
            VarOrRef::Var(name, constraints) => {
                let name = S::symbol(format!("?{}", name));
                if constraints.len() > 0 {
                    let mut ret = vec![ name ];
                    for c in constraints {
                        ret.push(S::symbol(c.to_string()))
                    }
                    S::list(ret)
                } else {
                    name
                }
            },
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

#[derive(Clone, Debug)]
pub struct TypeExpr<T> {
    pub body: TypeVar<T>,
    pub vars: Vec<TypeVar<T>>,
}

impl<T> TypeExpr<T> {
    pub fn constant(t: T) -> Self {
        Self {
            body: TypeVar::constant(t),
            vars: vec![],
        }
    }

    pub fn make_ref(&self) -> Self {
        Self {
            body: self.body.make_ref(),
            vars: self.vars.iter().map(|v| v.make_ref()).collect(),
        }
    }
}

impl<T: PrimType> TypeExpr<T> {
    pub fn label_type_vars(&mut self, labeler: &mut VarLabeler) {
        for v in self.vars.iter_mut() {
            v.label(labeler)
        }
    }
}

impl<T: Clone + PrimType> TypeExpr<T> {
    pub fn unify<M: Clone, E: TypeEnv<T>>(
        &self,
        other: &Self,
        env: &E,
        meta: &M,
    ) -> Result<(), TypeError<M, T>> {
        self.body.unify(&other.body, env, meta)
    }
}

impl<T: Clone + IntoSexp> IntoSexp for TypeExpr<T> {
    fn into_sexp<S: Sexp>(self) -> S {
        self.body.into_sexp()
    }
}

#[derive(Debug)]
pub struct TypeExprGen<T> {
    pub gen: fn(&TypeExprGen<T>) -> TypeExpr<T>,
}

impl<T> TypeExprGen<T> {
    pub fn make(&self) -> TypeExpr<T> {
        (self.gen)(self)
    }
}

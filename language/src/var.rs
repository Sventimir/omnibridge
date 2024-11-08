use std::{
    any::Any,
    fmt::{self, Debug, Formatter},
    sync::{Arc, Mutex},
};

use crate::typed::{IType, Type, TypeConstr};

pub struct Var {
    id: usize,
    val: Arc<Mutex<Box<dyn Any>>>,
    typ: Type,
}

impl Var {
    pub fn new<T: Any + IType>(id: usize, val: T) -> Self {
        Self {
            id,
            typ: T::tag(),
            val: Arc::new(Mutex::new(Box::new(val))),
        }
    }

    pub fn clone(&self) -> Self {
        Self {
            id: self.id,
            val: Arc::clone(&self.val),
            typ: self.typ.clone(),
        }
    }

    pub fn lock(&self) -> std::sync::MutexGuard<Box<dyn Any>> {
        self.val.try_lock().unwrap()
    }

    pub fn typ(&self) -> Type {
        self.typ.clone()
    }

    pub fn value<T: IType + Clone + 'static>(&self) -> Option<T> {
        if self.typ == T::tag() {
            let v = self.lock();
            let val: &T = v.downcast_ref().unwrap();
            Some(val.clone())
        } else {
            None
        }
    }
}

impl Debug for Var {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let v = self.lock();
        match self.typ.0 {
            TypeConstr::Bool => {
                let v: &bool = v.downcast_ref().unwrap();
                return write!(f, "Var({}: bool = {:?})", self.id, v);
            }
            TypeConstr::Decimal => {
                let v: &f64 = v.downcast_ref().unwrap();
                return write!(f, "Var({}: decimal = {:?})", self.id, v);
            }
            TypeConstr::Int => {
                let v: &i64 = v.downcast_ref().unwrap();
                return write!(f, "Var({}: int = {:?})", self.id, v);
            }
            TypeConstr::Nat => {
                let v: &u64 = v.downcast_ref().unwrap();
                return write!(f, "Var({}: nat = {:?})", self.id, v);
            }
            TypeConstr::String => {
                let v: &String = v.downcast_ref().unwrap();
                return write!(f, "Var({}: string = {:?})", self.id, v);
            }
            TypeConstr::Expr => {
                let v: &String = v.downcast_ref().unwrap();
                return write!(f, "Var({}: sexp = {:?})", self.id, v);
            }
            TypeConstr::Nil => {
                return write!(f, "Var({}: nil = nil)", self.id);
            }
            TypeConstr::Func(_, _) => {
                return write!(f, "Var({}: {:?})", self.id, self.typ);
            }
        }
    }
}

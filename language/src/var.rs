use std::{
    any::Any, fmt::{self, Debug, Formatter}, sync::{Arc, Mutex}
};

use crate::typed::{Type, IType};

pub struct Var {
    val: Arc<Mutex<Box<dyn Any>>>,
    typ: Type,
}

impl Var {
    pub fn new<T: Any + IType>(val: T) -> Self {
        Self {
            typ: T::tag(),
            val: Arc::new(Mutex::new(Box::new(val))),
        }
    }

    pub fn clone(&self) -> Self {
        Self {
            val: Arc::clone(&self.val),
            typ: self.typ.clone(),
        }
    }

    pub fn lock(&self) -> std::sync::MutexGuard<Box<dyn Any>> {
        self.val.lock().unwrap()
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
        let v: &dyn Any = &*self.lock();
        match self.typ {
            Type::Bool => {
                let v: &bool = v.downcast_ref().unwrap();
                return write!(f, "Var({:?})", v);
            }
            Type::Number => {
                let v: &f32 = v.downcast_ref().unwrap();
                return write!(f, "Var({:?})", v);
            }
            Type::String => {
                let v: &String = v.downcast_ref().unwrap();
                return write!(f, "Var({:?})", v);
            }
            Type::Expr => {
                let v: &String = v.downcast_ref().unwrap();
                return write!(f, "Var({:?})", v);
            }
            Type::Nil => {
                return write!(f, "Var(nil)");
            }
            Type::Func(_, _) => {
                return write!(f, "Var(func)");
            }
        }
    }
}

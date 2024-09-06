use std::{
    fmt::{self, Debug, Formatter},
    sync::{Arc, Mutex},
};

use crate::typed::Type;

pub struct Var<T: Type> {
    val: Arc<Mutex<T::Repr>>,
    typ: T,
}

impl<T: Type> Var<T> {
    pub fn new(t: T, val: T::Repr) -> Self {
        Self {
            typ: t,
            val: Arc::new(Mutex::new(val)),
        }
    }

    pub fn clone(&self) -> Self {
        Self {
            val: Arc::clone(&self.val),
            typ: self.typ.clone(),
        }
    }

    pub fn lock(&self) -> std::sync::MutexGuard<T::Repr> {
        self.val.lock().unwrap()
    }

    pub fn value(&self) -> T::Repr {
        (*self.lock()).clone()
    }
}

impl<T> Debug for Var<T>
where
    T: Type,
    T::Repr: Debug,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Var({:?})", self.value())
    }
}

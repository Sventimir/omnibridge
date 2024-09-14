use std::{
    fmt::{self, Debug, Formatter},
    sync::{Arc, Mutex},
};

use crate::typed::{Type, IType};

pub struct Var<T: IType + Clone> {
    val: Arc<Mutex<T>>,
    typ: Type,
}

impl<T: IType + Clone> Var<T> {
    pub fn new(val: T) -> Self {
        Self {
            typ: T::tag(),
            val: Arc::new(Mutex::new(val)),
        }
    }

    pub fn clone(&self) -> Self {
        Self {
            val: Arc::clone(&self.val),
            typ: self.typ.clone(),
        }
    }

    pub fn lock(&self) -> std::sync::MutexGuard<T> {
        self.val.lock().unwrap()
    }

    pub fn value(&self) -> T {
        (*self.lock()).clone()
    }
}

impl<T: IType + Clone + Debug> Debug for Var<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Var({:?})", self.value())
    }
}

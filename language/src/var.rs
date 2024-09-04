use std::{fmt::{self, Debug, Formatter}, sync::{Arc, Mutex}};

use crate::typed::{MalformedDataError, Type};

pub struct Var<T: Type> {
    val: Arc<Mutex<Vec<u8>>>,
    typ: T,
}

impl<T: Type> Var<T> {
    pub fn new(t: T, val: T::Repr) -> Self {
        Self {
            typ: t,
            val: Arc::new(Mutex::new(T::to_bytes(&val))),
        }
    }

    pub fn clone(&self) -> Self {
        Self {
            val: Arc::clone(&self.val),
            typ: self.typ.clone(),
        }
    }

    pub fn lock(&self) -> std::sync::MutexGuard<Vec<u8>> {
        self.val.lock().unwrap()
    }

    pub fn value(&self) -> Result<T::Repr, MalformedDataError> {
        let val = self.lock();
        T::from_bytes(val.clone())
    }
}

impl<T: Type> Debug for Var<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Var({:?})", *self.lock())
    }
}

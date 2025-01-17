use std::sync::{Arc, Mutex};

use crate::type_error::TypeError;

enum ValOrRef<T> {
    Val(Option<T>),
    Ref(TypeVar<T>)
}

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

impl<T: Clone + Eq> TypeVar<T> {
    pub fn unify<M>(&self, other: &Self, meta: M) -> Result<(), TypeError<M, T>> {
        match self.set_ref(other) {
            None => Ok(()),
            Some(prev) => {
                match other.value() {
                    None => {
                        let mut that = other.0.lock().unwrap();
                        *that = ValOrRef::Val(Some(prev));
                        Ok(())
                    },
                    Some(t) => {
                        if t == prev {
                            Ok(())
                        } else {
                            Err(TypeError::Mismatch {
                                expected: prev,
                                found: t,
                                meta
                            })
                        }
                    }
                }
            }
        }
    }
}


// #[derive(Clone)]
// pub struct TypeVal<T> {
//     resolved: Arc<Mutex<Option<T>>>
// }

// impl<T> TypeVal<T> {
//     fn unknown() -> Self {
//         TypeVal {
//             resolved: Arc::new(Mutex::new(None))
//         }
//     }

//     fn constant(t: T) -> Self {
//         TypeVal {
//             resolved: Arc::new(Mutex::new(Some(t)))
//         }
//     }
// }

// pub struct TypeVar<T> {
//     var: Arc<Mutex<TypeVal<T>>>
// }

// impl<T> TypeVar<T> {
//     pub fn unknown() -> Self {
//         TypeVar {
//             var: Arc::new(Mutex::new(TypeVal::unknown()))
//         }
//     }

//     pub fn constant(t: T) -> Self {
//         TypeVar {
//             var: Arc::new(Mutex::new(TypeVal::constant(t)))
//         }
//     }
// }

// impl<T: Clone> TypeVar<T> {
//     pub fn value(&self) -> Option<T> {
//         self.var.lock().unwrap().resolved.lock().unwrap().clone()
//     }
// }

// impl<T: Clone + Eq> TypeVar<T> {
//     pub fn unify<M>(&self, other: &TypeVar<T>, meta: M) -> Result<(), TypeError<M, T>> {
//         let mut this = self.var.lock().unwrap();
//         let this_val: Option<T>;
//         {
//             this_val = this.resolved.lock().unwrap().clone();
//         };
//         let that = other.var.lock().unwrap();
//         if let Some(t) = &this_val {
//             let mut that_val = that.resolved.lock().unwrap();
//             match &*that_val {
//                 None => *that_val = this_val,
//                 Some(t1) => {
//                     if *t != *t1 {
//                         return Err(TypeError::Mismatch {
//                             expected: t.clone(),
//                             found: t1.clone(),
//                             meta
//                         })
//                     }
//                 }
//             }
//         };
//         *this = that.clone();
//         Ok(())
//     }
// }

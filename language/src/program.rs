use std::fmt::{self, Debug, Formatter};
use std::sync::{Arc, Mutex};

use crate::typed::{Bool, MalformedDataError, Type};

pub struct Var<T: Type> {
    val: Arc<Mutex<Vec<u8>>>,
    typ: T,
}

impl<T: Type> Var<T> {
    fn new(t: T, val: T::Repr) -> Self {
        Self {
            typ: t,
            val: Arc::new(Mutex::new(T::to_bytes(&val))),
        }
    }

    fn clone(&self) -> Self {
        Self {
            val: Arc::clone(&self.val),
            typ: self.typ.clone(),
        }
    }

    fn lock(&self) -> std::sync::MutexGuard<Vec<u8>> {
        self.val.lock().unwrap()
    }

    pub fn value(&self) -> Result<T::Repr, MalformedDataError> {
        let val = self.lock();
        T::from_bytes(val.clone())
    }
}

#[derive(Debug)]
pub struct Program {
    instructions: Arc<Mutex<Vec<Instr>>>,
}

pub enum Instr {
    Not {
        arg: Var<Bool>,
        result: Var<Bool>,
    },
    And {
        left: Var<Bool>,
        right: Var<Bool>,
        result: Var<Bool>,
    },
    Or {
        left: Var<Bool>,
        right: Var<Bool>,
        result: Var<Bool>,
    },
}

impl Program {
    pub fn new() -> Self {
        Program {
            instructions: Arc::new(Mutex::new(Vec::new())),
        }
    }

    pub fn alloc<T: Type>(&self, t: T, val: T::Repr) -> Var<T> {
        Var::new(t, val)
    }

    pub fn exec(&self) {
        let mut instrs = self.instructions.lock().unwrap();
        for instr in &mut *instrs {
            instr.exec();
        }
    }

    pub fn push_instr(&self, instr: Instr) {
        let mut instrs = self.instructions.lock().unwrap();
        (*instrs).push(instr);
    }

    pub fn push_instr_not(&self, arg: &Var<Bool>) -> Var<Bool> {
        let result = Var::new(Bool, Default::default());
        self.push_instr(Instr::Not {
            arg: arg.clone(),
            result: result.clone(),
        });
        result
    }

    pub fn push_instr_and(&self, left: &Var<Bool>, right: &Var<Bool>) -> Var<Bool> {
        let result = Var::new(Bool, Default::default());
        self.push_instr(Instr::And {
            left: left.clone(),
            right: right.clone(),
            result: result.clone(),
        });
        result
    }

    pub fn push_instr_or(&self, left: &Var<Bool>, right: &Var<Bool>) -> Var<Bool> {
        let result = Var::new(Bool, Default::default());
        self.push_instr(Instr::Or {
            left: left.clone(),
            right: right.clone(),
            result: result.clone(),
        });
        result
    }
}

impl Instr {
    pub fn exec(&mut self) {
        match self {
            Instr::Not { arg, result } => {
                let mut ret = result.lock();
                ret.clear();
                for byte in arg.lock().iter() {
                    ret.push(!byte);
                }
            }
            Instr::And {
                left,
                right,
                result,
            } => {
                let mut ret = result.lock();
                ret.clear();
                for (l, r) in left.lock().iter().zip(right.lock().iter()) {
                    ret.push(l & r);
                }
            }
            Instr::Or {
                left,
                right,
                result,
            } => {
                let mut ret = result.lock();
                ret.clear();
                for (l, r) in left.lock().iter().zip(right.lock().iter()) {
                    ret.push(l | r);
                }
            }
        }
    }
}

impl Debug for Instr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Instr::Not { arg, result } => write!(f, "Not({:?} -> {:?})", arg.lock(), result.lock()),
            Instr::And {
                left,
                right,
                result,
            } => {
                write!(
                    f,
                    "And({:?}, {:?} -> {:?})",
                    left.lock(),
                    right.lock(),
                    result.lock()
                )
            }
            Instr::Or {
                left,
                right,
                result,
            } => {
                write!(
                    f,
                    "Or({:?}, {:?} -> {:?})",
                    left.lock(),
                    right.lock(),
                    result.lock()
                )
            }
        }
    }
}

impl<T: Type> Debug for Var<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Var({:?})", *self.lock())
    }
}

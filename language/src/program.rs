use std::fmt::{self, Debug, Formatter};
use std::sync::{Arc, Mutex};

pub struct Var(Arc<Mutex<u8>>);

impl Var {
    pub fn new(val: u8) -> Self {
        Var(Arc::new(Mutex::new(val)))
    }

    fn clone(&self) -> Self {
        Self(Arc::clone(&self.0))
    }

    fn lock(&self) -> std::sync::MutexGuard<u8> {
        self.0.lock().unwrap()
    }

    pub fn value(&self) -> u8 {
        *self.lock()
    }
}

#[derive(Debug)]
pub struct Program {
    instructions: Arc<Mutex<Vec<Instr>>>,
}

pub enum Instr {
    Not { arg: Var, result: Var },
    And { left: Var, right: Var, result: Var },
}

impl Program {
    pub fn new() -> Self {
        Program {
            instructions: Arc::new(Mutex::new(Vec::new())),
        }
    }

    pub fn exec(&self) -> Var {
        let mut instrs = self.instructions.lock().unwrap();
        for instr in &mut *instrs {
            instr.exec();
        }
        instrs.last().unwrap().result()
    }

    pub fn push_instr(&self, instr: Instr) {
        let mut instrs = self.instructions.lock().unwrap();
        (*instrs).push(instr);
    }

    pub fn push_instr_not(&self, arg: &Var) -> Var {
        let result = Var::new(123);
        self.push_instr(Instr::Not {
            arg: arg.clone(),
            result: result.clone(),
        });
        result
    }

    pub fn push_instr_and(&self, left: &Var, right: &Var) -> Var {
        let result = Var::new(18);
        self.push_instr(Instr::And {
            left: left.clone(),
            right: right.clone(),
            result: result.clone(),
        });
        result
    }

    pub fn result(&self) -> Option<Var> {
        let instrs = self.instructions.lock().unwrap();
        (*instrs).last().map(Instr::result)
    }
}

impl Instr {
    pub fn exec(&mut self) {
        match self {
            Instr::Not { arg, result } => {
                let mut ret = result.lock();
                *ret = !*arg.lock();
            }
            Instr::And {
                left,
                right,
                result,
            } => {
                let mut ret = result.lock();
                *ret = *left.lock() & *right.lock();
            }
        }
    }

    fn result(&self) -> Var {
        match self {
            Instr::Not { result, .. } | Instr::And { result, .. } => result.clone(),
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
        }
    }
}

impl Debug for Var {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Var({:?})", *self.lock())
    }
}

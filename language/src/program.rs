use std::sync::{Arc, Mutex};

use crate::{
    instr::Instr, nil, typed::{Expr, Type}, var::Var
};

pub struct Program {
    instructions: Arc<Mutex<Vec<Box<dyn Instr>>>>,
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

    pub fn push_instr(&self, instr: Box<dyn Instr>) {
        let mut instrs = self.instructions.lock().unwrap();
        (*instrs).push(instr);
    }

    pub fn result_as_sexp(&self) -> Expr {
        let instrs = self.instructions.lock().unwrap();
        match instrs.last() {
            None => nil(),
            Some(instr) => instr.result_as_sexp(),
        }
    }
}


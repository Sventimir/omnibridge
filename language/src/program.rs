use std::sync::{Arc, Mutex};

use crate::{
    instr::Instr, nil, typed::IType, var::Var, Expr
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

    pub fn alloc<T: IType + Clone>(&self, val: T) -> Var<T> {
        Var::new(val)
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


use std::{
    fmt::{self, Debug, Formatter},
    sync::{Arc, Mutex},
};

use crate::{
    instr::Instr,
    nil,
    typed::{IType, Type},
    var::Var,
    Expr,
};

pub struct Program {
    instructions: Arc<Mutex<Vec<Box<dyn Instr>>>>,
    ty: Type,
}

impl Debug for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let contents = self.instructions.lock().unwrap();
        let instr: String = contents.iter().map(|i| format!("{:?}, ", i)).collect();
        write!(f, "Program : {:?} [{}]", self.ty, instr)
    }
}

impl Program {
    pub fn new() -> Self {
        Program {
            instructions: Arc::new(Mutex::new(Vec::new())),
            ty: Type::Nil,
        }
    }

    pub fn alloc<T: IType + Clone + 'static>(&self, val: T) -> Var {
        Var::new(val)
    }

    pub fn assign_type(&mut self, ty: Type) {
        self.ty = ty;
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

    pub fn result<T: IType + Clone + 'static>(&self) -> Option<T> {
        let instrs = self.instructions.lock().unwrap();
        instrs.last().and_then(|instr| instr.result_var().value())
    }

    pub fn result_as_sexp(&self) -> Expr {
        let instrs = self.instructions.lock().unwrap();
        match instrs.last() {
            None => nil(),
            Some(instr) => instr.result_as_sexp(),
        }
    }
}

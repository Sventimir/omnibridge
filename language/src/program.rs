use std::{
    fmt::{self, Debug, Formatter},
    sync::{Arc, Mutex},
};

use crate::{
    instr::Instr,
    nil,
    typed::{IType, Type, TypeConstr, TypePrimitive},
    var::Var,
    Expr,
};

pub struct Program {
    instructions: Arc<Mutex<Vec<Box<dyn Instr>>>>,
    ty: Type,
    next_var_id: usize,
}

impl Debug for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let contents = self.instructions.try_lock().unwrap();
        let instr: String = contents.iter().map(|i| format!("{:?}, ", i)).collect();
        write!(f, "Program : {:?} [{}]", self.ty, instr)
    }
}

impl Program {
    pub fn new() -> Self {
        Program {
            instructions: Arc::new(Mutex::new(Vec::new())),
            ty: Type(TypeConstr::Prim(TypePrimitive::Nil)),
            next_var_id: 0,
        }
    }

    pub fn alloc<T: IType + Clone + 'static>(&mut self, val: T) -> Var {
        let id = self.next_var_id;
        self.next_var_id += 1;
        Var::new(id, val)
    }

    pub fn assign_type(&mut self, ty: Type) {
        self.ty = ty;
    }

    pub fn exec(&self) {
        let mut instrs = self.instructions.try_lock().unwrap();
        for instr in &mut *instrs {
            instr.exec();
        }
    }

    pub fn push_instr(&self, instr: Box<dyn Instr>) {
        let mut instrs = self.instructions.try_lock().unwrap();
        (*instrs).push(instr);
    }

    pub fn result_var(&self) -> Option<Var> {
        let instrs = self.instructions.try_lock().unwrap();
        instrs.last().map(|instr| instr.result_var().clone())
    }

    pub fn result<T: IType + Clone + 'static>(&self) -> Option<T> {
        self.result_var().and_then(|v| v.value())
    }

    pub fn result_as_sexp(&self) -> Expr {
        let instrs = self.instructions.try_lock().unwrap();
        match instrs.last() {
            None => nil(),
            Some(instr) => instr.result_as_sexp(),
        }
    }
}

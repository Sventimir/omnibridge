use super::Instr;
use crate::{program::Program, var::Var, Expr, IntoSexp};
use std::fmt::{self, Debug, Formatter};

struct Not {
    arg: Var,
    result: Var,
}

impl Debug for Not {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Not({:?} -> {:?})", self.arg, self.result)
    }
}

impl Instr for Not {
    fn exec(&mut self) {
        let mut ret = self.result.lock();
        *ret = Box::new(!self.arg.value::<bool>().unwrap());
    }

    fn result_var(&self) -> Var {
        self.result.clone()
    }

    fn result_as_sexp(&self) -> Expr {
        self.result.value::<bool>().unwrap().into_sexp()
    }
}

pub fn not(prog: &mut Program, arg: Var) -> (Box<dyn Instr>, Var) {
    let result = prog.alloc(false);
    let instr = Not {
        arg,
        result: result.clone(),
    };
    (Box::new(instr), result)
}

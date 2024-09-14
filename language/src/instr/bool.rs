use super::Instr;
use crate::{var::Var, Expr, IntoSexp};

struct Not {
    arg: Var<bool>,
    result: Var<bool>,
}

impl Instr for Not {
    fn exec(&mut self) {
        let mut ret = self.result.lock();
        *ret = !*self.arg.lock();
    }

    fn result_as_sexp(&self) -> Expr {
        self.result.lock().into_sexp()
    }
}

pub fn not(arg: Var<bool>) -> (Box<dyn Instr>, Var<bool>) {
    let result = Var::new(Default::default());
    let instr = Not {
        arg,
        result: result.clone(),
    };
    (Box::new(instr), result)
}

struct Binary {
    args: [Var<bool>; 2],
    result: Var<bool>,
    op: fn(bool, bool) -> bool,
}

impl Instr for Binary {
    fn exec(&mut self) {
        let mut ret = self.result.lock();
        *ret = (self.op)(*self.args[0].lock(), *self.args[1].lock());
    }

    fn result_as_sexp(&self) -> Expr {
        self.result.lock().into_sexp()
    }
}

pub fn and(left: Var<bool>, right: Var<bool>) -> (Box<dyn Instr>, Var<bool>) {
    let result = Var::new(Default::default());
    let instr = Binary {
        args: [left, right],
        result: result.clone(),
        op: |l, r| l && r,
    };
    (Box::new(instr), result)
}

pub fn or(left: Var<bool>, right: Var<bool>) -> (Box<dyn Instr>, Var<bool>) {
    let result = Var::new(Default::default());
    let instr = Binary {
        args: [left, right],
        result: result.clone(),
        op: |l, r| l | r,
    };
    (Box::new(instr), result)
}

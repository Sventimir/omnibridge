use super::Instr;
use crate::{var::Var, Expr, IntoSexp};

struct Not {
    arg: Var,
    result: Var,
}

impl Instr for Not {
    fn exec(&mut self) {
        let mut ret = self.result.lock();
        *ret = Box::new(!self.arg.value::<bool>().unwrap());
    }

    fn result_as_sexp(&self) -> Expr {
        self.result.value::<bool>().unwrap().into_sexp()
    }
}

pub fn not(arg: Var) -> (Box<dyn Instr>, Var) {
    let result = Var::new(false);
    let instr = Not {
        arg,
        result: result.clone(),
    };
    (Box::new(instr), result)
}

struct Binary {
    args: [Var; 2],
    result: Var,
    op: fn(bool, bool) -> bool,
}

impl Instr for Binary {
    fn exec(&mut self) {
        let mut ret = self.result.lock();
        *ret = Box::new((self.op)(
            self.args[0].value::<bool>().unwrap(),
            self.args[1].value::<bool>().unwrap()
        ));
    }

    fn result_as_sexp(&self) -> Expr {
        self.result.value::<bool>().unwrap().into_sexp()
    }
}

pub fn and(left: Var, right: Var) -> (Box<dyn Instr>, Var) {
    let result = Var::new(false);
    let instr = Binary {
        args: [left, right],
        result: result.clone(),
        op: |l, r| l && r,
    };
    (Box::new(instr), result)
}

pub fn or(left: Var, right: Var) -> (Box<dyn Instr>, Var) {
    let result = Var::new(false);
    let instr = Binary {
        args: [left, right],
        result: result.clone(),
        op: |l, r| l | r,
    };
    (Box::new(instr), result)
}

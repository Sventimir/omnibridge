use super::Instr;
use crate::{program::Program, typed::IType, var::Var, Expr, IntoSexp};
use std::fmt::{self, Debug, Formatter};

struct BinaryOp<A, B, C> {
    args: [Var; 2],
    result: Var,
    op: fn(A, B) -> C,
    symbol: String,
}

impl<A: Debug, B: Debug, C: Debug> Debug for BinaryOp<A, B, C> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "({:?} {} {:?} -> {:?})",
            self.args[0], self.symbol, self.args[1], self.result
        )
    }
}

impl<A, B, C> Instr for BinaryOp<A, B, C>
where
    A: Clone + Debug + IType + 'static,
    B: Clone + Debug + IType + 'static,
    C: Clone + Debug + IType + IntoSexp + 'static,
{
    fn exec(&mut self) {
        let mut ret = self.result.lock();
        *ret = Box::new((self.op)(
            self.args[0].value::<A>().unwrap(),
            self.args[1].value::<B>().unwrap(),
        ));
    }

    fn result_var(&self) -> Var {
        self.result.clone()
    }

    fn result_as_sexp(&self) -> Expr {
        self.result.value::<C>().unwrap().into_sexp()
    }
}

pub fn and(prog: &mut Program, left: Var, right: Var) -> (Box<dyn Instr>, Var) {
    let result = prog.alloc(false);
    let instr = BinaryOp {
        args: [left, right],
        result: result.clone(),
        op: |l, r| l && r,
        symbol: "&&".to_string(),
    };
    (Box::new(instr), result)
}

pub fn or(prog: &mut Program, left: Var, right: Var) -> (Box<dyn Instr>, Var) {
    let result = prog.alloc(false);
    let instr = BinaryOp {
        args: [left, right],
        result: result.clone(),
        op: |l, r| l || r,
        symbol: "||".to_string(),
    };
    (Box::new(instr), result)
}

pub fn equal(prog: &mut Program, left: Var, right: Var) -> (Box<dyn Instr>, Var) {
    let result = prog.alloc(false);
    let instr: BinaryOp<f64, f64, bool> = BinaryOp {
        args: [left, right],
        result: result.clone(),
        op: |l, r| l == r,
        symbol: "=".to_string(),
    };
    (Box::new(instr), result)
}

pub fn add(prog: &mut Program, left: Var, right: Var) -> (Box<dyn Instr>, Var) {
    let result = prog.alloc(0.0);
    let instr: BinaryOp<f64, f64, f64> = BinaryOp {
        args: [left, right],
        result: result.clone(),
        op: |l, r| l + r,
        symbol: "+".to_string(),
    };
    (Box::new(instr), result)
}

pub fn mul(prog: &mut Program, left: Var, right: Var) -> (Box<dyn Instr>, Var) {
    let result = prog.alloc(0.0);
    let instr: BinaryOp<f64, f64, f64> = BinaryOp {
        args: [left, right],
        result: result.clone(),
        op: |l, r| l * r,
        symbol: "*".to_string(),
    };
    (Box::new(instr), result)
}

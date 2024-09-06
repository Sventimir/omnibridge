use super::Instr;
use crate::{typed::Bool, var::Var};

pub struct Not {
    arg: Var<Bool>,
    result: Var<Bool>,
}

impl Instr for Not {
    fn exec(&mut self) {
        let mut ret = self.result.lock();
        *ret = !*self.arg.lock();
    }
}

pub fn not(arg: Var<Bool>) -> (Box<dyn Instr>, Var<Bool>) {
    let result = Var::new(Bool, Default::default());
    let instr = Not {
        arg,
        result: result.clone(),
    };
    (Box::new(instr), result)
}

pub struct Binary {
    args: [Var<Bool>; 2],
    result: Var<Bool>,
    op: fn(bool, bool) -> bool,
}

impl Instr for Binary {
    fn exec(&mut self) {
        let mut ret = self.result.lock();
        *ret = (self.op)(*self.args[0].lock(), *self.args[1].lock());
    }
}

pub fn and(left: Var<Bool>, right: Var<Bool>) -> (Box<dyn Instr>, Var<Bool>) {
    let result = Var::new(Bool, Default::default());
    let instr = Binary {
        args: [left, right],
        result: result.clone(),
        op: |l, r| l && r,
    };
    (Box::new(instr), result)
}

pub fn or(left: Var<Bool>, right: Var<Bool>) -> (Box<dyn Instr>, Var<Bool>) {
    let result = Var::new(Bool, Default::default());
    let instr = Binary {
        args: [left, right],
        result: result.clone(),
        op: |l, r| l | r,
    };
    (Box::new(instr), result)
}

use crate::Expr;

pub enum NextStep {
    Forward,
    JumpTo(usize),
    Return,
    AddReturn,
}

struct Interpreter<V: Clone> {
    stack: Vec<V>,
    returns: Vec<usize>,
    cursor: usize,
}

impl<V: Clone> Interpreter<V> {
    fn new() -> Self {
        Interpreter {
            stack: Vec::new(),
            returns: Vec::new(),
            cursor: 0,
        }
    }

    fn exec<I>(&mut self, instr: &I)
    where
        I: Instr<Value = V>,
    {
        let args = self.stack.split_off(self.stack.len() - instr.arity());
        let (result, advance) = instr.eval(args.as_slice());
        match advance {
            NextStep::Forward => self.cursor += 1,
            NextStep::JumpTo(n) => self.cursor = n,
            NextStep::Return => self.cursor = self.returns.pop().unwrap(),
            NextStep::AddReturn => {
                self.returns.push(self.cursor);
                self.cursor += 1;
            }
        }
        match result {
            Some(v) => self.stack.push(v),
            None => (),
        }
    }
}

pub trait Instr {
    type Value: Clone;

    fn arity(&self) -> usize;
    fn eval(&self, args: &[Self::Value]) -> (Option<Self::Value>, NextStep);

    fn push_nat(v: u64) -> Self;
    fn push_int(v: i64) -> Self;
    fn push_float(v: f64) -> Self;
    fn push_str(v: String) -> Self;
    fn push_sexp(v: Expr) -> Self;
}

pub fn execute<I, V>(program: &[I]) -> Vec<V>
where
    V: Clone,
    I: Instr<Value = V>,
{
    let mut interp = Interpreter::new();
    while interp.cursor < program.len() {
        interp.exec(&program[interp.cursor]);
    }
    interp.stack.clone()
}

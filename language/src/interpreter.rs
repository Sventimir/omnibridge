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

    fn exec(&mut self, instr: &dyn Instr<Value = V>) {
        let args = self.stack.split_off(self.stack.len() - instr.arity());
        let (result, advance) = instr.eval(&args);
        match advance {
            NextStep::Forward => self.cursor += 1,
            NextStep::JumpTo(n) => self.cursor = n,
            NextStep::Return => self.cursor = self.returns.pop().unwrap(),
            NextStep::AddReturn => {
                self.returns.push(self.cursor);
                self.cursor += 1;
            }
        }
        self.stack.push(result);
    }
}

pub trait Instr {
    type Value: Clone;
    fn arity(&self) -> usize;
    fn eval(&self, args: &[Self::Value]) -> (Self::Value, NextStep);
}

pub fn execute<I, V>(program: &[I]) -> Vec<V>
where V: Clone,
      I: Instr<Value = V> 
{
    let mut interp = Interpreter::new();
    while interp.cursor < program.len() {
        interp.exec(&program[interp.cursor]);
    }
    interp.stack.clone()
}

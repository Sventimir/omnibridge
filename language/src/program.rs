

pub struct Program<'a> {
    instructions: Vec<Instr<'a>>,
}

pub enum Instr<'a> {
    Not { arg: &'a u8, result: u8 },
}

impl<'a> Program<'a> {
    pub fn new() -> Self {
        Program {
            instructions: Vec::new(),
        }
    }

    pub fn exec(&mut self) {
        for instr in &mut self.instructions {
            instr.exec();
        }
        self.result().unwrap();
    }

    pub fn push_instr(&mut self, instr: Instr<'a>) {
        self.instructions.push(instr);
    }

    pub fn result(&mut self) -> Option<&u8> {
        self.instructions.last().map(Instr::result)
    }
}

impl<'a> Instr<'a> {
    pub fn exec(&mut self) {
        match self {
            Instr::Not { arg, ref mut result } => {
                *result = !*arg;
            }
        }
    }

    fn result(&self) -> &u8 {
        match self {
            Instr::Not { result, .. } => result,
        }
    }
}

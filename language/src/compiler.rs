use std::collections::HashMap;

use crate::{ast::AST, program::Program};


struct Value;

pub fn compile<M>(src: &Vec<AST<M>>) -> Program {
    let mut prog = Program::new();
    let mut env: HashMap<String, Value> = HashMap::new();
    for ast in src {
        compile_ast(ast, &mut prog, &mut env);
    }
    prog
}

fn compile_ast<M>(ast: &AST<M>, prog: &mut Program, env: &mut HashMap<String, Value>) -> Value {
    match ast {
        AST::List { content, meta } => {
            Value
        }
        _ => Value
}
}

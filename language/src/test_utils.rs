use std::{any::Any, sync::Arc};

use crate::{
    IntoSexp, Program, ast::AST, builtin_instr::BuiltinInstr, builtin_type::BuiltinType, compile, env::Env, parse, source_meta::Meta, type_var::VarLabeler
};

pub fn bool_prog(instr: Vec<BuiltinInstr>) -> Program<BuiltinType, BuiltinInstr> {
    Program { instr, ret: BuiltinType::Bool }
}


pub fn get_result<T: Clone + 'static>(stack: &[Arc<dyn Any>]) -> T {
    stack
        .last()
        .expect("stack is empty!")
        .downcast_ref::<T>()
        .expect("cast failed!")
        .clone()
}

pub fn exec<T: Any + Clone>(src: &str) -> T {
    println!("src: '{}'", src);
    let mut ast: Vec<AST<Meta>> = parse(src).unwrap();
    let mut env: Env<BuiltinType, BuiltinInstr> = Env::new();
    env.init();
    match compile(&mut ast, &mut env) {
        Ok(prog) => {
            println!("{:?}", prog.clone().into_sexp::<String>());
            let stack = prog.execute();
            get_result::<T>(stack.as_slice())
        }
        Err(e) => {
            let mut labeler = VarLabeler::new();
            ast[0].label_type_vars(&mut labeler);
            println!("{}", ast[0].clone().into_sexp_debug::<String>());
            println!("{}", e.into_sexp::<String>());
            panic!("Failed to compile the program!")
        }
    }
}

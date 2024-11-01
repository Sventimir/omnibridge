use std::collections::HashMap;

use crate::{instr, program::Program, typed::Type, var::Var};

#[derive(Debug)]
pub struct Value {
    ty: Type,
    constr: fn(prog: &mut Program, args: &[Var]) -> Var,
}

impl Value {
    pub fn compile_var(&self, prog: &mut Program) -> Var {
        (self.constr)(prog, &[])
    }

    pub fn compile_func(&self, prog: &mut Program, args: &[Var]) -> Var {
        (self.constr)(prog, args)
    }
}

pub struct Env {
    vars: HashMap<String, Value>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            vars: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.vars.get(name)
    }

    pub fn ty(&self, name: &str) -> Option<Type> {
        self.get(name).map(|v| v.ty.clone())
    }

    pub fn initialize(&mut self) {
        self.vars.insert(
            "t".to_string(),
            Value {
                ty: Type::Bool,
                constr: |prog, _| prog.alloc(true),
            },
        );
        self.vars.insert(
            "f".to_string(),
            Value {
                ty: Type::Bool,
                constr: |prog, _| prog.alloc(false),
            },
        );
        self.vars.insert(
            "nil".to_string(),
            Value {
                ty: Type::Nil,
                constr: |prog, _| prog.alloc(()),
            },
        );
        self.vars.insert(
            "not".to_string(),
            Value {
                ty: Type::Func(vec![Type::Bool], Box::new(Type::Bool)),
                constr: |prog, args| {
                    let (instr, ret) = instr::bool::not(args[0].clone());
                    prog.push_instr(instr);
                    ret
                },
            },
        );
        self.vars.insert(
            "and".to_string(),
            Value {
                ty: Type::Func(vec![Type::Bool, Type::Bool], Box::new(Type::Bool)),
                constr: |prog, args| {
                    let (instr, ret) = instr::bool::and(args[0].clone(), args[1].clone());
                    prog.push_instr(instr);
                    ret
                },
            },
        );
        self.vars.insert(
            "or".to_string(),
            Value {
                ty: Type::Func(vec![Type::Bool, Type::Bool], Box::new(Type::Bool)),
                constr: |prog, args| {
                    let (instr, ret) = instr::bool::or(args[0].clone(), args[1].clone());
                    prog.push_instr(instr);
                    ret
                },
            },
        );
    }
}

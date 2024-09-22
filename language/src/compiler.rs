use core::f64;
use std::collections::HashMap;

use crate::{ast::AST, instr, program::Program, typed::Type, var::Var, Expr, IntoSexp};

#[derive(Debug)]
pub struct Value {
    ty: Type,
    constr: fn(prog: &mut Program, args: &[Var]) -> Var,
}

impl Value {
    fn compile_var(&self, prog: &mut Program) -> Var {
        (self.constr)(prog, &[])
    }

    fn compile_func(&self, prog: &mut Program, args: &[Var]) -> Var {
        (self.constr)(prog, args)
    }
}

pub fn initialize_env() -> HashMap<String, Value> {
    let mut env = HashMap::new();
    env.insert(
        "t".to_string(),
        Value {
            ty: Type::Bool,
            constr: |prog, _| prog.alloc(true),
        },
    );
    env.insert(
        "f".to_string(),
        Value {
            ty: Type::Bool,
            constr: |prog, _| prog.alloc(false),
        },
    );
    env.insert(
        "nil".to_string(),
        Value {
            ty: Type::Nil,
            constr: |prog, _| prog.alloc(()),
        },
    );
    env.insert(
        "NaN".to_string(),
        Value {
            ty: Type::Number,
            constr: |prog, _| prog.alloc(f64::NAN),
        },
    );
    env.insert(
        "inf".to_string(),
        Value {
            ty: Type::Number,
            constr: |prog, _| prog.alloc(f64::INFINITY),
        },
    );
    env.insert(
        "-inf".to_string(),
        Value {
            ty: Type::Number,
            constr: |prog, _| prog.alloc(f64::NEG_INFINITY),
        },
    );
    env.insert(
        "not".to_string(),
        Value {
            ty: Type::Func(vec![Type::Bool], Box::new(Type::Bool)),
            constr: |prog, args| {
                let (instr, ret) = instr::bool::not(prog, args[0].clone());
                prog.push_instr(instr);
                ret
            },
        },
    );
    env.insert(
        "and".to_string(),
        Value {
            ty: Type::Func(vec![Type::Bool, Type::Bool], Box::new(Type::Bool)),
            constr: |prog, args| {
                let (instr, ret) = instr::binop::and(prog, args[0].clone(), args[1].clone());
                prog.push_instr(instr);
                ret
            },
        },
    );
    env.insert(
        "or".to_string(),
        Value {
            ty: Type::Func(vec![Type::Bool, Type::Bool], Box::new(Type::Bool)),
            constr: |prog, args| {
                let (instr, ret) = instr::binop::or(prog, args[0].clone(), args[1].clone());
                prog.push_instr(instr);
                ret
            },
        },
    );
    env.insert(
        "=".to_string(),
        Value {
            ty: Type::Func(vec![Type::Number, Type::Number], Box::new(Type::Bool)),
            constr: |prog, args| {
                let (instr, ret) = instr::binop::equal(prog, args[0].clone(), args[1].clone());
                prog.push_instr(instr);
                ret
            },
        },
    );
    env.insert(
        "+".to_string(),
        Value {
            ty: Type::Func(vec![Type::Number, Type::Number], Box::new(Type::Number)),
            constr: |prog, args| {
                let (instr, ret) = instr::binop::add(prog, args[0].clone(), args[1].clone());
                prog.push_instr(instr);
                ret
            },
        },
    );
    env.insert(
        "*".to_string(),
        Value {
            ty: Type::Func(vec![Type::Number, Type::Number], Box::new(Type::Number)),
            constr: |prog, args| {
                let (instr, ret) = instr::binop::mul(prog, args[0].clone(), args[1].clone());
                prog.push_instr(instr);
                ret
            },
        },
    );
    env
}

pub fn compile<M: Clone + Typed>(src: &mut Vec<AST<M>>) -> Result<Program, TypeError<M>> {
    let mut prog = Program::new();
    let mut env: HashMap<String, Value> = initialize_env();
    for ast in src {
        let ty = typecheck(ast, &env)?;
        prog.assign_type(ty.clone());
        compile_ast(ast, &mut prog, &mut env);
    }
    Ok(prog)
}

fn compile_ast<M: Clone + Typed>(
    ast: &mut AST<M>,
    prog: &mut Program,
    env: &mut HashMap<String, Value>,
) -> Var {
    match ast {
        AST::Nat { content, .. } => prog.alloc(*content as f64),
        AST::Float { content, .. } => prog.alloc(*content),
        AST::String { content, .. } => prog.alloc(content.clone()),
        AST::Quoted { content, .. } | AST::QuasiQuoted { content, .. } => {
            prog.alloc(content.clone().into_sexp::<Expr>())
        }
        AST::Unquoted { content, .. } => compile_ast(content, prog, env),
        AST::Symbol { content, .. } => {
            let v = env.get(content).unwrap(); // this has already been verified by the type checker.
            v.compile_var(prog)
        }
        AST::List { content, .. } => {
            let mut cs = content.iter_mut();
            if let Some(func_ast) = cs.next() {
                match func_ast {
                    AST::Symbol { content, .. } => {
                        let args: Vec<Var> = cs.map(|arg| compile_ast(arg, prog, env)).collect();
                        let v = env.get(content).unwrap();
                        v.compile_func(prog, args.as_slice())
                    }
                    _ => panic!("Typechecker didn't catch a typing error!"),
                }
            } else {
                prog.alloc(())
            }
        }
    }
}

pub fn typecheck<M: Clone + Typed>(
    ast: &mut AST<M>,
    env: &HashMap<String, Value>,
) -> Result<Type, TypeError<M>> {
    let ty = match ast {
        AST::Nat { .. } | AST::Float { .. } => Ok(Type::Number),
        AST::String { .. } => Ok(Type::String),
        AST::Quoted { .. } => Ok(Type::Expr),
        AST::QuasiQuoted { meta, .. } | AST::Unquoted { meta, .. } => {
            Err(TypeError::UnexpectedQuasiquote(meta.clone()))
        }
        AST::Symbol { content, meta } => {
            let v = env.get(content).ok_or(TypeError::UnknownSymbol {
                symbol: content.clone(),
                meta: meta.clone(),
            })?;
            Ok(v.ty.clone())
        }
        AST::List {
            content,
            ref mut meta,
        } => {
            let mut cs = content.iter_mut();
            if let Some(ast) = cs.next() {
                match typecheck(ast, env)? {
                    Type::Func(args, ret) => {
                        typecheck_func_args(&mut cs, &mut args.iter(), env, meta)?;
                        Ok((*ret).clone())
                    }
                    ty => Err(TypeError::Mismatch {
                        expected: Type::Func(vec![], Box::new(Type::Expr)),
                        actual: ty,
                        meta: meta.clone(),
                    }),
                }
            } else {
                Ok(Type::Nil)
            }
        }
    }?;
    ast.meta_mut().assign_type(ty.clone());
    Ok(ty)
}

fn typecheck_func_args<M: Clone + Typed>(
    actual: &mut std::slice::IterMut<AST<M>>,
    expected: &mut std::slice::Iter<Type>,
    env: &HashMap<String, Value>,
    func_meta: &mut M,
) -> Result<(), TypeError<M>> {
    match (actual.next(), expected.next()) {
        (None, None) => Ok(()),
        (Some(ast), Some(ty)) => {
            let ty2 = typecheck(ast, env)?;
            if *ty != ty2 {
                Err(TypeError::Mismatch {
                    expected: ty.clone(),
                    actual: ty2,
                    meta: ast.meta().clone(),
                })
            } else {
                typecheck_func_args(actual, expected, env, func_meta)
            }
        }
        (Some(ast), None) => Err(TypeError::ExcessArg(ast.meta().clone())),
        (None, Some(exp)) => Err(TypeError::MissingArg {
            expected: exp.clone(),
            meta: func_meta.clone(),
        }),
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError<M> {
    UnexpectedQuasiquote(M),
    UnknownSymbol {
        symbol: String,
        meta: M,
    },
    MissingArg {
        expected: Type,
        meta: M,
    },
    ExcessArg(M),
    Mismatch {
        expected: Type,
        actual: Type,
        meta: M,
    },
}

pub trait Typed {
    fn assign_type(&mut self, ty: Type);
    fn typ(&self) -> Type;
}

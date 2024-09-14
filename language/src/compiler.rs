use std::collections::HashMap;

use crate::{ast::AST, program::Program, typed::Type};

pub struct Value {
    ty: Type,
}

pub fn initialize_env() -> HashMap<String, Value> {
    let mut env = HashMap::new();
    env.insert("t".to_string(), Value { ty: Type::Bool });
    env.insert("f".to_string(), Value { ty: Type::Bool });
    env.insert("nil".to_string(), Value { ty: Type::Nil });
    env.insert(
        "and".to_string(),
        Value {
            ty: Type::Func(vec![Type::Bool, Type::Bool], Box::new(Type::Bool)),
        },
    );
    env
}

pub fn compile<M>(src: &Vec<AST<M>>) -> Program {
    let mut prog = Program::new();
    let mut env: HashMap<String, Value> = HashMap::new();
    for ast in src {
        compile_ast(ast, &mut prog, &mut env);
    }
    prog
}

fn compile_ast<M>(ast: &AST<M>, prog: &mut Program, env: &mut HashMap<String, Value>) -> Value {
    todo!()
}

pub fn typecheck<M: Clone>(
    ast: &AST<M>,
    env: &HashMap<String, Value>,
) -> Result<Type, TypeError<M>> {
    match ast {
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
        AST::List { content, meta } => {
            let mut cs = content.iter();
            if let Some(ast) = cs.next() {
                match typecheck(ast, env)? {
                    Type::Func(args, ret) => {
                        typecheck_func_args(&mut cs, &mut args.iter(), env, &meta)?;
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
    }
}

fn typecheck_func_args<M: Clone>(
    actual: &mut std::slice::Iter<AST<M>>,
    expected: &mut std::slice::Iter<Type>,
    env: &HashMap<String, Value>,
    func_meta: &M,
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

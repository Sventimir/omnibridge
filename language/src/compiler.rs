use crate::{
    ast::AST,
    env::Env,
    program::Program,
    typed::{Type, TypeConstr, TypePrimitive},
    var::Var,
    Expr, IntoSexp,
};

pub fn compile<M: Clone + Typed>(src: &mut Vec<AST<M>>) -> Result<Program, TypeError<M>> {
    let mut prog = Program::new();
    let mut env = Env::new();
    env.initialize();
    for ast in src {
        let ty = typecheck(ast, &env)?;
        prog.assign_type(ty.clone());
        compile_ast(ast, &mut prog, &mut env);
    }
    Ok(prog)
}

fn compile_ast<M: Clone + Typed>(ast: &mut AST<M>, prog: &mut Program, env: &mut Env) -> Var {
    match ast {
        AST::Nat { content, .. } => prog.alloc(*content as f64),
        AST::Int { content, .. } => prog.alloc(*content as f64),
        AST::Float { content, .. } => prog.alloc(*content),
        AST::String { content, .. } => prog.alloc(content.clone()),
        AST::Quoted { content, .. } | AST::QuasiQuoted { content, .. } => {
            prog.alloc(content.clone().into_sexp::<Expr>())
        }
        AST::Unquoted { content, .. } => compile_ast(content, prog, env),
        AST::Symbol { content, .. } => {
            let v = env.get(content).unwrap(); // this has already been verified by the type checker.
            (v.constr)(prog, &[])
        }
        AST::List { content, .. } => {
            let mut cs = content.iter_mut();
            if let Some(func_ast) = cs.next() {
                match func_ast {
                    AST::Symbol { content, .. } => {
                        let args: Vec<Var> = cs.map(|arg| compile_ast(arg, prog, env)).collect();
                        let v = env.get(content).unwrap();
                        (v.constr)(prog, args.as_slice())
                    }
                    _ => panic!("Typechecker didn't catch a typing error!"),
                }
            } else {
                prog.alloc(())
            }
        }
    }
}

pub fn typecheck<M: Clone + Typed>(ast: &mut AST<M>, env: &Env) -> Result<Type, TypeError<M>> {
    let ty = match ast {
        AST::Nat { .. } | AST::Int { .. } | AST::Float { .. } => {
            Ok(Type(TypeConstr::Prim(TypePrimitive::Decimal)))
        }
        AST::String { .. } => Ok(Type(TypeConstr::Prim(TypePrimitive::String))),
        AST::Quoted { .. } => Ok(Type(TypeConstr::Prim(TypePrimitive::Expr))),
        AST::QuasiQuoted { meta, .. } | AST::Unquoted { meta, .. } => {
            Err(TypeError::UnexpectedQuasiquote(meta.clone()))
        }
        AST::Symbol { content, meta } => {
            let ty = env.ty(content).ok_or(TypeError::UnknownSymbol {
                symbol: content.clone(),
                meta: meta.clone(),
            })?;
            Ok(ty)
        }
        AST::List {
            content,
            ref mut meta,
        } => {
            let mut cs = content.iter_mut();
            if let Some(ast) = cs.next() {
                let t = typecheck(ast, env)?;
                match t.0 {
                    TypeConstr::Func(args, ret) => {
                        typecheck_func_args(&mut cs, &mut args.iter(), env, meta)?;
                        Ok((*ret).clone())
                    }
                    ty => Err(TypeError::Mismatch {
                        expected: Type(TypeConstr::Func(
                            vec![],
                            Box::new(Type(TypeConstr::Prim(TypePrimitive::Expr))),
                        )),
                        actual: Type(ty),
                        meta: meta.clone(),
                    }),
                }
            } else {
                Ok(Type(TypeConstr::Prim(TypePrimitive::Nil)))
            }
        }
    }?;
    ast.meta_mut().assign_type(ty.clone());
    Ok(ty)
}

fn typecheck_func_args<M: Clone + Typed>(
    actual: &mut std::slice::IterMut<AST<M>>,
    expected: &mut std::slice::Iter<Type>,
    env: &Env,
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

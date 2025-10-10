use std::fmt::{self, Debug, Formatter};

use crate::{
    ast::AST,
    env::Env,
    interpreter::Instr,
    pair,
    type_checker::{typecheck, Environment},
    type_error::TypeError,
    type_var::{PrimType, TypedMeta, VarLabeler},
    Expr, IntoSexp, Sexp,
};

#[derive(Clone)]
pub enum CompilationError<M, T> {
    TypeError(TypeError<M, T>),
    UnresolvedTypeVar(M),
}

impl<M, T> CompilationError<M, T>
where
    T: Clone,
    M: TypedMeta,
{
    fn label_type_vars(&self) {
        let mut labeler = VarLabeler::new();
        match self {
            CompilationError::TypeError(e) => e.label_type_vars(&mut labeler),
            CompilationError::UnresolvedTypeVar(m) => m.get_type().label_type_vars(&mut labeler),
        }
    }
}

impl<M, T> IntoSexp for CompilationError<M, T>
where
    M: Clone + TypedMeta + IntoSexp,
    T: Clone + IntoSexp,
{
    fn into_sexp<S: Sexp>(self) -> S {
        self.label_type_vars();
        match self {
            CompilationError::TypeError(e) => e.into_sexp(),
            CompilationError::UnresolvedTypeVar(m) => {
                pair(S::symbol("unresolved-type-var".to_string()), m.into_sexp())
            }
        }
    }
}

impl<M, T> Debug for CompilationError<M, T>
where
    M: Clone + TypedMeta + IntoSexp,
    T: Clone + IntoSexp,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "CompilationError: {}",
            self.clone().into_sexp::<String>()
        )
    }
}

pub fn compile<M, I, T>(
    src: &mut Vec<AST<M>>,
    env: &mut Env<T, I>,
) -> Result<Vec<I>, CompilationError<M, T>>
where
    M: TypedMeta<Type = T> + Clone,
    I: Instr,
    T: PrimType + Ord + Clone,
{
    let mut prog = Vec::new();
    for ast in src {
        // typechecking annotates the AST with type information.
        typecheck(ast, env).map_err(CompilationError::TypeError)?;
        compile_ast(ast, &mut prog, env)?;
    }
    Ok(prog)
}

fn compile_ast<E, M, I, T>(
    ast: &AST<M>,
    prog: &mut Vec<I>,
    env: &mut E,
) -> Result<(), CompilationError<M, T>>
where
    E: Environment<T, I>,
    M: TypedMeta<Type = T> + Clone,
    I: Instr,
    T: PrimType + Clone,
{
    match ast {
        AST::Nat { content, meta } => {
            let t = meta.get_type();
            let ins = env.get_instr("from-nat", &t).unwrap();
            prog.push(I::push_nat(*content));
            prog.extend(ins);
            Ok(())
        }
        AST::Int { content, meta } => {
            let t = meta.get_type();
            let ins = env.get_instr("from-int", &t).unwrap();
            prog.push(I::push_int(*content));
            prog.extend(ins);
            Ok(())
        }
        AST::Float { content, meta } => {
            let t = meta.get_type();
            let ins = env.get_instr("from-float", &t).unwrap();
            prog.push(I::push_float(*content));
            prog.extend(ins);
            Ok(())
        }
        AST::String { content, .. } => Ok(prog.push(I::push_str(content.clone()))),
        AST::Quoted { content, .. } | AST::QuasiQuoted { content, .. } => {
            Ok(prog.push(I::push_sexp(content.clone().into_sexp::<Expr>())))
        }
        AST::Unquoted { content, .. } => compile_ast(content, prog, env),
        AST::Symbol { content, meta } => {
            let t = meta.get_type();
            let ins = env.get_instr(content, &t).unwrap();
            prog.extend(ins);
            Ok(())
        }
        AST::List { content, .. } => {
            for ast in content.iter().rev() {
                compile_ast(ast, prog, env)?;
            }
            Ok(())
        }
    }
}

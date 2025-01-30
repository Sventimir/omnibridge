use std::fmt::{self, Debug, Formatter};

use crate::{
    ast::AST, env::Env, interpreter::Instr, pair, type_checker::{typecheck, Environment, Typed}, type_error::TypeError, type_var::PrimType, Expr, IntoSexp, Sexp
};

#[derive(Clone)]
pub enum CompilationError<M, T> {
    TypeError(TypeError<M, T>),
    UnresolvedTypeVar(M),
}

impl<M, T> CompilationError<M, T>
where M: Typed
{
    fn label_type_vars(&self) {
        let mut label_index: u8 = 'a' as u8;
        match self {
            CompilationError::TypeError(e) => e.label_type_vars(Some(&mut label_index)),
            CompilationError::UnresolvedTypeVar(m) => {
                m.get_type().label(&mut label_index)
            },
        }
    }

}

impl<M, T> IntoSexp for CompilationError<M, T>
where
    M: Clone + Typed + IntoSexp,
    T: Clone + IntoSexp,
{
    fn into_sexp<S: Sexp>(self) -> S {
        self.label_type_vars();
        match self {
            CompilationError::TypeError(e) => e.into_sexp(),
            CompilationError::UnresolvedTypeVar(m) => 
                pair(S::symbol("unresolved-type-var".to_string()), m.into_sexp()),
        }
    }
}

impl<M, T> Debug for CompilationError<M, T>
where
    M: Clone + Typed + IntoSexp,
    T: Clone + IntoSexp,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "CompilationError: {}", self.clone().into_sexp::<String>())    }
}

pub fn compile<M, I, T>(
    src: &mut Vec<AST<M>>,
    env: &mut Env<T, I>,
) -> Result<Vec<I>, CompilationError<M, T>>
where
    M: Typed<Type = T> + Clone,
    I: Instr,
    T: PrimType + Clone,
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
    M: Typed<Type = T> + Clone,
    I: Instr,
    T: PrimType + Clone,
{
    match ast {
        AST::Nat { content, .. } => Ok(prog.push(I::push_int(*content as i64))),
        AST::Int { content, .. } => Ok(prog.push(I::push_int(*content))),
        AST::Float { content, .. } => Ok(prog.push(I::push_float(*content))),
        AST::String { content, .. } => Ok(prog.push(I::push_str(content.clone()))),
        AST::Quoted { content, .. } | AST::QuasiQuoted { content, .. } => {
            Ok(prog.push(I::push_sexp(content.clone().into_sexp::<Expr>())))
        }
        AST::Unquoted { content, .. } => compile_ast(content, prog, env),
        AST::Symbol { content, meta } => {
            let t = meta
                .get_type()
                .value()
                .ok_or(CompilationError::UnresolvedTypeVar(meta.clone()))?;
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

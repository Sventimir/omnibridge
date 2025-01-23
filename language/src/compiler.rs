use crate::{
    ast::AST,
    env::Env,
    interpreter::Instr,
    type_checker::{typecheck, Environment, Typed},
    type_error::TypeError,
    type_var::PrimType,
    Expr, IntoSexp,
};

#[derive(Debug)]
pub enum CompilationError<M, T> {
    TypeError(TypeError<M, T>),
    UnresolvedTypeVar(M),
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
        AST::Nat { content, .. } => Ok(prog.push(I::push(*content))),
        AST::Int { content, .. } => Ok(prog.push(I::push(*content))),
        AST::Float { content, .. } => Ok(prog.push(I::push(*content))),
        AST::String { content, .. } => Ok(prog.push(I::push(content.clone()))),
        AST::Quoted { content, .. } | AST::QuasiQuoted { content, .. } => {
            Ok(prog.push(I::push(content.clone().into_sexp::<Expr>())))
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

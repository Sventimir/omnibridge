use crate::{
    ast::AST,
    type_error::TypeError,
    type_var::{PrimType, TypeEnv, TypeExpr, TypedMeta},
};

pub trait Environment<T, I> {
    fn type_of(&self, name: &str) -> Option<TypeExpr<T>>;
    fn get_instr(&self, name: &str, ty: &TypeExpr<T>) -> Option<Vec<I>>;

    fn poly_float(&self) -> TypeExpr<T>;
}

fn assign_const_and_return<M, T>(meta: &mut M, ty: T) -> TypeExpr<T>
where
    M: TypedMeta<Type = T>,
    T: PrimType,
{
    let v = TypeExpr::constant(ty);
    let refv = v.make_ref();
    meta.assign_type(v);
    refv
}

pub fn typecheck<E, M, I, T>(ast: &mut AST<M>, env: &E) -> Result<TypeExpr<T>, TypeError<M, T>>
where
    E: Environment<T, I> + TypeEnv<T>,
    M: Clone + TypedMeta<Type = T>,
    T: Clone + PrimType,
{
    let t = typecheck_ast(ast, env)?;
    select_default_instances(ast, env)?;
    Ok(t)
}

pub fn typecheck_ast<E, M, I, T>(ast: &mut AST<M>, env: &E) -> Result<TypeExpr<T>, TypeError<M, T>>
where
    E: Environment<T, I> + TypeEnv<T>,
    M: Clone + TypedMeta<Type = T>,
    T: Clone + PrimType,
{
    match ast {
        AST::Nat { ref mut meta, .. } => Ok(assign_const_and_return(meta, T::int())),
        AST::Int { ref mut meta, .. } => Ok(assign_const_and_return(meta, T::int())),
        AST::Float { ref mut meta, .. } => {
            let t = env.poly_float();
            meta.assign_type(t.make_ref());
            Ok(t)
        }
        AST::String { ref mut meta, .. } => Ok(assign_const_and_return(meta, T::string())),
        AST::Symbol { content, meta } => match env.type_of(content) {
            Some(t) => {
                meta.assign_type(t.make_ref());
                Ok(t)
            }
            None => Err(TypeError::Undefined {
                symbol: content.clone(),
                meta: meta.clone(),
            }),
        },
        AST::Quoted { ref mut meta, .. } => Ok(assign_const_and_return(meta, T::sexp())),
        AST::QuasiQuoted { meta, .. } | AST::Unquoted { meta, .. } => {
            Err(TypeError::UnexpectedQuasiquote { meta: meta.clone() })
        }
        AST::List {
            content,
            ref mut meta,
        } => {
            let mut cs = content.iter_mut();
            match cs.next() {
                None => Ok(assign_const_and_return(meta, T::nil())),
                Some(mut call_ast) => {
                    let fun_ty = typecheck(&mut call_ast, env)?;
                    let mut vars: Vec<_> = fun_ty.vars.iter().map(|v| v.make_ref()).collect();
                    let mut act_arg_tys = vec![];
                    for mut arg_ast in cs {
                        let t = typecheck(&mut arg_ast, env)?;
                        act_arg_tys.push(t);
                    }
                    let (exp_arg_tys, ret_ty) = fun_ty.body.as_callable(act_arg_tys.len(), meta)?;
                    for (act, exp) in act_arg_tys.iter().zip(exp_arg_tys.iter()) {
                        exp.unify(&act.body, env, meta)?;
                        for v in act.vars.iter() {
                            vars.push(v.make_ref());
                        }
                    }
                    Ok(TypeExpr { body: ret_ty, vars: vars })
                }
            }
        }
    }
}

fn select_default_instances<E, M, I, T>(ast: &mut AST<M>, env: &E) -> Result<(), TypeError<M, T>>
where
    E: Environment<T, I> + TypeEnv<T>,
    M: Clone + TypedMeta<Type = T>,
    T: Clone + PrimType,
{
    match ast {
        AST::Nat { ref mut meta, .. }
        | AST::Int { ref mut meta, .. }
        | AST::Float { ref mut meta, .. }
        | AST::String { ref mut meta, .. }
        | AST::Symbol { ref mut meta, .. }
        | AST::Quoted { ref mut meta, .. }
        | AST::QuasiQuoted { ref mut meta, .. }
        | AST::Unquoted { ref mut meta, .. } => ensure_type_selected(meta, env),
        AST::List {
            content,
            ref mut meta,
        } => {
            ensure_type_selected(meta, env)?;
            for sub_ast in content.iter_mut() {
                select_default_instances(sub_ast, env)?;
            }
            Ok(())
        }
    }
}

fn ensure_type_selected<E, M, I, T>(meta: &mut M, env: &E) -> Result<(), TypeError<M, T>>
where
    E: Environment<T, I> + TypeEnv<T>,
    M: Clone + TypedMeta<Type = T>,
    T: Clone + PrimType,
{
    for var in meta.get_type().vars.iter() {
        if let None = var.value() {
            env.set_default_type(var, meta)?;
        }
    }
    Ok(())
}

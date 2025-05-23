use crate::{
    ast::AST,
    type_error::TypeError,
    type_var::{PrimType, TypeEnv, TypeExpr, TypeVar, TypedMeta},
};

pub trait Environment<T, I> {
    fn type_of(&self, name: &str) -> Option<TypeExpr<T>>;
    fn get_instr(&self, name: &str, ty: &TypeExpr<T>) -> Option<Vec<I>>;
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
    match ast {
        AST::Nat { ref mut meta, .. } => Ok(assign_const_and_return(meta, T::int())),
        AST::Int { ref mut meta, .. } => Ok(assign_const_and_return(meta, T::int())),
        AST::Float { ref mut meta, .. } => Ok(assign_const_and_return(meta, T::float())),
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
                    let arg_tys = cs
                        .map(|arg_ast| typecheck(arg_ast, env))
                        .collect::<Result<Vec<_>, _>>()?;
                    let ret_ty = TypeVar::unknown(vec![]);
                    let mut arg_bodys: Vec<TypeVar<T>> = vec![];
                    let mut arg_vars: Vec<TypeVar<T>> = vec![];
                    for expr in arg_tys.iter() {
                        arg_bodys.push(expr.body.make_ref());
                        arg_vars.extend_from_slice(&expr.vars);
                    }
                    let expected_ty = TypeExpr {
                        body: TypeVar::constant(T::fun(arg_bodys.as_slice(), ret_ty.make_ref())),
                        vars: arg_vars,
                    };
                    fun_ty.unify(&expected_ty, env, meta)?;
                    let ret_expr = TypeExpr {
                        body: ret_ty.make_ref(),
                        vars: vec![ret_ty.make_ref()],
                    };
                    meta.assign_type(ret_expr.make_ref());
                    Ok(ret_expr)
                }
            }
        }
    }
}

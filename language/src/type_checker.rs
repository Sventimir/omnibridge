use crate::{
    ast::AST,
    type_error::TypeError,
    type_var::{PrimType, TypeVar},
};

pub trait Typed {
    type Type;

    fn assign_type(&mut self, ty: TypeVar<Self::Type>);
    fn get_type(&self) -> TypeVar<Self::Type>;

    fn label_type_vars(&self, label_index: Option<&mut u8>) {
        match label_index {
            Some(index) => self.get_type().label(index),
            None => {
                let mut label_index: u8 = 'a' as u8;
                self.get_type().label(&mut label_index);
            }
        }
    }
}

pub trait Environment<T, I> {
    fn type_of(&self, name: &str) -> Option<TypeVar<T>>;
    fn get_instr(&self, name: &str, ty: &T) -> Option<Vec<I>>;
}

fn assign_const_and_return<M, T>(meta: &mut M, ty: T) -> TypeVar<T>
where
    M: Typed<Type = T>,
    T: PrimType,
{
    let v = TypeVar::constant(ty);
    let refv = v.make_ref();
    meta.assign_type(v);
    refv
}

pub fn typecheck<E, M, I, T>(ast: &mut AST<M>, env: &E) -> Result<TypeVar<T>, TypeError<M, T>>
where
    E: Environment<T, I>,
    M: Clone + Typed<Type = T>,
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
                    let ret_ty = TypeVar::unknown();
                    let expected_ty =
                        TypeVar::constant(T::fun(arg_tys.as_slice(), ret_ty.make_ref()));
                    fun_ty.unify(&expected_ty, meta.clone())?;
                    meta.assign_type(ret_ty.make_ref());
                    Ok(ret_ty)
                }
            }
        }
    }
}

use std::fmt::{self, Debug, Formatter};

use crate::{
    builtin_type::BuiltinType,
    type_error::TypeError,
    type_var::{TypeEnv, TypeExpr, TypeVar, TypedMeta},
    IntoSexp, Sexp,
};

#[derive(Clone)]
struct Meta(TypeExpr<BuiltinType>);

impl Debug for Meta {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl IntoSexp for Meta {
    fn into_sexp<S: Sexp>(self) -> S {
        self.0.into_sexp()
    }
}

impl TypedMeta for Meta {
    type Type = BuiltinType;
    fn get_type(&self) -> TypeExpr<BuiltinType> {
        self.0.clone()
    }

    fn assign_type(&mut self, ty: TypeExpr<BuiltinType>) {
        self.0 = ty;
    }
}

impl TypeEnv<BuiltinType> for () {
    fn set_default_type<M>(
        &self,
        var: &TypeVar<BuiltinType>,
        _meta: &mut M,
    ) -> Result<(), TypeError<M, BuiltinType>>
    where
        M: Clone + TypedMeta<Type = BuiltinType>,
    {
        var.set_val(BuiltinType::Nil);
        Ok(())
    }

    fn check_constraint<M: Clone>(
        &self,
        _name: &str,
        _t: &BuiltinType,
        _meta: &M,
    ) -> Result<(), TypeError<M, BuiltinType>> {
        Ok(())
    }
}

#[test]
fn unification_propagates() {
    let num = TypeVar::constant(BuiltinType::Nat);
    let a: TypeVar<BuiltinType> = TypeVar::unknown(vec![]);
    let b: TypeVar<BuiltinType> = TypeVar::unknown(vec![]);
    b.unify(
        &a,
        &(),
        &Meta(TypeExpr {
            body: b.make_ref(),
            vars: vec![b.make_ref()],
        }),
    )
    .expect("B unified successfully with A.");
    assert_eq!(a.value(), None);
    assert_eq!(b.value(), None);
    a.unify(
        &num,
        &(),
        &Meta(TypeExpr {
            body: a.make_ref(),
            vars: vec![a.make_ref()],
        }),
    )
    .expect("A unified successfully with Num.");
    assert_eq!(a.value(), Some(BuiltinType::Nat));
    assert_eq!(b.value(), Some(BuiltinType::Nat));
}

#[test]
fn setting_value_propagates() {
    let a = TypeVar::unknown(vec![]);
    let b = a.make_ref();
    a.set_val(BuiltinType::Nat);
    assert_eq!(a.value(), Some(BuiltinType::Nat));
    assert_eq!(b.value(), Some(BuiltinType::Nat));
}

use std::fmt::{self, Debug, Formatter};

use crate::{
    builtin_type::BuiltinType,
    type_checker::Typed,
    type_error::TypeError,
    type_var::{TypeEnv, TypeVar},
    IntoSexp, Sexp,
};

#[derive(Clone)]
struct Meta(TypeVar<BuiltinType>);

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

impl Typed for Meta {
    type Type = BuiltinType;
    fn get_type(&self) -> TypeVar<BuiltinType> {
        self.0.clone()
    }

    fn assign_type(&mut self, ty: TypeVar<BuiltinType>) {
        self.0 = ty;
    }
}

impl TypeEnv<BuiltinType> for () {
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
    let a: TypeVar<BuiltinType> = TypeVar::unknown(&[]);
    let b: TypeVar<BuiltinType> = TypeVar::unknown(&[]);
    b.unify(&a, &(), &Meta(b.make_ref()))
        .expect("B unified successfully with A.");
    assert_eq!(a.value(), None);
    assert_eq!(b.value(), None);
    a.unify(&num, &(), &Meta(a.make_ref()))
        .expect("A unified successfully with Num.");
    assert_eq!(a.value(), Some(BuiltinType::Nat));
    assert_eq!(b.value(), Some(BuiltinType::Nat));
}

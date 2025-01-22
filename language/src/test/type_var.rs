use crate::{builtin_type::BuiltinType, type_var::TypeVar};

#[test]
fn unification_propagates() {
    let num = TypeVar::constant(BuiltinType::Nat);
    let a: TypeVar<BuiltinType> = TypeVar::unknown();
    let b: TypeVar<BuiltinType> = TypeVar::unknown();
    b.unify(&a, ()).expect("B unified successfully with A.");
    assert_eq!(a.value(), None);
    assert_eq!(b.value(), None);
    a.unify(&num, ()).expect("A unified successfully with Num.");
    assert_eq!(a.value(), Some(BuiltinType::Nat));
    assert_eq!(b.value(), Some(BuiltinType::Nat));
}

use crate::type_var::TypeVar;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Type {
    Num,
    Bool,
}

#[test]
fn unification_propagates() {
    let num = TypeVar::constant(Type::Num);
    let a: TypeVar<Type> = TypeVar::unknown();
    let b: TypeVar<Type> = TypeVar::unknown();
    b.unify(&a, ()).expect("B unified successfully with A.");
    assert_eq!(a.value(), None);
    assert_eq!(b.value(), None);
    a.unify(&num, ()).expect("A unified successfully with Num.");
    assert_eq!(a.value(), Some(Type::Num));
    assert_eq!(b.value(), Some(Type::Num));
}

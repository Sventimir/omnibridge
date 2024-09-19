use crate::{
    ast::AST,
    compiler::{self, TypeError},
    parse,
    test_utils::{Meta, exec},
    typed::Type,
    IntoSexp,
};

#[test]
fn typecheck_a_bool_expr() {
    let src = "(and t f)";
    let env = compiler::initialize_env();
    let mut ast: Vec<AST<Meta>> = parse(&src).unwrap();
    let ty: Result<Type, TypeError<Meta>> = compiler::typecheck(&mut ast[0], &env);
    assert_eq!(ty, Ok(Type::Bool));
}

quickcheck! {
    fn a_simple_function_call(a: bool, b: bool) -> bool {
        let result = exec(
            &format!(
                "(and {} {})",
                a.into_sexp::<String>(),
                b.into_sexp::<String>()
            )
        );
        result.value() == Some(a && b)
    }

    fn test_de_morgan_equivalence(a: bool, b: bool) -> bool {
        let result1 = exec(
            &format!(
                "(not (or {} {}))",
                a.into_sexp::<String>(),
                b.into_sexp::<String>(),
            )
        );
        let result2 = exec(
            &format!(
                "(and (not {}) (not {}))",
                a.into_sexp::<String>(),
                b.into_sexp::<String>(),
            )
        );
        result1.value::<bool>() == result2.value()
    }
}

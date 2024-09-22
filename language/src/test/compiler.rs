use crate::{
    ast::AST,
    compiler::{self, TypeError},
    parse,
    test_utils::{exec, Meta},
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

    fn test_multiplication_distribution_over_addition(a: f64, b: f64, c: f64) -> bool {
        if a.is_nan() || b.is_nan() || c.is_nan() {
            return true;
        }
        let result = exec(
            &format!(
                "(= (* {} (+ {} {})) (+ (* {} {}) (* {} {})))",
                a.into_sexp::<String>(),
                b.into_sexp::<String>(),
                c.into_sexp::<String>(),
                a.into_sexp::<String>(),
                b.into_sexp::<String>(),
                a.into_sexp::<String>(),
                c.into_sexp::<String>(),
            )
        );
        println!("Program returned: {}.", result.value::<bool>().unwrap());
        result.value::<bool>() == Some(true)
    }
}

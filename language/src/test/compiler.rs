use std::fmt::{self, Display, Formatter};

use proptest::prelude::Strategy;
use proptest_derive::Arbitrary;

use crate::{
    ast::AST, builtin_type::BuiltinType, env::Env, parse, test_utils::Meta,
    type_checker::typecheck, IntoSexp, Sexp,
};

fn test_typechecker<F>(src: &str, check_result: F)
where
    F: FnOnce(BuiltinType) -> Result<(), String>,
{
    let mut env: Env<BuiltinType> = Env::new();
    let mut ast: Vec<AST<Meta>> = parse(&src).unwrap();
    env.init();
    let ret: Result<(), String> = typecheck(&mut ast[0], &env)
        .map_err(|e| e.into_sexp())
        .and_then(|tvar| {
            tvar.value()
                .ok_or(String::symbol("unresolved-type-var".to_string()))
        })
        .and_then(check_result);
    match ret {
        Ok(()) => (),
        Err(e) => {
            println!("AST:{:?}", ast[0]);
            panic!("typecheck failed with error: {}", e)
        },
    }
}

#[test]
fn typecheck_a_bool_expr() {
    test_typechecker("(and t f)", |ty| match ty {
        BuiltinType::Bool => Ok(()),
        _ => Err(String::list(vec![
            String::symbol("result-type-mismatch".to_string()),
            String::symbol("bool".to_string()),
            String::symbol(ty.into_sexp()),
        ])),
    })
}

#[test]
fn typecheck_a_polymorphic_function_call() {
    test_typechecker("(id (+ 3.0 (* 2.0 (id 7.0))))", |ty| match ty {
        BuiltinType::Float => Ok(()),
        _ => Err(String::list(vec![
            String::symbol("result-type-mismatch".to_string()),
            String::symbol("float".to_string()),
            String::symbol(ty.into_sexp()),
        ])),
    })
}

/* This property is not true in general with respect to floating-point
numbers. Things may go wrong when some computation hits infitinity
or because of rounding errors. In general the order of operations
matters with floats. Here we try to select a subset of values for
which the property holds. Later we might switch to a more reliable
type. */
fn integral_float() -> impl Strategy<Value = f64> {
    (i16::MIN..i16::MAX).prop_map(|x| x as f64)
}

#[derive(Arbitrary, Debug, PartialEq)]
enum AnyNumber {
    Nat(u64),
    Int(i64),
    Float(f64),
}

impl Display for AnyNumber {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            AnyNumber::Nat(n) => write!(f, "{}", n),
            AnyNumber::Int(n) => write!(f, "{}", n),
            AnyNumber::Float(n) => write!(f, "{}", n),
        }
    }
}

proptest! {
    #[test]
    fn a_simple_function_call(a: bool, b: bool) {
        let result = crate::test_utils::old::exec(
            &format!(
                "(and {} {})",
                a.into_sexp::<String>(),
                b.into_sexp::<String>()
            )
        );
        assert_eq!(result.value(), Some(a && b))
    }

    #[test]
    fn test_de_morgan_equivalence(a: bool, b: bool) {
        let result1 = crate::test_utils::old::exec(
            &format!(
                "(not (or {} {}))",
                a.into_sexp::<String>(),
                b.into_sexp::<String>(),
            )
        );
        let result2 = crate::test_utils::old::exec(
            &format!(
                "(and (not {}) (not {}))",
                a.into_sexp::<String>(),
                b.into_sexp::<String>(),
            )
        );
        assert_eq!(result1.value::<bool>(), result2.value())
    }

    #[test]
    fn test_multiplication_distribution_over_addition(
        a in integral_float(),
        b in integral_float(),
        c in integral_float()
    ) {
        let result = crate::test_utils::old::exec(
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
        assert_eq!(result.value::<bool>(), Some(true))
    }

    #[ignore]
    #[test]
    fn test_polymorhic_identity(any_num: AnyNumber) {
        match any_num {
            AnyNumber::Nat(n) => {
                let result = crate::test_utils::old::exec(&format!("(id {})", n));
                assert_eq!(result.value::<u64>(), Some(n))
            }
            AnyNumber::Int(i) => {
                let result = crate::test_utils::old::exec(&format!("(id {})", i));
                assert_eq!(result.value::<i64>(), Some(i))
            }
            AnyNumber::Float(f) => {
                let result = crate::test_utils::old::exec(&format!("(id {})", f));
                assert_eq!(result.value::<f64>(), Some(f))
            }
        }
    }
}

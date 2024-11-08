use std::fmt::{self, Display, Formatter};

use proptest::prelude::Strategy;
use proptest_derive::Arbitrary;

use crate::{
    ast::AST,
    compiler::{self, TypeError},
    env::Env,
    parse,
    test_utils::{exec, Meta},
    typed::{Type, TypeConstr},
    IntoSexp,
};

#[test]
fn typecheck_a_bool_expr() {
    let src = "(and t f)";
    let mut env = Env::new();
    let mut ast: Vec<AST<Meta>> = parse(&src).unwrap();
    env.initialize();
    let ty: Result<Type, TypeError<Meta>> = compiler::typecheck(&mut ast[0], &env);
    assert_eq!(ty, Ok(Type(TypeConstr::Bool)));
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
        let result = exec(
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
        assert_eq!(result1.value::<bool>(), result2.value())
    }

    #[test]
    fn test_multiplication_distribution_over_addition(
        a in integral_float(),
        b in integral_float(),
        c in integral_float()
    ) {
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
        assert_eq!(result.value::<bool>(), Some(true))
    }

    #[ignore]
    #[test]
    fn test_polymorhic_identity(any_num: AnyNumber) {
        match any_num {
            AnyNumber::Nat(n) => {
                let result = exec(&format!("(id {})", n));
                assert_eq!(result.value::<u64>(), Some(n))
            }
            AnyNumber::Int(i) => {
                let result = exec(&format!("(id {})", i));
                assert_eq!(result.value::<i64>(), Some(i))
            }
            AnyNumber::Float(f) => {
                let result = exec(&format!("(id {})", f));
                assert_eq!(result.value::<f64>(), Some(f))
            }
        }
    }
}

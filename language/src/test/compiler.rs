use std::ops::Range;

use crate::{
    ast::AST, compile, compiler::{self, TypeError, Typed}, env::Env, parse, program::Program, src_location::WithLocation, typed::Type, var::Var, IntoSexp
};

#[derive(Clone, Debug, PartialEq, Eq)]
struct Meta {
    loc: Range<usize>,
    ty: Type,
}

impl Typed for Meta {
    fn typ(&self) -> Type {
        self.ty.clone()
    }

    fn assign_type(&mut self, ty: Type) {
        self.ty = ty;
    }
}

impl WithLocation for Meta {
    type Loc = Range<usize>;

    fn get_location(&self) -> Range<usize> {
        self.loc.clone()
    }

    fn annot(&mut self, loc: Range<usize>) {
        self.loc = loc;
    }
}

impl Default for Meta {
    fn default() -> Self {
        Meta {
            loc: 0..0,
            ty: Type::Nil,
        }
    }
}

#[test]
fn typecheck_a_bool_expr() {
    let src = "(and t f)";
    let mut env = Env::new();
    let mut ast: Vec<AST<Meta>> = parse(&src).unwrap();
    env.initialize();
    let ty: Result<Type, TypeError<Meta>> = compiler::typecheck(&mut ast[0], &env);
    assert_eq!(ty, Ok(Type::Bool));
}

fn exec(src: &str) -> Var {
    let mut ast: Vec<AST<Meta>> = parse(src).unwrap();
    let prog: Program = compile(&mut ast).unwrap();
    prog.exec();
    prog.result_var().unwrap()
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

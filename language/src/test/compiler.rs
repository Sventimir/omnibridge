use std::ops::Range;

use crate::{
    ast::AST,
    compile,
    compiler::{self, TypeError, Typed},
    parse,
    program::Program,
    src_location::WithLocation,
    typed::Type,
    IntoSexp,
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
    let env = compiler::initialize_env();
    let mut ast: Vec<AST<Meta>> = parse(&src).unwrap();
    let ty: Result<Type, TypeError<Meta>> = compiler::typecheck(&mut ast[0], &env);
    assert_eq!(ty, Ok(Type::Bool));
}

quickcheck! {
    fn a_simple_function_call(a: bool, b: bool) -> bool {
        let src = format!(
            "(and {} {})",
            a.into_sexp::<String>(),
            b.into_sexp::<String>()
        );
        let mut ast: Vec<AST<Meta>> = parse(&src).unwrap();
        let prog: Program = compile(&mut ast).unwrap();
        prog.exec();
        println!("{:?}", prog);
        prog.result() == Some(a && b)
    }
}

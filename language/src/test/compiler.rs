use std::{collections::HashMap, ops::Range};

use crate::{
    ast::AST,
    compiler::{self, TypeError},
    parse,
    program::Program,
    typed::Type,
    IntoSexp,
};

#[test]
fn typecheck_a_bool_expr() {
    let src = "(and t f)";
    let env = compiler::initialize_env();
    let ast: Vec<AST<Range<usize>>> = parse(&src).unwrap();
    let ty: Result<Type, TypeError<Range<usize>>> = compiler::typecheck(&ast[0], &env);
    assert_eq!(ty, Ok(Type::Bool));
}

// quickcheck! {
//     fn a_simple_function_call(a: bool, b: bool) -> bool {
//         let src = format!(
//             "(and {} {})",
//             a.into_sexp::<String>(),
//             b.into_sexp::<String>()
//         );
//         let ast: Vec<AST<Range<usize>>> = parse(&src).unwrap();
//         let prog: Program = compile(&ast);
//         prog.exec();
//         prog.result_as_sexp().try_into() == Ok(a && b)
//     }
// }

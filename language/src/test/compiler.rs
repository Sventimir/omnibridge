use std::ops::Range;

use crate::{ast::AST, compile, parse, program::Program};

#[test]
fn a_simple_function_call() {
    let src = "(and t f)";
    let ast: Vec<AST<Range<usize>>> = parse(src).unwrap();
    let prog: Program = compile(&ast);
    prog.exec();
    assert_eq!(prog.result_as_sexp().try_into(), Ok(false))
}

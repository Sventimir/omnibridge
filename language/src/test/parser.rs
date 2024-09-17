use std::ops::Range;

use crate::{
    ast::AST, nil, parser::ParseError, src_location::WithLocation, CoreLisp, IntoSexp, Lisp, Sexp,
};

type Meta = Range<usize>;

fn assert_success(res: &Result<Vec<AST<Meta>>, ParseError>) -> &AST<Meta> {
    match res {
        Ok(exprs) => match exprs.as_slice() {
            [sexpr] => sexpr,
            _ => panic!("Expected one sexpr, got: {:?}", exprs),
        },
        Err(e) => panic!(
            "Expected success, got error: {}",
            e.clone().into_sexp::<String>()
        ),
    }
}

#[test]
fn parse_nothing() {
    let result = crate::parse::<AST<Meta>>("");
    assert_eq!(result, Ok(vec![]));
}

#[test]
fn parse_nil() {
    let result = crate::parse::<AST<Meta>>("()");
    let sexpr = assert_success(&result);
    let mut nil: AST<Meta> = nil();
    nil.annot(0..2);
    assert_eq!(*sexpr, nil);
}

#[test]
fn parse_nil_symbol() {
    let result = crate::parse::<AST<Meta>>("nil");
    let sexpr = assert_success(&result);
    let mut nil: AST<Meta> = nil();
    nil.annot(0..3);
    assert_eq!(*sexpr, nil)
}

#[test]
fn parse_symbol() {
    let result = crate::parse::<AST<Meta>>("foo");
    let sexpr = assert_success(&result);
    let mut sym: AST<Meta> = AST::symbol("foo".to_string());
    sym.annot(0..3);
    assert_eq!(*sexpr, sym);
}

#[test]
fn parse_quoted_symbol() {
    let result = crate::parse::<AST<Meta>>("'foo");
    let sexpr = assert_success(&result);
    let mut sym: AST<Meta> = AST::symbol("foo".to_string());
    sym.annot(1..4);
    let mut quot = AST::quoted(sym);
    quot.annot(0..4);
    assert_eq!(*sexpr, quot)
}

#[test]
fn parse_nat() {
    let result = crate::parse::<AST<Meta>>("42");
    let sexpr = assert_success(&result);
    let mut nat: AST<Meta> = AST::nat(42);
    nat.annot(0..2);
    assert_eq!(*sexpr, nat);
}

#[test]
fn parse_float() {
    let result = crate::parse::<AST<Meta>>("42.32");
    let sexpr = assert_success(&result);
    let mut float: AST<Meta> = AST::float(42.32);
    float.annot(0..5);
    assert_eq!(*sexpr, float);
}

#[test]
fn parse_string() {
    let result = crate::parse::<AST<Meta>>("\"foo bar\"");
    let sexpr = assert_success(&result);
    let mut string: AST<Meta> = AST::string("foo bar".to_string());
    string.annot(0..9);
    assert_eq!(*sexpr, string);
}

#[test]
fn parse_list() {
    let result = crate::parse::<AST<Meta>>("(foo 42)");
    let sexpr = assert_success(&result);
    let mut string: AST<Meta> = AST::symbol("foo".to_string());
    string.annot(1..4);
    let mut nat: AST<Meta> = AST::nat(42);
    nat.annot(5..7);
    let mut list: AST<Meta> = AST::list(vec![string, nat]);
    list.annot(0..8);
    assert_eq!(*sexpr, list);
}

#[test]
fn parse_quoted_list() {
    let result = crate::parse::<AST<Meta>>("'(foo x y 42 z)");
    let sexpr = assert_success(&result);
    let mut foo: AST<Meta> = AST::symbol("foo".to_string());
    foo.annot(2..5);
    let mut x: AST<Meta> = AST::symbol("x".to_string());
    x.annot(6..7);
    let mut y: AST<Meta> = AST::symbol("y".to_string());
    y.annot(8..9);
    let mut nat: AST<Meta> = AST::nat(42);
    nat.annot(10..12);
    let mut z: AST<Meta> = AST::symbol("z".to_string());
    z.annot(13..14);
    let mut list = AST::list(vec![foo, x, y, nat, z]);
    list.annot(1..15);
    let mut quoted = AST::quoted(list);
    quoted.annot(0..15);
    assert_eq!(*sexpr, quoted);
}

#[test]
fn parse_nested_lists() {
    let result = crate::parse::<AST<Meta>>("(+ (* 3 4) (/ 5 7) (expt 2 3))");
    let sexpr = assert_success(&result);
    let mut plus: AST<Meta> = AST::symbol("+".to_string());
    plus.annot(1..2);
    let mut mult: AST<Meta> = AST::symbol("*".to_string());
    mult.annot(4..5);
    let mut div: AST<Meta> = AST::symbol("/".to_string());
    div.annot(12..13);
    let mut expt: AST<Meta> = AST::symbol("expt".to_string());
    expt.annot(20..24);
    let mut three: AST<Meta> = AST::nat(3);
    three.annot(6..7);
    let mut four: AST<Meta> = AST::nat(4);
    four.annot(8..9);
    let mut five: AST<Meta> = AST::nat(5);
    five.annot(14..15);
    let mut seven: AST<Meta> = AST::nat(7);
    seven.annot(16..17);
    let mut two: AST<Meta> = AST::nat(2);
    two.annot(25..26);
    let mut three2: AST<Meta> = AST::nat(3);
    three2.annot(27..28);
    let mut ex_list = AST::list(vec![expt, two, three2]);
    ex_list.annot(19..29);
    let mut mult_list = AST::list(vec![mult, three, four]);
    mult_list.annot(3..10);
    let mut div_list = AST::list(vec![div, five, seven]);
    div_list.annot(11..18);
    let mut list = AST::list(vec![plus, mult_list, div_list, ex_list]);
    list.annot(0..30);
    assert_eq!(*sexpr, list);
}

#[test]
fn parse_quasi_quoted_list() {
    let result = crate::parse::<AST<Meta>>("`(foo)");
    let sexpr = assert_success(&result);
    let mut sym: AST<Meta> = AST::symbol("foo".to_string());
    sym.annot(2..5);
    let mut list = AST::list(vec![sym]);
    list.annot(1..6);
    let mut quasiquot = AST::quasiquoted(list);
    quasiquot.annot(0..6);
    assert_eq!(*sexpr, quasiquot);
}

#[test]
fn parse_quasi_quoted_with_unquotes() {
    let result = crate::parse::<AST<Meta>>("(1 `(+ 1 ,one) 3)");
    let sexpr = assert_success(&result);
    let mut one1: AST<Meta> = AST::nat(1);
    one1.annot(1..2);
    let mut plus: AST<Meta> = AST::symbol("+".to_string());
    plus.annot(5..6);
    let mut one2: AST<Meta> = AST::nat(1);
    one2.annot(7..8);
    let mut one_sym: AST<Meta> = AST::symbol("one".to_string());
    one_sym.annot(10..13);
    let mut three: AST<Meta> = AST::nat(3);
    three.annot(15..16);
    let mut unquot = AST::unquoted(one_sym);
    unquot.annot(9..13);
    let mut plus_list = AST::list(vec![plus, one2, unquot]);
    plus_list.annot(4..14);
    let mut quasiquot = AST::quasiquoted(plus_list);
    quasiquot.annot(3..14);
    let mut list = AST::list(vec![one1, quasiquot, three]);
    list.annot(0..17);
    assert_eq!(*sexpr, list);
}

// I wonder, if this is correct code in Common Lisp.
// According to our parser it is.
#[test]
fn parse_unquote_outside_quasi_quote() {
    let result = crate::parse::<AST<Meta>>("(1 2 ,x ,y 5)");
    let expr = assert_success(&result);
    let mut one: AST<Meta> = AST::nat(1);
    one.annot(1..2);
    let mut two: AST<Meta> = AST::nat(2);
    two.annot(3..4);
    let mut x: AST<Meta> = AST::symbol("x".to_string());
    x.annot(6..7);
    let mut y: AST<Meta> = AST::symbol("y".to_string());
    y.annot(9..10);
    let mut five: AST<Meta> = AST::nat(5);
    five.annot(11..12);
    let mut x_unquot = AST::unquoted(x);
    x_unquot.annot(5..7);
    let mut y_unquot = AST::unquoted(y);
    y_unquot.annot(8..10);
    let mut list = AST::list(vec![one, two, x_unquot, y_unquot, five]);
    list.annot(0..13);
    assert_eq!(*expr, list);
}

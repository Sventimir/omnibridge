use crate::language::{self, ast::AST, nil, parser::ParseError, CoreLisp, IntoSexp, Lisp, Sexp};

fn assert_success(res: &Result<Vec<AST>, ParseError>) -> &AST {
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
    let result = language::parse::<AST>("");
    assert_eq!(result, Ok(vec![]));
}

#[test]
fn parse_nil() {
    let result = language::parse::<AST>("()");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, nil());
}

#[test]
fn parse_symbol() {
    let result = language::parse::<AST>("foo");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, AST::symbol("foo".to_string()));
}

#[test]
fn parse_quoted_symbol() {
    let result = language::parse::<AST>("'foo");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, AST::quoted(AST::symbol("foo".to_string())))
}

#[test]
fn parse_nat() {
    let result = language::parse::<AST>("42");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, AST::nat(42));
}

#[test]
fn dont_parse_negative_num_without_parens() {
    const INPUT: &'static str = "-42";
    let result = language::parse::<AST>(INPUT);
    assert_eq!(result, Err(ParseError::InvalidNumber(INPUT.to_string())));
}

#[test]
fn parse_float() {
    let result = language::parse::<AST>("42.32");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, AST::float(42.32));
}

#[test]
fn parse_string() {
    let result = language::parse::<AST>("\"foo bar\"");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, AST::string("foo bar".to_string()));
}

#[test]
fn parse_list() {
    let result = language::parse::<AST>("(foo 42)");
    let sexpr = assert_success(&result);
    assert_eq!(
        *sexpr,
        AST::list(vec![AST::symbol("foo".to_string()), AST::nat(42),])
    );
}

#[test]
fn parse_quoted_list() {
    let result = language::parse::<AST>("'(foo x y 42 z)");
    let sexpr = assert_success(&result);
    assert_eq!(
        *sexpr,
        AST::quoted(AST::list(vec![
            AST::symbol("foo".to_string()),
            AST::symbol("x".to_string()),
            AST::symbol("y".to_string()),
            AST::nat(42),
            AST::symbol("z".to_string()),
        ]))
    );
}

#[test]
fn parse_nested_lists() {
    let result = language::parse::<AST>("(+ (* 3 4) (/ 5 7) (expt 2 3))");
    let sexpr = assert_success(&result);
    assert_eq!(
        *sexpr,
        AST::list(vec![
            AST::symbol("+".to_string()),
            AST::list(vec![AST::symbol("*".to_string()), AST::nat(3), AST::nat(4),]),
            AST::list(vec![AST::symbol("/".to_string()), AST::nat(5), AST::nat(7),]),
            AST::list(vec![
                AST::symbol("expt".to_string()),
                AST::nat(2),
                AST::nat(3),
            ]),
        ])
    );
}

#[test]
fn parse_quasi_quoted_list() {
    let result = language::parse::<AST>("`(foo)");
    let sexpr = assert_success(&result);
    assert_eq!(
        *sexpr,
        AST::quasiquoted(AST::list(vec![AST::symbol("foo".to_string()),]))
    );
}

#[test]
fn parse_quasi_quoted_with_unquotes() {
    let result = language::parse::<AST>("(1 `(+ 1 ,one) 3)");
    let sexpr = assert_success(&result);
    assert_eq!(
        *sexpr,
        AST::list(vec![
            AST::nat(1),
            AST::quasiquoted(AST::list(vec![
                AST::symbol("+".to_string()),
                AST::nat(1),
                AST::unquoted(AST::symbol("one".to_string())),
            ])),
            AST::nat(3)
        ])
    )
}

// I wonder, if this is correct code in Common Lisp.
// According to our parser it is.
#[test]
fn parse_unquote_outside_quasi_quote() {
    let result = language::parse::<AST>("(1 2 ,x ,y 5)");
    let expr = assert_success(&result);
    assert_eq!(
        *expr,
        AST::list(vec![
            AST::nat(1),
            AST::nat(2),
            AST::unquoted(AST::symbol("x".to_string())),
            AST::unquoted(AST::symbol("y".to_string())),
            AST::nat(5),
        ])
    )
}

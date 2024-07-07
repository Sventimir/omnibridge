use crate::language::*;
use Sexpr::*;

fn assert_success(res: &Result<Sexpr, ParseError>) -> &Sexpr {
    match res {
        Ok(expr) => &expr,
        Err(e) => panic!("Expected success, got error: {}", e),
    }
}

#[test]
fn parse_nothing() {
    let result = parse("");
    assert_eq!(result, Err(ParseError::UnexpectedEndOfParsing));
}

#[test]
fn parse_nil() {
    let result = parse("()");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, NIL);
    
}

#[test]
fn parse_symbol() {
    let result = parse("foo");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, Symbol("foo".to_string()));
}

#[test]
fn parse_quoted_symbol() {
    let result = parse("'foo");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, quoted(Symbol("foo".to_string())))
}

#[test]
fn parse_nat() {
    let result = parse("42");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, Nat(42));
}

#[test]
fn dont_parse_negative_num_without_parens() {
    const INPUT: &'static str = "-42";
    let result = parse(INPUT);
    assert_eq!(result, Err(ParseError::InvalidNumber(INPUT.to_string())));
}

#[test]
fn parse_float() {
    let result = parse("42.32");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, Float(42.32));
}

#[test]
fn parse_string() {
    let result = parse("\"foo bar\"");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, Str("foo bar".to_string()));
}

#[test]
fn parse_list() {
    let result = parse("(foo 42)");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, list([
        Symbol("foo".to_string()),
        Nat(42),
    ]));
}

#[test]
fn parse_quoted_list() {
    let result = parse("'(foo x y 42 z)");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, quoted(list([
        Symbol("foo".to_string()),
        Symbol("x".to_string()),
        Symbol("y".to_string()),
        Nat(42),
        Symbol("z".to_string()),
    ])));
}

#[test]
fn parse_nested_lists() {
    let result = parse("(+ (* 3 4) (/ 5 7) (expt 2 3))");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, list([
        Symbol("+".to_string()),
        list([
            Symbol("*".to_string()),
            Nat(3),
            Nat(4),
        ]),
        list([
            Symbol("/".to_string()),
            Nat(5),
            Nat(7),
        ]),
        list([
            Symbol("expt".to_string()),
            Nat(2),
            Nat(3),
        ]),
    ]));
}

#[test]
fn parse_quasi_quoted_list() {
    let result = parse("`(foo)");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, quasi_quoted(list([
        Symbol("foo".to_string()),
    ])));
}

#[test]
fn parse_quasi_quoted_with_unquotes() {
    let result = parse("(1 `(+ 1 ,one) 3)");
    let sexpr = assert_success(&result);
    assert_eq!(*sexpr, list([
        Nat(1),
        quasi_quoted(list([
            Symbol("+".to_string()),
            Nat(1),
            unquoted(Symbol("one".to_string())),
        ])),
        Nat(3)
    ]))
}

// I wonder, if this is correct code in Common Lisp. 
// According to our parser it is.
#[test]
fn parse_unquote_outside_quasi_quote() {
    let result = parse("(1 2 ,x ,y 5)");
    let expr = assert_success(&result);
    assert_eq!(*expr, list([
        Nat(1),
        Nat(2),
        unquoted(symbol("x")),
        unquoted(symbol("y")),
        Nat(5),
    ]))
}

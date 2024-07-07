use super::sexpr::*;


pub trait ToSexp {
    fn to_sexp(&self) -> Sexpr;
}

pub trait FromSexp: Sized {
    type Error;
    fn from_sexp(sexp: &Sexpr) -> std::result::Result<Self, Self::Error>;
}

pub trait Sexpable: ToSexp + FromSexp {}

impl<T> Sexpable for T where T: ToSexp + FromSexp {}

impl ToSexp for ParseError {
    fn to_sexp(&self) -> Sexpr {
        match self {
            ParseError::SyntaxError => symbol("syntax-error"),
            ParseError::UtfError(_) => symbol("utf-error"),
            ParseError::UnexpectedNode(node) => list([symbol("unexpected-node"), string(node)]),
            ParseError::UnexpectedEndOfParsing => symbol("unexpected-end-of-parsing"),
            ParseError::InvalidNumber(num) => list([symbol("invalid-number"), string(num)]),
        }
    }
}

pub enum InterpretError {
    Unexpected {
        expected: &'static str,
        found: Sexpr,
    }
}

type Result<T> = std::result::Result<T, InterpretError>;

pub fn expect_list(sexp: &Sexpr) -> Result<&Vec<Sexpr>> {
    match sexp {
        Sexpr::List(list) => Ok(list),
        _ => Err(InterpretError::Unexpected {
            expected: "a list",
            found: sexp.clone()
        }),
    }
}

pub fn expect_symbol(sexp: &Sexpr) -> Result<&str> {
    match sexp {
        Sexpr::Symbol(s) => Ok(s),
        _ => Err(InterpretError::Unexpected {
            expected: "a symbol",
            found: sexp.clone()
        })
    }
}

pub fn expect_string(sexp: &Sexpr) -> Result<&str> {
    match sexp {
        Sexpr::Str(s) => Ok(s),
        _ => Err(InterpretError::Unexpected {
            expected: "a string",
            found: sexp.clone()
        })
    }
}

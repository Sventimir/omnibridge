use crate::language::IntoSexp;

use super::*;

#[derive(Debug, Clone)]
pub enum ExpectError {
    Symbol(AST),
    Nat(AST),
    Float(AST),
    String(AST),
    List(AST),
    WrongLength(usize, Vec<AST>),
    InvalidSymbol(String),
}

pub fn symbol(ast: &AST) -> Result<&str, ExpectError> {
    match ast {
        AST::Symbol(s) => Ok(&s),
        _ => Err(ExpectError::Symbol(ast.clone())),
    }
}

pub fn nat(ast: &AST) -> Result<u64, ExpectError> {
    match ast {
        AST::Nat(n) => Ok(*n),
        _ => Err(ExpectError::Nat(ast.clone())),
    }
}

pub fn float(ast: &AST) -> Result<f64, ExpectError> {
    match ast {
        AST::Float(f) => Ok(*f),
        _ => Err(ExpectError::Float(ast.clone())),
    }
}

pub fn string(ast: &AST) -> Result<&str, ExpectError> {
    match ast {
        AST::String(s) => Ok(&s),
        _ => Err(ExpectError::String(ast.clone())),
    }
}

pub fn list(ast: &AST) -> Result<&[AST], ExpectError> {
    match ast {
        AST::List(l) => Ok(l),
        _ => Err(ExpectError::List(ast.clone())),
    }
}

pub fn pair(ast: &AST) -> Result<(&AST, &AST), ExpectError> {
    match list(ast)? {
        [left, right] => Ok((&left, &right)),
        l => Err(ExpectError::WrongLength(2, l.to_vec())),
    }
}

pub fn nil(ast: &AST) -> Result<(), ExpectError> {
    let l = list(ast)?;
    if l.len() == 0 {
        Ok(())
    } else {
        Err(ExpectError::WrongLength(0, l.to_vec()))
    }
}

pub fn int(ast: &AST) -> Result<i64, ExpectError> {
    nat(ast).map(|n| n as i64).or_else(|_| {
        let (sign, n) = pair(ast)?;
        let s = symbol(sign)?;
        if s != "-" {
            return Err(ExpectError::InvalidSymbol(s.to_string()));
        }
        nat(n).map(|n| -(n as i64))
    })
}

pub fn optional<'a, F, S>(expect: &F, ast: &'a AST) -> Result<Option<S>, ExpectError>
where
    F: Fn(&'a AST) -> Result<S, ExpectError>,
{
    nil(ast).map(|_| None).or_else(|_| expect(ast).map(Some))
}

impl IntoSexp for ExpectError {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            ExpectError::Symbol(ast) => S::list(vec![
                S::symbol("expected-symbol".to_string()),
                S::string(format!("{:?}", ast)),
            ]),
            ExpectError::Nat(ast) => S::list(vec![
                S::symbol("expected-nat".to_string()),
                S::string(format!("{:?}", ast)),
            ]),
            ExpectError::Float(ast) => S::list(vec![
                S::symbol("expected-float".to_string()),
                S::string(format!("{:?}", ast)),
            ]),
            ExpectError::String(ast) => S::list(vec![
                S::symbol("expected-string".to_string()),
                S::string(format!("{:?}", ast)),
            ]),
            ExpectError::List(ast) => S::list(vec![
                S::symbol("expected-list".to_string()),
                S::string(format!("{:?}", ast)),
            ]),
            ExpectError::WrongLength(n, l) => S::list(vec![
                S::symbol("wrong-length".to_string()),
                S::nat(n as u64),
                l.into_sexp(),
            ]),
            ExpectError::InvalidSymbol(s) => {
                S::list(vec![S::symbol("invalid-symbol".to_string()), S::symbol(s)])
            }
        }
    }
}

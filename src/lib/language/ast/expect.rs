use crate::language::IntoSexp;
use std::fmt::Debug;

use super::*;

#[derive(Debug, Clone)]
pub enum ExpectError<M> {
    Symbol(AST<M>),
    Nat(AST<M>),
    Float(AST<M>),
    String(AST<M>),
    List(AST<M>),
    WrongLength(usize, Vec<AST<M>>),
    InvalidSymbol(String),
}

pub fn symbol<M: Clone>(ast: &AST<M>) -> Result<&str, ExpectError<M>> {
    match ast {
        AST::Symbol { content, .. } => Ok(&content),
        _ => Err(ExpectError::Symbol(ast.clone())),
    }
}

pub fn nat<M: Clone>(ast: &AST<M>) -> Result<u64, ExpectError<M>> {
    match ast {
        AST::Nat { content, .. } => Ok(*content),
        _ => Err(ExpectError::Nat(ast.clone())),
    }
}

pub fn float<M: Clone>(ast: &AST<M>) -> Result<f64, ExpectError<M>> {
    match ast {
        AST::Float { content, .. } => Ok(*content),
        _ => Err(ExpectError::Float(ast.clone())),
    }
}

pub fn string<M: Clone>(ast: &AST<M>) -> Result<&str, ExpectError<M>> {
    match ast {
        AST::String { content, .. } => Ok(&content),
        _ => Err(ExpectError::String(ast.clone())),
    }
}

pub fn list<M: Clone>(ast: &AST<M>) -> Result<&[AST<M>], ExpectError<M>> {
    match ast {
        AST::List { content, .. } => Ok(content),
        _ => Err(ExpectError::List(ast.clone())),
    }
}

pub fn pair<M: Clone>(ast: &AST<M>) -> Result<(&AST<M>, &AST<M>), ExpectError<M>> {
    match list(ast)? {
        [left, right] => Ok((&left, &right)),
        l => Err(ExpectError::WrongLength(2, l.to_vec())),
    }
}

pub fn nil<M: Clone>(ast: &AST<M>) -> Result<(), ExpectError<M>> {
    let l = list(ast)?;
    if l.len() == 0 {
        Ok(())
    } else {
        Err(ExpectError::WrongLength(0, l.to_vec()))
    }
}

pub fn int<M: Clone>(ast: &AST<M>) -> Result<i64, ExpectError<M>> {
    nat(ast).map(|n| n as i64).or_else(|_| {
        let (sign, n) = pair(ast)?;
        let s = symbol(sign)?;
        if s != "-" {
            return Err(ExpectError::InvalidSymbol(s.to_string()));
        }
        nat(n).map(|n| -(n as i64))
    })
}

pub fn optional<'a, F, S, M>(expect: &F, ast: &'a AST<M>) -> Result<Option<S>, ExpectError<M>>
where
    M: Clone,
    F: Fn(&'a AST<M>) -> Result<S, ExpectError<M>>,
{
    nil(ast).map(|_| None).or_else(|_| expect(ast).map(Some))
}

impl<M> IntoSexp for ExpectError<M>
where
    M: Debug,
{
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
                l.into_iter()
                    .map(AST::drop_meta)
                    .collect::<Vec<AST<()>>>()
                    .into_sexp(),
            ]),
            ExpectError::InvalidSymbol(s) => {
                S::list(vec![S::symbol("invalid-symbol".to_string()), S::symbol(s)])
            }
        }
    }
}

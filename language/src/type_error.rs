use std::fmt::{self, Debug, Display, Formatter};

use crate::{IntoSexp, Sexp};


#[derive(Clone)]
pub enum TypeError<M, T> {
    Mismatch {
        expected: T,
        found: T,
        meta: M,
    },
    Undefined {
        symbol: String,
        meta: M,
    },
    UnexpectedQuasiquote {
        meta: M,
    },
}

impl<M, T> Display for TypeError<M, T> 
where M: Display,
      T: Display
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TypeError::Mismatch {
                expected,
                found,
                meta,
            } => write!(
                f,
                "Type mismatch: expected {}, found {} at {}",
                expected, found, meta
            ),
            TypeError::Undefined { symbol, meta } => {
                write!(f, "Undefined symbol {} at {}", symbol, meta)
            }
            TypeError::UnexpectedQuasiquote { meta } => {
                write!(f, "Unexpected quasiquote at {}", meta)
            }
        }
    }
}

impl<M, T> Debug for TypeError<M, T> 
where T: Debug,
      M: Debug
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TypeError::Mismatch {
                expected,
                found,
                meta,
            } => write!(
                f,
                "Type mismatch: expected {:?}, found {:?} at {:?}",
                expected, found, meta
            ),
            TypeError::Undefined { symbol, meta } => {
                write!(f, "Undefined symbol {:?} at {:?}", symbol, meta)
            }
            TypeError::UnexpectedQuasiquote { meta } => {
                write!(f, "Unexpected quasiquote at {:?}", meta)
            }
        }
    }
}

impl<M, T> IntoSexp for TypeError<M, T>
    where M: IntoSexp,
          T: IntoSexp
{
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            TypeError::Mismatch { expected, found, meta} =>
                S::list(vec![
                    S::symbol("type-mismatch".to_string()),
                    expected.into_sexp(),
                    found.into_sexp(),
                    meta.into_sexp(),
                ]),
            TypeError::Undefined { symbol, meta } =>
                S::list(vec![
                    S::symbol("undefined-symbol".to_string()),
                    S::symbol(symbol),
                    meta.into_sexp(),
                ]),
            TypeError::UnexpectedQuasiquote { meta } =>
                S::list(vec![
                    S::symbol("unexpected-quasi-quote".to_string()),
                    meta.into_sexp(),
                ])
        }
    }
}

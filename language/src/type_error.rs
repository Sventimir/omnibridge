use std::fmt::{self, Debug, Display, Formatter};

use crate::{
    pair,
    src_location::{SrcLocation, WithLocation},
    type_checker::Typed,
    IntoSexp, Sexp,
};

#[derive(Clone)]
pub enum TypeError<M, T> {
    Mismatch { expected: T, found: T, meta: M },
    Undefined { symbol: String, meta: M },
    UnexpectedQuasiquote { meta: M },
}

impl<M, T> Display for TypeError<M, T>
where
    M: Display,
    T: Display,
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
where
    T: Clone + IntoSexp,
    M: Clone + Typed + IntoSexp,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "TypeError: {}", self.clone().into_sexp::<String>())
    }
}

impl<M, T> IntoSexp for TypeError<M, T>
where
    M: IntoSexp + Typed,
    T: IntoSexp,
{
    fn into_sexp<S: Sexp>(self) -> S {
        self.label_type_vars(None);
        match self {
            TypeError::Mismatch {
                expected,
                found,
                meta,
            } => S::list(vec![
                S::symbol("type-mismatch".to_string()),
                expected.into_sexp(),
                found.into_sexp(),
                meta.into_sexp(),
            ]),
            TypeError::Undefined { symbol, meta } => S::list(vec![
                S::symbol("undefined-symbol".to_string()),
                S::symbol(symbol),
                meta.into_sexp(),
            ]),
            TypeError::UnexpectedQuasiquote { meta } => S::list(vec![
                S::symbol("unexpected-quasi-quote".to_string()),
                meta.into_sexp(),
            ]),
        }
    }
}

fn named_value<S: Sexp, T: IntoSexp>(name: &str, val: T) -> S {
    S::list(vec![S::symbol(name.to_string()), val.into_sexp()])
}

fn location_sexp<M: WithLocation, S: Sexp>(meta: &M, src: &str) -> S {
    let loc = meta.get_location();
    S::list(vec![
        S::string(loc.find(src).to_string()),
        loc.byte_range().into_sexp(),
    ])
}

impl<M, T> TypeError<M, T>
where
    M: WithLocation,
    T: IntoSexp + Clone,
{
    pub fn error_message_sexp<S: Sexp>(&self, src: &str) -> S {
        let header = S::symbol("error".to_string());
        match self {
            TypeError::Mismatch {
                expected,
                found,
                meta,
            } => S::list(vec![
                header,
                S::symbol("type-mismatch".to_string()),
                named_value("expected", (*expected).clone()),
                named_value("found", (*found).clone()),
                location_sexp(meta, src),
            ]),
            TypeError::Undefined { symbol, meta } => S::list(vec![
                header,
                S::symbol("undefined-symbol".to_string()),
                pair(S::symbol("symbol".to_string()), S::symbol(symbol.clone())),
                location_sexp(meta, src),
            ]),
            TypeError::UnexpectedQuasiquote { meta } => S::list(vec![
                header,
                S::symbol("unexpected-quasiquote".to_string()),
                location_sexp(meta, src),
            ]),
        }
    }
}

impl<M, T> TypeError<M, T>
where
    M: Typed,
{
    pub fn label_type_vars(&self, label_index: Option<&mut u8>) {
        match self {
            TypeError::Mismatch { meta, .. } => {
                meta.label_type_vars(label_index);
            }
            TypeError::Undefined { meta, .. } => {
                meta.label_type_vars(label_index);
            }
            TypeError::UnexpectedQuasiquote { meta } => {
                meta.label_type_vars(label_index);
            }
        }
    }
}

use std::ops::Range;

use super::{src_location::WithLocation, CoreLisp, IntoSexp, Lisp, Sexp};

pub mod expect;

#[derive(Clone, Debug, PartialEq)]
pub enum AST<M> {
    Symbol { content: String, meta: M },
    String { content: String, meta: M },
    Nat { content: u64, meta: M },
    Float { content: f64, meta: M },
    List { content: Vec<AST<M>>, meta: M },
    Quoted { content: Box<AST<M>>, meta: M },
    QuasiQuoted { content: Box<AST<M>>, meta: M },
    Unquoted { content: Box<AST<M>>, meta: M },
}

impl<M> AST<M> {
    pub fn meta(&self) -> &M {
        match self {
            AST::Symbol { meta, .. } => meta,
            AST::String { meta, .. } => meta,
            AST::Nat { meta, .. } => meta,
            AST::Float { meta, .. } => meta,
            AST::List { meta, .. } => meta,
            AST::Quoted { meta, .. } => meta,
            AST::QuasiQuoted { meta, .. } => meta,
            AST::Unquoted { meta, .. } => meta,
        }
    }

    pub fn meta_mut(&mut self) -> &mut M {
        match self {
            AST::Symbol { meta, .. } => meta,
            AST::String { meta, .. } => meta,
            AST::Nat { meta, .. } => meta,
            AST::Float { meta, .. } => meta,
            AST::List { meta, .. } => meta,
            AST::Quoted { meta, .. } => meta,
            AST::QuasiQuoted { meta, .. } => meta,
            AST::Unquoted { meta, .. } => meta,
        }
    }

    pub fn drop_meta(self) -> AST<()> {
        match self {
            AST::Symbol { content, .. } => AST::Symbol { content, meta: () },
            AST::String { content, .. } => AST::String { content, meta: () },
            AST::Nat { content, .. } => AST::Nat { content, meta: () },
            AST::Float { content, .. } => AST::Float { content, meta: () },
            AST::List { content, .. } => AST::List {
                content: content.into_iter().map(|x| x.drop_meta()).collect(),
                meta: (),
            },
            AST::Quoted { content, .. } => AST::Quoted {
                content: Box::new(content.drop_meta()),
                meta: (),
            },
            AST::QuasiQuoted { content, .. } => AST::QuasiQuoted {
                content: Box::new(content.drop_meta()),
                meta: (),
            },
            AST::Unquoted { content, .. } => AST::Unquoted {
                content: Box::new(content.drop_meta()),
                meta: (),
            },
        }
    }
}

impl<M: Default> Sexp for AST<M> {
    fn symbol(s: String) -> Self {
        AST::Symbol {
            content: s,
            meta: Default::default(),
        }
    }

    fn string(s: String) -> Self {
        AST::String {
            content: s,
            meta: Default::default(),
        }
    }

    fn nat(n: u64) -> Self {
        AST::Nat {
            content: n,
            meta: Default::default(),
        }
    }

    fn float(f: f64) -> Self {
        AST::Float {
            content: f,
            meta: Default::default(),
        }
    }

    fn list(l: Vec<Self>) -> Self {
        AST::List {
            content: l,
            meta: Default::default(),
        }
    }
}

impl<M: Default> CoreLisp for AST<M> {
    fn quoted(self) -> Self {
        AST::Quoted {
            content: Box::new(self),
            meta: Default::default(),
        }
    }
}

impl<M: Default> Lisp for AST<M> {
    fn unquoted(self) -> Self {
        AST::Unquoted {
            content: Box::new(self),
            meta: Default::default(),
        }
    }

    fn quasiquoted(self) -> Self {
        AST::QuasiQuoted {
            content: Box::new(self),
            meta: Default::default(),
        }
    }
}

// This is an ad-hoc conversion, which represents quotations,
// quasiquotations and unquotations as function calls.
// It will serve to quickly parse Sexp before a more serious
// implementation of Lisp is developed.
impl<M> IntoSexp for AST<M> {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            AST::Symbol { content, .. } => S::symbol(content.clone()),
            AST::String { content, .. } => S::string(content.clone()),
            AST::Nat { content, .. } => S::nat(content),
            AST::Float { content, .. } => S::float(content),
            AST::List { content, .. } => {
                let mut acc = Vec::new();
                for item in content {
                    acc.push(item.into_sexp());
                }
                S::list(acc)
            }
            AST::Quoted { content, .. } => {
                S::list(vec![S::symbol("quote".to_string()), content.into_sexp()])
            }
            AST::QuasiQuoted { content, .. } => S::list(vec![
                S::symbol("quasiquote".to_string()),
                content.into_sexp(),
            ]),
            AST::Unquoted { content, .. } => {
                S::list(vec![S::symbol("unquote".to_string()), content.into_sexp()])
            }
        }
    }
}

impl<M: WithLocation> WithLocation for AST<M> {
    type Loc = M::Loc;

    fn annot(&mut self, loc: Range<usize>) {
        self.meta_mut().annot(loc)
    }

    fn get_location(&self) -> Self::Loc {
        self.meta().get_location()
    }
}

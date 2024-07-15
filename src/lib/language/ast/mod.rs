use super::{CoreLisp, IntoSexp, Lisp, Sexp};

pub mod expect;

#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    Symbol(String),
    String(String),
    Nat(u64),
    Float(f64),
    List(Vec<AST>),
    Quoted(Box<AST>),
    QuasiQuoted(Box<AST>),
    Unquoted(Box<AST>),
}

impl Sexp for AST {
    fn symbol(s: String) -> Self {
        AST::Symbol(s)
    }

    fn string(s: String) -> Self {
        AST::String(s)
    }

    fn nat(n: u64) -> Self {
        AST::Nat(n)
    }

    fn float(f: f64) -> Self {
        AST::Float(f)
    }

    fn list(l: Vec<Self>) -> Self {
        AST::List(l)
    }
}

impl CoreLisp for AST {
    fn quoted(self) -> Self {
        AST::Quoted(Box::new(self))
    }
}

impl Lisp for AST {
    fn unquoted(self) -> Self {
        AST::Unquoted(Box::new(self))
    }

    fn quasiquoted(self) -> Self {
        AST::QuasiQuoted(Box::new(self))
    }
}

// This is an ad-hoc conversion, which represents quotations,
// quasiquotations and unquotations as function calls.
// It will serve to quickly parse Sexp before a more serious
// implementation of Lisp is developed.
impl IntoSexp for AST {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            AST::Symbol(s) => S::symbol(s.clone()),
            AST::String(s) => S::string(s.clone()),
            AST::Nat(n) => S::nat(n),
            AST::Float(f) => S::float(f),
            AST::List(l) => {
                let mut acc = Vec::new();
                for item in l {
                    acc.push(item.into_sexp());
                }
                S::list(acc)
            }
            AST::Quoted(ast) => S::list(vec![S::symbol("quote".to_string()), ast.into_sexp()]),
            AST::QuasiQuoted(ast) => {
                S::list(vec![S::symbol("quasiquote".to_string()), ast.into_sexp()])
            }
            AST::Unquoted(ast) => S::list(vec![S::symbol("unquote".to_string()), ast.into_sexp()]),
        }
    }
}

// impl Debug for AST {
//     fn fmt(&self, f: &mut Formatter) -> fmt::Result {
//         write!(f, "{}", self.clone().into_sexp::<String>())
//     }
// }

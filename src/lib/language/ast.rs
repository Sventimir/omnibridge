use super::{parser::{self, ParseError}, CoreLisp, Lisp, Sexp};

#[derive(Clone)]
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

pub enum UnimplementedError {
    QuasiQuotation(AST),
    Unquotation(AST),
}

impl AST {
    pub fn parse(input: &str) -> Result<Vec<AST>, ParseError> {
        parser::parse(input)
    }

    // This is an ad-hoc conversion, which ignores quotation and
    // throws error upon encoutering quasiquotations and unquotations.
    // It will serve to quickly parse Sexp before a more serious
    // implementation of Lisp is developed.
    pub fn into_sexp<S: Sexp + Sized>(&self) -> Result<S, UnimplementedError> {
        match self {
            AST::Symbol(s) => Ok(S::symbol(s.clone())),
            AST::String(s) => Ok(S::string(s.clone())),
            AST::Nat(n) => Ok(S::nat(*n)),
            AST::Float(f) => Ok(S::float(*f)),
            AST::List(l) => {
                let mut acc = Vec::new();
                for item in l {
                    acc.push(item.into_sexp()?);
                }
                Ok(S::list(acc))
            }
            AST::Quoted(ast) => ast.into_sexp(),
            AST::QuasiQuoted(_) => Err(UnimplementedError::QuasiQuotation(self.clone())),
            AST::Unquoted(_) => Err(UnimplementedError::Unquotation(self.clone())),
        }
    }
}

use crate::{IntoSexp, Sexp, SexpError};

#[derive(Clone, Debug)]
pub enum Expr {
    Symbol(String),
    Str(String),
    Nat(u64),
    Float(f64),
    List(Vec<Expr>),
}

impl Default for Expr {
    fn default() -> Self {
        Expr::List(Vec::new())
    }
}

impl Sexp for Expr {
    fn symbol(s: String) -> Self {
        Expr::Symbol(s)
    }

    fn string(s: String) -> Self {
        Expr::Str(s)
    }

    fn nat(n: u64) -> Self {
        Expr::Nat(n)
    }

    fn float(f: f64) -> Self {
        Expr::Float(f)
    }

    fn list(l: Vec<Self>) -> Self {
        Expr::List(l)
    }
}

impl IntoSexp for Expr {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            Expr::Symbol(s) => S::symbol(s),
            Expr::Str(s) => S::string(s),
            Expr::Nat(n) => S::nat(n),
            Expr::Float(f) => S::float(f),
            Expr::List(l) => S::list(l.into_iter().map(|e| e.into_sexp()).collect()),
        }
    }
}

impl TryFrom<Expr> for bool {
    type Error = SexpError;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::List(l) => {
                if l.is_empty() {
                    Ok(false)
                } else {
                    Err(SexpError::Unexpected {
                        expected: "bool".to_string(),
                        found: l.into_sexp::<String>(),
                    })
                }
            }
            Expr::Symbol(s) => match s.as_str() {
                "t" => Ok(true),
                "f" => Ok(false),
                _ => Err(SexpError::Unexpected {
                    expected: "bool".to_string(),
                    found: s,
                }),
            },
            _ => Err(SexpError::Unexpected {
                expected: "bool".to_string(),
                found: value.into_sexp::<Expr>().into_sexp::<String>(),
            }),
        }
    }
}

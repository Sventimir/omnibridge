/// Implementors of this trait represent symbolic expressions.
/// A symbolic expression is a fully-evaluated, normal form
/// of Lisp expressions. This trait can be extended to also
/// represent unevaluated Lisp expressions. Another expansion
/// on top of that can also contain macros.
pub trait Sexp: Sized {
    fn symbol(s: String) -> Self;
    fn string(s: String) -> Self;
    fn nat(n: u64) -> Self;
    fn float(f: f64) -> Self;
    fn list(l: Vec<Self>) -> Self;
}

impl Sexp for String {
    fn symbol(s: String) -> Self {
        s
    }

    fn string(s: String) -> Self {
        format!("\"{}\"", s)
    }

    fn nat(n: u64) -> Self {
        n.to_string()
    }

    fn float(f: f64) -> Self {
        f.to_string()
    }

    fn list(l: Vec<Self>) -> Self {
        let mut s = String::from("(");
        for item in l {
            s.push_str(&item);
            s.push(' ');
        }
        s = s.trim().into();
        s.push(')');
        s
    }
}

pub trait IntoSexp {
    fn into_sexp<S: Sexp>(self) -> S;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SexpError {
    Unexpected { expected: String, found: String },
}

pub fn nil<S: Sexp>() -> S {
    S::list(vec![])
}

pub fn pair<S: Sexp>(left: S, right: S) -> S {
    S::list(vec![left, right])
}

pub fn int<S: Sexp>(i: i64) -> S {
    let n = i.abs() as u64;
    if i > 0 {
        S::nat(n)
    } else {
        S::list(vec![S::symbol("-".to_string()), S::nat(n)])
    }
}

impl<T: IntoSexp> IntoSexp for Vec<T> {
    fn into_sexp<S: Sexp>(self) -> S {
        S::list(self.into_iter().map(IntoSexp::into_sexp).collect())
    }
}

impl<T: IntoSexp, U: IntoSexp> IntoSexp for (T, U) {
    fn into_sexp<S: Sexp>(self) -> S {
        S::list(vec![self.0.into_sexp(), self.1.into_sexp()])
    }
}

impl<T: IntoSexp> IntoSexp for std::ops::Range<T> {
    fn into_sexp<S: Sexp>(self) -> S {
        S::list(vec![self.start.into_sexp(), self.end.into_sexp()])
    }
}

impl IntoSexp for usize {
    fn into_sexp<S: Sexp>(self) -> S {
        S::nat(self as u64)
    }
}

impl IntoSexp for bool {
    fn into_sexp<S: Sexp>(self) -> S {
        if self {
            S::symbol("t".to_string())
        } else {
            S::symbol("f".to_string())
        }
    }
}

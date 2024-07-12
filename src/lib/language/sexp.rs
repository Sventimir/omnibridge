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
    fn into_sexp<S: Sexp + Sized>(self) -> S;
}

pub enum SexpError {
    Unexpected { expected: String, found: String },
}

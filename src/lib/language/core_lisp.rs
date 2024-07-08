use super::sexp::Sexp;

/// Implementors of this trait represent core Lisp expressions.
/// A core Lisp expression can be evaluated to a normal form or
/// Sexp or can be converted to Sexp as is withoput evaluation
/// by simply removing all quotations. During evaluation, quotations
/// prevent the quoted portion from being evaluated and simply
/// convert to sexp as-is.
/// This trait can be extended into a complete Lisp by adding
/// macros (quasi-quotation and unquotation).
pub trait CoreLisp: Sexp {
    fn quoted(self) -> Self;
}

impl CoreLisp for String {
    fn quoted(self) -> Self {
        format!("'{}", self)
    }
}

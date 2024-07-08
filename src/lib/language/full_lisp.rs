use super::core_lisp::CoreLisp;

/// Implementors of this trait represent full Lisp expressions.
/// A Lisp expression can contain macros which need to be expanded
/// before the expression can be evaluated. 
pub trait Lisp: CoreLisp {
    fn unquoted(self) -> Self;
    fn quasiquoted(self) -> Self;
}

impl Lisp for String {
    fn unquoted(self) -> Self {
        format!(",{}", self)
    }

    fn quasiquoted(self) -> Self {
        format!("`{}", self)
    }
}

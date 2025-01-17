
#[derive(Debug, Clone)]
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

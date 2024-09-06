pub trait Type: Copy {
    type Repr: Clone + Default; // Rust type representing this type

    fn symbol() -> &'static str;
}

#[derive(Clone, Copy)]
pub struct Bool;

impl Type for Bool {
    type Repr = bool;

    fn symbol() -> &'static str {
        "bool"
    }
}

pub fn match_types<A: Type, B: Type>(_: A, _: B) -> bool {
    A::symbol() == B::symbol()
}

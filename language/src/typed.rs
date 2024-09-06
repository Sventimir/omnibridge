pub trait Type: Copy {
    type Repr: Clone + Default; // Rust type representing this type

    fn symbol() -> &'static str;
    fn to_bytes(repr: &Self::Repr) -> Vec<u8>;
    fn from_bytes(bytes: Vec<u8>) -> Result<Self::Repr, MalformedDataError>;
}

#[derive(Clone, Copy)]
pub struct Bool;

impl Type for Bool {
    type Repr = bool;

    fn symbol() -> &'static str {
        "bool"
    }

    fn to_bytes(repr: &Self::Repr) -> Vec<u8> {
        vec![*repr as u8]
    }

    fn from_bytes(bytes: Vec<u8>) -> Result<Self::Repr, MalformedDataError> {
        if bytes.len() != 1 {
            return Err(MalformedDataError {
                typ: Self::symbol(),
                bytes,
            });
        }
        match bytes[0] & 1 {
            0 => Ok(false),
            1 => Ok(true),
            _ => unreachable!(),
        }
    }
}

pub fn match_types<A: Type, B: Type>(_: A, _: B) -> bool {
    A::symbol() == B::symbol()
}

#[derive(Debug, PartialEq, Eq)]
pub struct MalformedDataError {
    typ: &'static str,
    bytes: Vec<u8>,
}

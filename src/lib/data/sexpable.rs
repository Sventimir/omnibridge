use sexp::{Atom, Sexp};


#[derive(Debug)]
pub enum SexpError<'a> {
    UnexpectedList(&'a Sexp),
    UnexpectedAtom(&'a Atom),
    UnexpectedNegativeNumber(i64),
    UnexpectedValue(&'a Sexp)
}

pub trait Sexpable: Sized {
    fn to_sexp(&self) -> Sexp;
    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError>;
}

pub fn atom(sexp: &Sexp) -> Result<&Atom, SexpError> {
    match sexp {
        &Sexp::Atom(ref a) => Ok(&a),
        _ => Err(SexpError::UnexpectedList(&sexp))
    }
}

pub fn list(sexp: &Sexp) -> Result<&[Sexp], SexpError> {
    match sexp {
        &Sexp::List(ref l) => Ok(&l),
        &Sexp::Atom(ref a) => Err(SexpError::UnexpectedAtom(&a))
    }
}

pub fn string(atom: &Atom) -> Result<&str, SexpError> {
    match atom {
        &Atom::S(ref s) => Ok(&s),
        _ => Err(SexpError::UnexpectedAtom(&atom))
    }
}

pub fn int(atom: &Atom) -> Result<i64, SexpError> {
    match atom {
        &Atom::I(i) => Ok(i),
        _ => Err(SexpError::UnexpectedAtom(&atom))
    }
}

pub fn float(atom: &Atom) -> Result<f64, SexpError> {
    match atom {
        &Atom::F(f) => Ok(f),
        _ => Err(SexpError::UnexpectedAtom(&atom))
    }
}

pub fn uint(atom: &Atom) -> Result<u64, SexpError> {
    let i = int(atom)?;
    if i < 0 {
        Err(SexpError::UnexpectedNegativeNumber(i))
    } else {
        Ok(i as u64)
    }
}

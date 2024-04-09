use sexp::{Atom, Sexp};


#[derive(Debug)]
pub enum SexpError<'a> {
    UnexpectedList(&'a Sexp),
    UnexpectedAtom(&'a Atom),
    UnexpectedNegativeNumber(i64),
    UnexpectedValue(&'a Sexp)
}

impl<'a> SexpError<'a> {
    pub fn to_sexp(&self) -> Sexp {
        match self {
            SexpError::UnexpectedAtom(a) =>
                sexp::list(&[
                    sexp::atom_s("error"),
                    mk_assoc_pair("type", sexp::atom_s("unexpected-atom")),
                    mk_assoc_pair("atom", Sexp::Atom((*a).clone()))
                ]),
            SexpError::UnexpectedList(s) =>
                sexp::list(&[
                    sexp::atom_s("error"),
                    mk_assoc_pair("type", sexp::atom_s("unexpected-list")),
                    mk_assoc_pair("atom", (*s).clone())
                ]),
            SexpError::UnexpectedNegativeNumber(n) =>
                sexp::list(&[
                    sexp::atom_s("error"),
                    mk_assoc_pair("type", sexp::atom_s("unexpected-negative-number")),
                    mk_assoc_pair("atom", sexp::atom_i(*n))
                ]),
            SexpError::UnexpectedValue(v) =>
                sexp::list(&[
                    sexp::atom_s("error"),
                    mk_assoc_pair("type", sexp::atom_s("unexpected-value")),
                    mk_assoc_pair("atom", (*v).clone())
                ])
        }
    }
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

pub fn mk_assoc_pair(key: &str, value: Sexp) -> Sexp {
    Sexp::List(vec![ sexp::atom_s(key), value ])
}

use sexp::*;

#[derive(Debug)]
pub enum SexpError {
    InvalidTag(String),
    InvalidValue(Sexp, String),
    NilExpected(Sexp),
    ListExpected(Atom),
    StringExpected(Sexp),
    NumberExpected(Sexp),
    ParseError(sexp::Error),
    Custom(String)
}

impl SexpError {
    pub fn to_sexp(&self) -> Sexp {
        match self {
            SexpError::InvalidTag(tag) =>
                (sexp::atom_s("invalid-tag"), tag.clone()).to_sexp(),
            SexpError::InvalidValue(sexp, ty) =>
                sexp::list(&[
                    sexp::atom_s("invalid-value"),
                    sexp.clone(),
                    ty.to_sexp()
                ]),
            SexpError::NilExpected(sexp) =>
                (sexp::atom_s("nil-expected"), sexp.clone()).to_sexp(),
            SexpError::ListExpected(atom) =>
                (sexp::atom_s("list-expected"), Sexp::Atom(atom.clone())).to_sexp(),
            SexpError::StringExpected(sexp) =>
                (sexp::atom_s("string-expected"), sexp.clone()).to_sexp(),
            SexpError::NumberExpected(sexp) =>
                (sexp::atom_s("number-expected"), sexp.clone()).to_sexp(),
            SexpError::ParseError(err) =>
                sexp::list(&[
                    sexp::atom_s("parse-error"),
                    sexp::atom_s(err.message),
                    (sexp::atom_s("line"), err.line as i64).to_sexp(),
                    (sexp::atom_s("column"), err.column as i64).to_sexp(),
                    (sexp::atom_s("index"), err.index as i64).to_sexp()
                ]),
            SexpError::Custom(msg) =>
                sexp::atom_s(msg)
        }
    }
}

pub const NIL : Sexp = Sexp::List(vec![]);

pub trait Sexpable: Sized {
    fn to_sexp(&self) -> Sexp;
    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError>;
}

pub fn expect_list(sexp: &Sexp) -> Result<&[Sexp], SexpError> {
    match sexp {
        Sexp::List(xs) => Ok(xs),
        Sexp::Atom(a) => Err(SexpError::ListExpected(a.clone()))
    }
}

pub fn expect_nil(sexp: &Sexp) -> Result<(), SexpError> {
    match sexp {
        Sexp::Atom(sexp::Atom::S(s)) if s == "nil" => Ok(()),
        Sexp::List(xs) if xs.is_empty() => Ok(()),
        _ => Err(SexpError::NilExpected(sexp.clone()))
    }
}

pub fn expect_string(sexp: &Sexp) -> Result<&str, SexpError> {
    match sexp {
        Sexp::Atom(Atom::S(s)) => Ok(s),
        _ => Err(SexpError::StringExpected(sexp.clone()))
    }
}

pub fn expect_int(sexp: &Sexp) -> Result<i64, SexpError> {
    match sexp {
        Sexp::Atom(Atom::I(i)) => Ok(*i),
        _ => Err(SexpError::NumberExpected(sexp.clone()))
    }
}

pub fn expect_enum<T: Clone>(choice: &[(&str, T)], sexp: &Sexp) -> Result<T, SexpError> {
    let str = expect_string(sexp)?;
    for (tag, val) in choice {
        if str == *tag {
            return Ok(val.clone());
        }
    }
    Err(SexpError::InvalidTag(sexp.to_string()))
}

impl<A: Sexpable, B: Sexpable> Sexpable for (A, B) {
    fn to_sexp(&self) -> Sexp {
        list(&[ self.0.to_sexp(), self.1.to_sexp() ])
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        let elems = expect_list(sexp)?;
        match elems {
            [a, b] => Ok((A::from_sexp(a)?, B::from_sexp(b)?)),
            _ => Err(SexpError::InvalidValue(sexp.clone(), "Pair".to_string()))
        }
    }
}


impl<T: Sexpable> Sexpable for Option<T> {
    fn to_sexp(&self) -> Sexp {
        match self {
            Some(x) => list(&[ x.to_sexp() ]),
            None => NIL
        }
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        let elems = expect_list(&sexp)?;
        match elems {
            [] => Ok(None),
            [x] => Ok(Some(T::from_sexp(x)?)),
            _ => Err(SexpError::InvalidValue(sexp.clone(), "Option".to_string()))
        }
    }
}

impl<T: Sexpable, E: Sexpable> Sexpable for Result<T, E> {
    fn to_sexp(&self) -> Sexp {
        match self {
            Ok(x) => list(&[ atom_s("ok"), x.to_sexp() ]),
            Err(e) => list(&[ atom_s("error"), e.to_sexp() ])
        }
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        let elems = expect_list(&sexp)?;
        match elems {
            [tag, val] => {
                let t = expect_string(&tag)?;
                match t {
                    "ok" => Ok(Ok(T::from_sexp(val)?)),
                    "error" => Ok(Err(E::from_sexp(val)?)),
                    _ => Err(SexpError::InvalidValue(sexp.clone(), "Result".to_string()))
                }
            },
            _ => Err(SexpError::InvalidValue(sexp.clone(), "Result".to_string()))
        }
    }
}

impl Sexpable for Sexp {
    fn to_sexp(&self) -> Sexp {
        self.clone()
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        Ok(sexp.clone())
    }
}

impl Sexpable for String {
    fn to_sexp(&self) -> Sexp {
        atom_s(self)
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        expect_string(sexp).map(|s| s.to_string())
    }
}

impl Sexpable for u64 {
    fn to_sexp(&self) -> Sexp {
        atom_i(*self as i64)
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        expect_int(sexp).map(|i| i as u64)
    }
}

impl Sexpable for i64 {
    fn to_sexp(&self) -> Sexp {
        atom_i(*self)
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        expect_int(sexp)
    }
}

impl Sexpable for bool {
    fn to_sexp(&self) -> Sexp {
        atom_s(if *self { "t" } else { "nil" })
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        let symbol = expect_string(sexp)?;
        match symbol {
            "t" => Ok(true),
            "nil" => Ok(false),
            _ => Err(SexpError::InvalidValue(sexp.clone(), "boolean".to_string()))
        }
    }
}

impl<T: Sexpable> Sexpable for Vec<T> {
    fn to_sexp(&self) -> Sexp {
        list(&self.iter().map(|x| x.to_sexp()).collect::<Vec<_>>())
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        let elems = expect_list(sexp)?;
        let mut res = Vec::with_capacity(elems.len());
        for elem in elems {
            res.push(T::from_sexp(elem)?);
        }
        Ok(res)
    }
}

pub fn iter_into_sexp<I: Sexpable, T: Iterator<Item = I>>(i: T) -> Sexp {
    list(&i.map(|x| x.to_sexp()).collect::<Vec<_>>())
}

pub struct IterSexp<'a>(&'a[Sexp]);

impl<'a> Iterator for IterSexp<'a> {
    type Item = &'a Sexp;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_empty() {
            None
        } else {
            let (x, rest) = self.0.split_at(1);
            self.0 = rest;
            Some(&x[0])
        }
    }
}

pub fn iter_sexp<'a>(sexp: &'a Sexp) -> Result<IterSexp<'a>, SexpError> {
    let elems = expect_list(sexp)?;
    Ok(IterSexp(elems))
}

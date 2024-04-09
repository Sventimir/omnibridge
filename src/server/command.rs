use sexp::Sexp;
use sexp;
use bridge::data::sexpable::{Sexpable, SexpError};
use bridge::data::sexpable;
use bridge::dealer::deal;


pub enum Cmd {
    Deal(u8)
}

impl Sexpable for Cmd {
    fn to_sexp(&self) -> sexp::Sexp {
        match self {
            &Cmd::Deal(n) => sexp::list(&[
                sexp::atom_s("deal"),
                sexp::atom_i(n as i64)
            ])
        }
    }

    fn from_sexp(sexp: &sexp::Sexp) -> Result<Cmd, SexpError> {
        let l = sexpable::list(sexp)?;
        let cmd = sexpable::string(sexpable::atom(&l[0])?)?;
        match cmd {
            "deal" => Ok(Cmd::Deal(sexpable::uint(sexpable::atom(&l[1])?)? as u8)),
            _ => Err(SexpError::UnexpectedValue(sexp))
        }
    }
}

impl Cmd {
    pub fn execute(&self) -> Result<Sexp, Sexp> {
        match self {
            Cmd::Deal(board) => Ok(deal(*board).to_sexp())
        }
    }
}

use derive_more::{Add, Neg, Sub};
use serde::{Serialize, Deserialize};
use std::fmt::{self, Display, Formatter};

use crate::sexpr::{self, Sexpable, SexpError};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Add, Neg, Sub)]
pub struct Score(i16);

impl Score {
    pub const ZERO: Score = Score(0);

    pub fn from_i16(scr: i16) -> Score {
        Score(scr)
    }
}

impl Display for Score {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Sexpable for Score {
    fn to_sexp(&self) -> sexp::Sexp {
        sexp::atom_i(self.0 as i64)
    }

    fn from_sexp(sexp: &sexp::Sexp) -> Result<Self, SexpError> {
        Ok(Score::from_i16(sexpr::expect_int(sexp)? as i16))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Add, Neg, Sub)]
pub struct IMP(i8);

impl Display for IMP {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Sexpable for IMP {
    fn to_sexp(&self) -> sexp::Sexp {
        sexp::atom_i(self.0 as i64)
    }

    fn from_sexp(sexp: &sexp::Sexp) -> Result<Self, SexpError> {
        Ok(IMP(sexpr::expect_int(sexp)? as i8))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Matchpoints(u16);

impl Matchpoints {
    pub fn percentage(&self, max: &Matchpoints) -> f32 {
        (self.0 * 100) as f32 / max.0 as f32
    }
}

impl Sexpable for Matchpoints {
    fn to_sexp(&self) -> sexp::Sexp {
        sexp::atom_i(self.0 as i64)
    }

    fn from_sexp(sexp: &sexp::Sexp) -> Result<Self, SexpError> {
        Ok(Matchpoints(sexpr::expect_int(sexp)? as u16))
    }
}

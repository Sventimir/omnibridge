use derive_more::{Add, Neg, Sub, Sum};
use serde::{Deserialize, Serialize};
use std::fmt::{self, Display, Formatter};

use crate::sexpr::{self, SexpError, Sexpable};

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

impl Scorable for Score {
    fn score(&self) -> Score {
        self.clone()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Add, Neg, Sub, Sum)]
pub struct IMP(i8);

impl IMP {
    const TABLE: [i16; 24] = [
        20, 50, 90, 130, 170, 220, 270, 320, 370, 430, 500, 600, 750, 900, 1100, 1300, 1500, 1750,
        2000, 2250, 2500, 3000, 3500, 4000,
    ];

    pub fn from_scores(open: &Score, closed: &Score) -> IMP {
        let diff = (open.0 - closed.0) as i16;
        let abs_score = diff.abs();
        let mut imp = 0;
        for boundary in IMP::TABLE {
            if abs_score < boundary {
                break;
            }
            imp += 1;
        }
        IMP(imp)
    }
}

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

pub trait Scorable {
    fn score(&self) -> Score;
}

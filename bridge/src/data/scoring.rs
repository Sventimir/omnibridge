use derive_more::{Add, Neg, Sub, Sum};
use serde::{Deserialize, Serialize};
use std::fmt::{self, Display, Formatter};

use language::{
    ast::{
        expect::{self, ExpectError},
        AST,
    },
    int,
    typed::{IType, Type},
    IntoSexp, Sexp,
};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Add, Neg, Sub)]
pub struct Score(i16);

impl Score {
    pub const ZERO: Score = Score(0);

    pub fn from_i16(scr: i16) -> Score {
        Score(scr)
    }

    pub fn from_f64(scr: f64) -> Score {
        Score(scr as i16)
    }
}

impl Display for Score {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl IntoSexp for Score {
    fn into_sexp<S: Sexp>(self) -> S {
        int(self.0 as i64)
    }
}

impl<M: Clone> TryFrom<&AST<M>> for Score {
    type Error = ExpectError<M>;

    fn try_from(ast: &AST<M>) -> Result<Self, Self::Error> {
        Ok(Score::from_i16(expect::int(ast)? as i16))
    }
}

impl IType for Score {
    fn tag() -> Type {
        Type::Decimal
    }
}

impl Scorable for Score {
    fn score(&self) -> Score {
        self.clone()
    }
}

#[derive(
    Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Add, Neg, Sub, Sum,
)]
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

impl IntoSexp for IMP {
    fn into_sexp<S: Sexp>(self) -> S {
        int(self.0 as i64)
    }
}

impl<M: Clone> TryFrom<&AST<M>> for IMP {
    type Error = ExpectError<M>;

    fn try_from(ast: &AST<M>) -> Result<Self, Self::Error> {
        Ok(IMP(expect::int(ast)? as i8))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Matchpoints(u16);

impl Matchpoints {
    pub fn percentage(&self, max: &Matchpoints) -> f32 {
        (self.0 * 100) as f32 / max.0 as f32
    }
}

impl IntoSexp for Matchpoints {
    fn into_sexp<S: Sexp>(self) -> S {
        S::nat(self.0 as u64)
    }
}

impl<M: Clone> TryFrom<&AST<M>> for Matchpoints {
    type Error = ExpectError<M>;

    fn try_from(ast: &AST<M>) -> Result<Self, Self::Error> {
        Ok(Matchpoints(expect::nat(ast)? as u16))
    }
}

pub trait Scorable {
    fn score(&self) -> Score;
}

use bridge::{data::{
    bid::Contract,
    board::BoardNumber,
    match_protocol::{Match, Room},
    scoring::{Score, IMP},
}, sexpr::{expect_string, SexpError, Sexpable}};
use serde::{Deserialize, Serialize};

use crate::state::StateError;

use super::state::Item;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ProtocolType {
    ScoreOnly,
    Contract,
}

impl Sexpable for ProtocolType {
    fn to_sexp(&self) -> sexp::Sexp {
        match self {
            ProtocolType::ScoreOnly => sexp::atom_s("score-only"),
            ProtocolType::Contract => sexp::atom_s("contract"),
        }
    }

    fn from_sexp(sexp: &sexp::Sexp) -> Result<Self, SexpError> {
        let tag = expect_string(sexp)?;
        match tag {
            "score-only" => Ok(ProtocolType::ScoreOnly),
            "contract" => Ok(ProtocolType::Contract),
            _ => Err(SexpError::InvalidTag(tag.to_string())),
        }
    }
}

pub enum Protocol {
    ScoreOnly(Match<Score>),
    Contract(Match<Contract>),
}

impl Protocol {
    pub fn new(kind: ProtocolType, boards: usize, home: String, visitors: String) -> Protocol {
        match kind {
            ProtocolType::ScoreOnly => Protocol::ScoreOnly(Match::new(boards, home, visitors)),
            ProtocolType::Contract => Protocol::Contract(Match::new(boards, home, visitors)),
        }
    }
}

impl Item for Protocol {
    fn insert_score(
        &mut self,
        room: Room,
        board: BoardNumber,
        score: Score,
    ) -> Result<Option<IMP>, StateError> {
        match self {
            Protocol::ScoreOnly(m) => {
                m.set_result(board, room, score);
                Ok(m.imp(board))
            }
            Protocol::Contract(_) => Err(StateError::NotImplemented),
        }
    }
}

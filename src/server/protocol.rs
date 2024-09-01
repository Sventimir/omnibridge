use bridge::{
    data::{
        bid::Contract,
        board::BoardNumber,
        match_protocol::{Match, Room},
        scoring::{Score, IMP},
    },
    language::{
        ast::{
            expect::{self, ExpectError},
            AST,
        },
        IntoSexp, Sexp,
    },
};
use serde::{Deserialize, Serialize};

use crate::state::StateError;

use super::state::Item;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ProtocolType {
    ScoreOnly,
    Contract,
}

impl IntoSexp for ProtocolType {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            ProtocolType::ScoreOnly => S::symbol("score-only".to_string()),
            ProtocolType::Contract => S::symbol("contract".to_string()),
        }
    }
}

impl<M: Clone> TryFrom<&AST<M>> for ProtocolType {
    type Error = ExpectError<M>;

    fn try_from(ast: &AST<M>) -> Result<Self, Self::Error> {
        let tag = expect::string(ast)?;
        match tag {
            "score-only" => Ok(ProtocolType::ScoreOnly),
            "contract" => Ok(ProtocolType::Contract),
            _ => Err(ExpectError::InvalidSymbol(tag.to_string(), ast.meta().clone())),
        }
    }
}

#[allow(dead_code)]
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

use std::sync::Mutex;

use bridge::data::match_protocol::Room;
use serde::{Deserialize, Serialize};
use sexp::Sexp;

use bridge::data::board::{Board, BoardNumber};
use bridge::data::result::ContractResult;
use bridge::data::scoring::{Scorable, Score, IMP};
use bridge::dealer::deal;
use bridge::sexpr::*;

use crate::protocol::{Protocol, ProtocolType};
use crate::state::{Id, State};

#[derive(Debug, Serialize, Deserialize)]
pub enum Cmd {
    #[serde(rename = "deal")]
    Deal(BoardNumber),
    #[serde(rename = "score")]
    Score(ContractResult),
    #[serde(rename = "new_match")]
    NewMatch {
        kind: ProtocolType,
        boards: usize,
        home: String,
        visitors: String,
    },
    #[serde(rename = "insert_score")]
    InsertScore {
        id: Id,
        room: Room,
        board: BoardNumber,
        score: Score,
    },
}

impl Sexpable for Cmd {
    fn to_sexp(&self) -> Sexp {
        match self {
            Cmd::Deal(board) => 
                sexp::list(&[sexp::atom_s("deal"), sexp::atom_s(&board.to_string())]),
            Cmd::Score(result) => sexp::list(&[sexp::atom_s("score"), result.to_sexp()]),
            Cmd::NewMatch {kind, boards, home, visitors} =>
                sexp::list(&[
                    sexp::atom_s("new--atch"),
                    kind.to_sexp(),
                    sexp::atom_i(*boards as i64),
                    sexp::atom_s(home),
                    sexp::atom_s(visitors),
                ]),
            Cmd::InsertScore {id, room, board, score} =>
                sexp::list(&[
                    sexp::atom_s("insert-score"),
                    id.to_sexp(),
                    room.to_sexp(),
                    sexp::atom_i(*board as i64),
                    score.to_sexp(),
                ]),
        }
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        let cmd = expect_list(sexp)?;
        let (t, rem) = cmd
            .split_first()
            .ok_or(SexpError::InvalidValue(sexp.clone(), "command".to_string()))?;
        let tag = expect_string(t)?;
        match tag {
            "deal" => {
                let board = expect_int(&rem[0])?;
                Ok(Cmd::Deal(board as BoardNumber))
            }
            "score" => {
                let result = ContractResult::from_sexp(&sexp::list(&rem))?;
                Ok(Cmd::Score(result))
            }
            "new-match" => {
                let kind = ProtocolType::from_sexp(&rem[0])?;
                let boards = expect_int(&rem[1])? as usize;
                let home = expect_string(&rem[2]).map(str::to_string)?;
                let visitors = expect_string(&rem[3]).map(str::to_string)?;
                Ok(Cmd::NewMatch { kind, boards, home, visitors })
            }
            "insert-score" => {
                let id = Id::from_sexp(&rem[0])?;
                let room = Room::from_sexp(&rem[1])?;
                let board = expect_int(&rem[2])? as BoardNumber;
                let score = Score::from_sexp(&rem[3])?;
                Ok(Cmd::InsertScore { id, room, board, score })
            }
            _ => Err(SexpError::InvalidTag(tag.to_string())),
        }
    }
}

#[derive(Debug, Serialize)]
pub enum CommandError {
    CorruptState
}

impl Sexpable for CommandError {
    fn to_sexp(&self) -> Sexp {
        NIL
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        Err(SexpError::InvalidValue(
            sexp.clone(),
            "command error".to_string(),
        ))
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum CommandResult {
    Deal(Board),
    Score(Score),
    Identifier(Id),
    ImpScore(Option<IMP>),
}

impl Sexpable for CommandResult {
    fn to_sexp(&self) -> Sexp {
        match self {
            CommandResult::Deal(board) => board.to_sexp(),
            CommandResult::Score(score) => score.to_sexp(),
            CommandResult::Identifier(id) => id.to_sexp(),
            CommandResult::ImpScore(imp) => match imp {
                Some(imp) => imp.to_sexp(),
                None => NIL,
            },
        }
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        Ok(CommandResult::Deal(Board::from_sexp(sexp)?))
    }
}

impl Cmd {
    pub fn execute(self, state: &mut Mutex<State>) -> Result<CommandResult, CommandError> {
        match self {
            Cmd::Deal(board) => Ok(CommandResult::Deal(deal(board))),
            Cmd::Score(result) => Ok(CommandResult::Score(result.score())),
            Cmd::NewMatch {kind, boards, home, visitors} => {
                let protocol = Protocol::new(kind, boards, home, visitors);
                let mut s = state.lock().or(Err(CommandError::CorruptState))?;
                Ok(CommandResult::Identifier(s.create(protocol)))
            },
            Cmd::InsertScore { id, room, board, score } => {
                let mut s = state.lock().or(Err(CommandError::CorruptState))?;
                let imp = s.insert_score(id, room, board, score).map_err(|_| CommandError::CorruptState)?;
                Ok(CommandResult::ImpScore(imp))
            }
        }
    }
}

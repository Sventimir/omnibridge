use std::sync::Mutex;

use bridge::data::match_protocol::Room;
use bridge::language::ast::expect::{self, ExpectError};
use bridge::language::ast::AST;
use bridge::language::{nil, IntoSexp, Sexp};
use serde::{Deserialize, Serialize};

use bridge::data::board::{Board, BoardNumber};
use bridge::data::result::ContractResult;
use bridge::data::scoring::{Scorable, Score, IMP};
use bridge::dealer::deal;

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

impl IntoSexp for Cmd {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            Cmd::Deal(board) => S::list(vec![S::symbol("deal".to_string()), S::nat(board as u64)]),
            Cmd::Score(result) => S::list(vec![S::symbol("score".to_string()), result.into_sexp()]),
            Cmd::NewMatch {
                kind,
                boards,
                home,
                visitors,
            } => S::list(vec![
                S::symbol("new-match".to_string()),
                kind.into_sexp(),
                S::nat(boards as u64),
                S::string(home),
                S::string(visitors),
            ]),
            Cmd::InsertScore {
                id,
                room,
                board,
                score,
            } => S::list(vec![
                S::symbol("insert-score".to_string()),
                S::nat(id),
                room.into_sexp(),
                S::nat(board as u64),
                score.into_sexp(),
            ]),
        }
    }
}

impl<M: Clone> TryFrom<&AST<M>> for Cmd {
    type Error = ExpectError<M>;

    fn try_from(ast: &AST<M>) -> Result<Self, Self::Error> {
        let cmd = expect::list(ast)?;
        let (t, rem) = cmd
            .split_first()
            .ok_or(ExpectError::WrongLength(5, cmd.to_vec()))?;
        let tag = expect::symbol(t)?;
        match tag {
            "deal" => {
                let board = expect::nat(&rem[0])?;
                Ok(Cmd::Deal(board as BoardNumber))
            }
            "score" => {
                let result = ContractResult::try_from(&AST::List {
                    content: rem.to_vec(),
                    meta: ast.meta().clone(),
                })?;
                Ok(Cmd::Score(result))
            }
            "new-match" => {
                let kind = ProtocolType::try_from(&rem[0])?;
                let boards = expect::nat(&rem[1])? as usize;
                let home = expect::string(&rem[2]).map(str::to_string)?;
                let visitors = expect::string(&rem[3]).map(str::to_string)?;
                Ok(Cmd::NewMatch {
                    kind,
                    boards,
                    home,
                    visitors,
                })
            }
            "insert-score" => {
                let id = expect::nat(&rem[0])?;
                let room = Room::try_from(&rem[1])?;
                let board = expect::nat(&rem[2])? as BoardNumber;
                let score = Score::try_from(&rem[3])?;
                Ok(Cmd::InsertScore {
                    id,
                    room,
                    board,
                    score,
                })
            }
            _ => Err(ExpectError::InvalidSymbol(tag.to_string())),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum CommandError {
    CorruptState,
}

impl IntoSexp for CommandError {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            Self::CorruptState => S::symbol("corrupt-state".to_string()),
        }
    }
}

impl<M: Clone> TryFrom<&AST<M>> for CommandError {
    type Error = ExpectError<M>;

    fn try_from(ast: &AST<M>) -> Result<Self, Self::Error> {
        let tag = expect::symbol(ast)?;
        match tag {
            "corrupt-state" => Ok(Self::CorruptState),
            _ => Err(ExpectError::InvalidSymbol(tag.to_string())),
        }
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

impl IntoSexp for CommandResult {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            CommandResult::Deal(board) => board.into_sexp(),
            CommandResult::Score(score) => score.into_sexp(),
            CommandResult::Identifier(id) => S::nat(id),
            CommandResult::ImpScore(imp) => match imp {
                Some(imp) => imp.into_sexp(),
                None => nil(),
            },
        }
    }
}

impl<M: Clone> TryFrom<&AST<M>> for CommandResult {
    type Error = ExpectError<M>;

    fn try_from(ast: &AST<M>) -> Result<Self, Self::Error> {
        Ok(CommandResult::Deal(Board::try_from(ast)?))
    }
}

impl Cmd {
    pub fn execute(self, state: &mut Mutex<State>) -> Result<CommandResult, CommandError> {
        match self {
            Cmd::Deal(board) => Ok(CommandResult::Deal(deal(board))),
            Cmd::Score(result) => Ok(CommandResult::Score(result.score())),
            Cmd::NewMatch {
                kind,
                boards,
                home,
                visitors,
            } => {
                let protocol = Protocol::new(kind, boards, home, visitors);
                let mut s = state.lock().or(Err(CommandError::CorruptState))?;
                Ok(CommandResult::Identifier(s.create(protocol)))
            }
            Cmd::InsertScore {
                id,
                room,
                board,
                score,
            } => {
                let mut s = state.lock().or(Err(CommandError::CorruptState))?;
                let imp = s
                    .insert_score(id, room, board, score)
                    .map_err(|_| CommandError::CorruptState)?;
                Ok(CommandResult::ImpScore(imp))
            }
        }
    }
}

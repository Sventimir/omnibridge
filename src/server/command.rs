use sexp::Sexp;
use serde::{Serialize, Deserialize};

use bridge::data::board::Board;
use bridge::data::result::ContractResult;
use bridge::dealer::deal;
use bridge::sexpr::*;

#[derive(Debug, Serialize, Deserialize)]
pub enum Cmd {
    #[serde(rename = "deal")]
    Deal(u8),
    #[serde(rename = "score")]
    Score(ContractResult)
}

impl Sexpable for Cmd {
    fn to_sexp(&self) -> Sexp {
        match self {
            Cmd::Deal(board) =>
                sexp::list(&[sexp::atom_s("deal"), sexp::atom_s(&board.to_string())]),
            Cmd::Score(result) =>
                sexp::list(&[sexp::atom_s("score"), result.to_sexp()])
        }
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        let cmd = expect_list(sexp)?;
        let (t, rem) = cmd.split_first()
            .ok_or(SexpError::InvalidValue(sexp.clone(), "command".to_string()))?; 
        let tag = expect_string(t)?;
        match tag {
            "deal" => {
                let board = expect_int(&rem[0])?;
                Ok(Cmd::Deal(board as u8))
            },
            "score" => {
                let result = ContractResult::from_sexp(&sexp::list(&rem))?;
                Ok(Cmd::Score(result))
            },
            _ => Err(SexpError::InvalidTag(tag.to_string()))
        }
    }
}

#[derive(Debug, Serialize)]
pub enum CommandError {}

impl Sexpable for CommandError {
    fn to_sexp(&self) -> Sexp {
       NIL
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        Err(SexpError::InvalidValue(sexp.clone(), "command error".to_string()))
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum CommandResult {
    Deal(Board),
    Score(i16)
}

impl Sexpable for CommandResult {
    fn to_sexp(&self) -> Sexp {
        match self {
            CommandResult::Deal(board) => board.to_sexp(),
            CommandResult::Score(score) => (*score as i64).to_sexp()
        }
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        Ok(CommandResult::Deal(Board::from_sexp(sexp)?))
    }
}

impl Cmd {
    pub fn execute(&self) -> Result<CommandResult, CommandError> {
        match self {
            Cmd::Deal(board) => Ok(CommandResult::Deal(deal(*board))),
            Cmd::Score(result) => Ok(CommandResult::Score(result.score()))
        }
    }
}

use serde::{Serialize, Deserialize};

use bridge::data::board::Board;
use bridge::dealer::deal;

#[derive(Debug, Serialize, Deserialize)]
pub enum Cmd {
    #[serde(rename = "deal")]
    Deal(u8),
}

#[derive(Debug, Serialize)]
pub enum CommandError {}

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum CommandResult {
    Deal(Board)
}

impl Cmd {
    pub fn execute(&self) -> Result<CommandResult, CommandError> {
        match self {
            Cmd::Deal(board) => Ok(CommandResult::Deal(deal(*board))),
        }
    }
}

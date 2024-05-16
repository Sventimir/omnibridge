use serde::{Deserialize, Serialize};

use super::{
    board::{Board, BoardNumber},
    scoring::{Scorable, Score, IMP},
};
use crate::sexpr::{expect_string, SexpError, Sexpable};

pub struct Match<R: Sized> {
    pub home: String,
    pub visitors: String,
    pub boards: Vec<MatchBoard<R>>,
}

pub struct MatchBoard<R: Sized> {
    number: BoardNumber,
    pub board: Option<Board>,
    results: [Option<R>; 2],
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Room {
    Open = 0,   // Home: NS, Visitors: WE
    Closed = 1, // Home: WE, Visitors: NS
}

impl Sexpable for Room {
    fn to_sexp(&self) -> sexp::Sexp {
        match self {
            Room::Open => sexp::atom_s("open"),
            Room::Closed => sexp::atom_s("closed"),
        }
    }

    fn from_sexp(sexp: &sexp::Sexp) -> Result<Self, SexpError> {
        let tag = expect_string(sexp)?;
        match tag {
            "open" => Ok(Room::Open),
            "closed" => Ok(Room::Closed),
            _ => Err(SexpError::InvalidTag(tag.to_string())),
        }
    }
}

impl<R> MatchBoard<R> {
    pub fn new(number: usize, board: Option<Board>) -> MatchBoard<R> {
        MatchBoard {
            number,
            board,
            results: [None, None],
        }
    }
    
    pub fn number(&self) -> BoardNumber {
        self.number
    }

    pub fn result(&self, room: Room) -> Option<&R> {
        self.results[room as usize].as_ref()
    }

    pub fn set_result(&mut self, room: Room, result: R) {
        self.results[room as usize] = Some(result);
    }
}

impl<R: Scorable> MatchBoard<R> {
    pub fn score(&self, room: Room) -> Option<Score> {
        self.result(room).map(Scorable::score)
    }

    pub fn imp(&self) -> Option<IMP> {
        let open = self.score(Room::Open)?;
        let closed = self.score(Room::Closed)?;
        Some(IMP::from_scores(&open, &closed))
    }
}

impl<R> Match<R> {
    pub fn new(board_count: usize, home: String, visitors: String) -> Match<R> {
        let mut boards = Vec::with_capacity(board_count);
        boards.extend((0..board_count).map(|n| MatchBoard::new(n, None)));
        Match {
            home,
            visitors,
            boards,
        }
    }

    pub fn result(&self, board: BoardNumber, room: Room) -> Option<&R> {
        self.boards[board as usize].result(room)
    }

    pub fn set_result(&mut self, board: BoardNumber, room: Room, result: R) {
        self.boards[board as usize].set_result(room, result);
    }
}

impl<R: Scorable> Match<R> {
    pub fn imp(&self, board: BoardNumber) -> Option<IMP> {
        self.boards[board as usize].imp()
    }
   
    pub fn total_imp(&self) -> IMP {
        self.boards.iter().filter_map(MatchBoard::imp).sum()
    }
}

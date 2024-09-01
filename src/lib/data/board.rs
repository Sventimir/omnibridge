use num::FromPrimitive;
use serde::{Deserialize, Serialize};

use super::hand::Hand;
use super::table::{Dir, Vulnerability};
use crate::language::ast::expect::{self, ExpectError};
use crate::language::ast::AST;
use crate::language::{pair, IntoSexp, Sexp};

pub type BoardNumber = usize;

#[derive(Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Board {
    #[serde(rename(serialize = "board", deserialize = "board"))]
    pub number: BoardNumber,
    #[serde(rename(serialize = "N", deserialize = "N"))]
    pub north: Hand,
    #[serde(rename(serialize = "E", deserialize = "E"))]
    pub east: Hand,
    #[serde(rename(serialize = "S", deserialize = "S"))]
    pub south: Hand,
    #[serde(rename(serialize = "W", deserialize = "W"))]
    pub west: Hand,
}

pub fn vulnerability(board: &u8) -> Vulnerability {
    let b = board - 1;
    FromPrimitive::from_u8(((b / 4) + (b % 4)) % 4).unwrap()
}

impl Board {
    pub fn new(number: BoardNumber) -> Board {
        Board {
            number,
            north: Hand::new(),
            east: Hand::new(),
            south: Hand::new(),
            west: Hand::new(),
        }
    }

    pub fn dealer(&self) -> Dir {
        FromPrimitive::from_u8(self.number as u8 % 4).unwrap()
    }

    pub fn vulnerability(&self) -> Vulnerability {
        vulnerability(&(self.number as u8))
    }

    pub fn hand(&self, dir: Dir) -> &Hand {
        match dir {
            Dir::North => &self.north,
            Dir::East => &self.east,
            Dir::South => &self.south,
            Dir::West => &self.west,
        }
    }

    pub fn hand_mut(&mut self, dir: Dir) -> &mut Hand {
        match dir {
            Dir::North => &mut self.north,
            Dir::East => &mut self.east,
            Dir::South => &mut self.south,
            Dir::West => &mut self.west,
        }
    }

    pub fn set_hand(&mut self, dir: Dir, hand: Hand) {
        match dir {
            Dir::North => self.north = hand,
            Dir::East => self.east = hand,
            Dir::South => self.south = hand,
            Dir::West => self.west = hand,
        }
    }
}

impl IntoSexp for Board {
    fn into_sexp<S: Sexp>(self) -> S {
        S::list(vec![
            pair(S::symbol("board".to_string()), S::nat(self.number as u64)),
            pair(S::symbol("N".to_string()), self.north.into_sexp()),
            pair(S::symbol("E".to_string()), self.east.into_sexp()),
            pair(S::symbol("S".to_string()), self.south.into_sexp()),
            pair(S::symbol("W".to_string()), self.west.into_sexp()),
        ])
    }
}

impl<M: Clone> TryFrom<&AST<M>> for Board {
    type Error = ExpectError<M>;

    fn try_from(ast: &AST<M>) -> Result<Board, Self::Error> {
        let mut board = Board::new(0);
        let l = expect::list(ast)?;
        for elem in l.iter() {
            let (t, v) = expect::pair(elem)?;
            let tag = expect::symbol(t)?;
            match tag {
                "board" => {
                    let b: u64 = expect::nat(v)?;
                    board.number = b as BoardNumber
                }
                "N" => board.north = Hand::try_from(v)?,
                "E" => board.east = Hand::try_from(v)?,
                "S" => board.south = Hand::try_from(v)?,
                "W" => board.west = Hand::try_from(v)?,
                _ => return Err(ExpectError::InvalidSymbol(tag.to_string())),
            }
        }
        Ok(board)
    }
}

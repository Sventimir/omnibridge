use num::FromPrimitive;
use serde::{Deserialize, Serialize};
use sexp::Sexp;

use super::hand::Hand;
use super::table::{Dir, Vulnerability};
use crate::sexpr::*;

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

impl Sexpable for Board {
    fn to_sexp(&self) -> Sexp {
        sexp::list(&[
            (sexp::atom_s("board"), self.number as u64).to_sexp(),
            (sexp::atom_s("N"), self.north).to_sexp(),
            (sexp::atom_s("E"), self.east).to_sexp(),
            (sexp::atom_s("S"), self.south).to_sexp(),
            (sexp::atom_s("W"), self.west).to_sexp(),
        ])
    }

    fn from_sexp(sexp: &Sexp) -> Result<Board, SexpError> {
        let mut board = Board::new(0);
        for elem in iter_sexp(sexp)? {
            let (tag, v): (String, Sexp) = Sexpable::from_sexp(elem)?;
            match tag.as_str() {
                "board" => {
                    let b: u64 = Sexpable::from_sexp(&v)?;
                    board.number = b as BoardNumber
                }
                "N" => board.north = Hand::from_sexp(&v)?,
                "E" => board.east = Hand::from_sexp(&v)?,
                "S" => board.south = Hand::from_sexp(&v)?,
                "W" => board.west = Hand::from_sexp(&v)?,
                _ => return Err(SexpError::InvalidTag(tag.to_string())),
            }
        }
        Ok(board)
    }
}

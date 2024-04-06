use num::FromPrimitive;
use sexp;
use sexp::Sexp;

use super::hand::Hand;
use super::sexpable;
use super::sexpable::Sexpable;
use super::table::{Dir, Vulnerability};


#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Board {
    pub number: u8,
    pub north: Hand,
    pub east: Hand,
    pub south: Hand,
    pub west: Hand
}

impl Board {
    pub fn dealer(&self) -> Dir {
        FromPrimitive::from_u8(self.number % 4).unwrap()
    }

    pub fn vulnerability(&self) -> Vulnerability {
        FromPrimitive::from_u8(((self.number / 4) + (self.number % 4)) % 4).unwrap()
    }

    pub fn hand(&self, dir: Dir) -> &Hand {
        match dir {
            Dir::North => &self.north,
            Dir::East => &self.east,
            Dir::South => &self.south,
            Dir::West => &self.west
        }
    }

    pub fn hand_mut(&mut self, dir: Dir) -> &mut Hand {
        match dir {
            Dir::North => &mut self.north,
            Dir::East => &mut self.east,
            Dir::South => &mut self.south,
            Dir::West => &mut self.west
        }
    }

    pub fn set_hand(&mut self, dir: Dir, hand: Hand) {
        match dir {
            Dir::North => self.north = hand,
            Dir::East => self.east = hand,
            Dir::South => self.south = hand,
            Dir::West => self.west = hand
        }
    }
}

impl Sexpable for Board {
    fn to_sexp(&self) -> Sexp {
        sexp::list(&[
            sexp::atom_s("board"),
            sexp::atom_i(self.number as i64),
            sexp::list(&[sexp::atom_s("n"), self.north.to_sexp()]),
            sexp::list(&[sexp::atom_s("e"), self.east.to_sexp()]),
            sexp::list(&[sexp::atom_s("s"), self.south.to_sexp()]),
            sexp::list(&[sexp::atom_s("w"), self.west.to_sexp()])
        ])
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, sexpable::SexpError> {
        let list = sexpable::list(sexp)?;
        if list[0] != sexp::atom_s("board") {
            return Err(sexpable::SexpError::UnexpectedValue(&list[0]));
        }
        let num_atom = sexpable::atom(&list[1])?;
        let number = sexpable::uint(num_atom)?;
        let mut board = Board {
            number: number as u8,
            north: Hand::new(),
            east: Hand::new(),
            south: Hand::new(),
            west: Hand::new()
        };
        for h in list[2..].iter() {
            let l = sexpable::list(h)?;
            let dir = Dir::from_sexp(&l[0])?;
            let hand = Hand::from_sexp(&l[1])?;
            board.set_hand(dir, hand);
        }
        Ok(board)
    }
}

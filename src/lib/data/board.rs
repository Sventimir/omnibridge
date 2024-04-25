use num::FromPrimitive;

use super::hand::Hand;
use super::table::{Dir, Vulnerability};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Board {
    pub number: u8,
    pub north: Hand,
    pub east: Hand,
    pub south: Hand,
    pub west: Hand,
}

impl Board {
    pub fn new(number: u8) -> Board {
        Board {
            number,
            north: Hand::new(),
            east: Hand::new(),
            south: Hand::new(),
            west: Hand::new(),
        }
    }

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

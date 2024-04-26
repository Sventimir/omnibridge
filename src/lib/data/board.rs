use num::FromPrimitive;
use serde::{Deserialize, Serialize};

use super::hand::Hand;
use super::table::{Dir, Vulnerability};

#[derive(Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Board {
    #[serde(rename(serialize = "board", deserialize = "board"))]
    pub number: u8,
    #[serde(rename(serialize = "N", deserialize = "N"))]
    pub north: Hand,
    #[serde(rename(serialize = "E", deserialize = "E"))]
    pub east: Hand,
    #[serde(rename(serialize = "S", deserialize = "S"))]
    pub south: Hand,
    #[serde(rename(serialize = "W", deserialize = "W"))]
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

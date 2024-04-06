use num::FromPrimitive;

use super::hand::Hand;
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
}

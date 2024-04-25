use num::FromPrimitive;
use num_derive::FromPrimitive;
use std::str::FromStr;

use super::display::Display;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, FromPrimitive)]
pub enum Dir {
    North = 0,
    East = 1,
    South = 2,
    West = 3,
}

impl Display for Dir {
    fn display(&self) -> String {
        match *self {
            Dir::North => "N".to_string(),
            Dir::East => "E".to_string(),
            Dir::South => "S".to_string(),
            Dir::West => "W".to_string(),
        }
    }

    fn show(&self) -> String {
        self.display()
    }
}

impl Dir {
    pub fn from_int(i: u8) -> Dir {
        FromPrimitive::from_u8(i).unwrap()
    }

    pub fn opponent_of(&self, other: &Dir) -> bool {
        (*self as u8) | 0x1 != (*other as u8) | 0x1
    }

    pub fn iter(&self) -> IterDir {
        IterDir {
            current: *self,
            last: FromPrimitive::from_u8((*self as u8 + 3) % 4).unwrap(),
        }
    }
}

impl FromStr for Dir {
    type Err = String;

    fn from_str(s: &str) -> Result<Dir, String> {
        match s {
            "N" | "n" => Ok(Dir::North),
            "E" | "e" => Ok(Dir::East),
            "S" | "s" => Ok(Dir::South),
            "W" | "w" => Ok(Dir::West),
            _ => Err(format!("Invalid direction: {}", s)),
        }
    }
}

pub struct IterDir {
    current: Dir,
    last: Dir,
}

impl Iterator for IterDir {
    type Item = Dir;

    fn next(&mut self) -> Option<Dir> {
        if self.current == self.last {
            None
        } else {
            let next = FromPrimitive::from_u8((self.current as u8 + 1) % 4).unwrap();
            self.current = next;
            Some(next)
        }
    }
}

pub const DIRS: [Dir; 4] = [Dir::North, Dir::East, Dir::South, Dir::West];

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, FromPrimitive)]
pub enum Side {
    NS = 0,
    EW = 1,
}

impl Display for Side {
    fn display(&self) -> String {
        match *self {
            Side::NS => "NS".to_string(),
            Side::EW => "EW".to_string(),
        }
    }

    fn show(&self) -> String {
        self.display()
    }
}

impl Side {
    pub fn from_int(i: u8) -> Side {
        FromPrimitive::from_u8(i).unwrap()
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, FromPrimitive)]
pub enum Vulnerability {
    None = 0,
    NS = 1,
    EW = 2,
    Both = 3,
}

impl Display for Vulnerability {
    fn display(&self) -> String {
        match *self {
            Vulnerability::None => "None".to_string(),
            Vulnerability::NS => "NS".to_string(),
            Vulnerability::EW => "EW".to_string(),
            Vulnerability::Both => "Both".to_string(),
        }
    }

    fn show(&self) -> String {
        self.display()
    }
}

use num::FromPrimitive;
use num_derive::FromPrimitive;
use super::display::Display;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, FromPrimitive)]
pub enum Dir {
  North = 0,
  East = 1,
  South = 2,
  West = 3
}

impl Display for Dir {
  fn display(&self) -> String {
    match *self {
      Dir::North => "N".to_string(),
      Dir::East => "E".to_string(),
      Dir::South => "S".to_string(),
      Dir::West => "W".to_string()
    }
  }

  fn show(&self) -> String {
    self.display()
  }
}

impl Dir {
  pub fn from_int(i : u8) -> Dir {
    FromPrimitive::from_u8(i).unwrap()
  }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, FromPrimitive)]
pub enum Side {
  NS = 0,
  EW = 1
}

impl Display for Side {
  fn display(&self) -> String {
    match *self {
      Side::NS => "NS".to_string(),
      Side::EW => "EW".to_string()
    }
  }

  fn show(&self) -> String {
    self.display()
  }
}

impl Side {
  pub fn from_int(i : u8) -> Side {
    FromPrimitive::from_u8(i).unwrap()
  }
}

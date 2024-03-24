use std::iter::zip;
use std::ops::{Add, Sub, Range};
use super::card::{Rank, Suit, SUITS};
use super::display::Display;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct Milton(pub u8);

impl Display for Milton {
  fn show(&self) -> String {
    format!("{}", self.0)
  }

  fn display(&self) -> String {
    let int = self.0 >> 2;
    let frac = self.0 & 0x03;
    format!("{}.{}", int, frac * 25)
  }
}

impl Add for Milton {
  type Output = Milton;

  fn add(self, other: Milton) -> Milton {
    Milton(self.0 + other.0)
  }
}

impl Sub for Milton {
  type Output = Milton;

  fn sub(self, other: Milton) -> Milton {
    Milton(self.0 - other.0)
  }
}

impl Milton {
  pub fn scale(&self, factor: u8) -> Milton {
    Milton(self.0 * factor)
  }

  pub fn from_int(i : u8) -> Milton {
    Milton(i << 2)
  }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ShapeType {
  Balanced,
  Monosuiter,
  Twosuiter,
  Threesuiter
}

pub struct Shape(pub [u8; 4]);

impl Shape {
  pub fn empty() -> Shape {
    Shape([0; 4])
  }

  pub fn iter(&self) -> std::slice::Iter<u8> {
    self.0.iter()
  }

  pub fn add_card(&mut self, s: &Suit) {
    self.0[*s as usize] += 1;
  }

  pub fn length_of(&self, s: &Suit) -> u8 {
    self.0[*s as usize]
  }

  pub fn sorted(&self) -> [u8; 4] {
    let mut sorted = self.0;
    sorted.sort();
    sorted.reverse();
    sorted
  }

  pub fn shape_type(&self) -> ShapeType {
    let sorted = self.sorted();
    match sorted {
      [4, 3, 3, 3] | [4, 4, 3, 2] | [5, 3, 3, 2] => ShapeType::Balanced,
      _ => {
        let mut long_suits = 0;
        for l in sorted.iter() {
          if *l >= 4 {
            long_suits += 1;
          } else {
            break;
          }
        }
        match long_suits {
          1 => ShapeType::Monosuiter,
          2 => ShapeType::Twosuiter,
          3 => ShapeType::Threesuiter,
          _ => panic!("Invalid shape")
        }
      }
    }
  }

  pub fn longest_suit(&self) -> (Suit, u8) {
    let mut best_length = 0;
    let mut longest_suit = Suit::Club;
    for (suit, length) in zip(SUITS, self.0.iter()) {
      if *length >= best_length {
        best_length = *length;
        longest_suit = suit;
      }
    }
    (longest_suit, best_length)
  }

  pub fn shortest_suit(&self) -> (Suit, u8) {
    let mut best_length = 99;
    let mut shortest_suit = Suit::Club;
    for (suit, length) in zip(SUITS, self.0.iter()) {
      if *length < best_length {
        best_length = *length;
        shortest_suit = suit;
      }
    }
    (shortest_suit, best_length)
  }
}

impl Display for ShapeType {
  fn show(&self) -> String {
    match self {
      ShapeType::Balanced => "Balanced".to_string(),
      ShapeType::Monosuiter => "Monosuiter".to_string(),
      ShapeType::Twosuiter => "Twosuiter".to_string(),
      ShapeType::Threesuiter => "Threesuiter".to_string()
    }
  }

  fn display(&self) -> String {
    match self {
      ShapeType::Balanced => "Balanced".to_string(),
      ShapeType::Monosuiter => "Mono-suiter".to_string(),
      ShapeType::Twosuiter => "Two-suiter".to_string(),
      ShapeType::Threesuiter => "Three-suiter".to_string()
    }
  }
}

pub struct Eval {
  pub hcp: Milton,
  pub dist_points: Milton,
  pub shape: Shape
}

impl Eval {
  pub fn total_points(&self) -> Milton {
    self.hcp + self.dist_points
  }
}

pub fn hcp_per_rank(r: &Rank) -> Milton {
  match r {
    Rank::Ace => Milton(16),
    Rank::King => Milton(12),
    Rank::Queen => Milton(8),
    Rank::Jack => Milton(4),
    _ => Milton(0)
  }
}

pub fn initial_dist_points_per_length(l : &u8) -> Milton {
  if *l > 4 { Milton(4) } else { Milton(0) }
}

pub const OPENING_THRESHOLD : Milton = Milton(48);  // 12 HCP
pub const ONE_NO_TRUMP_RANGE : Range<Milton> = Milton(60)..Milton(72); // 15-18 HCP
pub const NO_TRUMP_THRESHOLD : Milton = Milton(60); // 15 HCP
pub const STRONG_NO_TRUMP_THRESHOLD : Milton = Milton(72); // 18HCP
pub const TWO_NO_TRUMP_RANGE : Range<Milton> = Milton(80)..Milton(92); // 20-22 HCP
pub const GF_OPENING_THRESHOLD : Milton = Milton(92);   // 23 HCP

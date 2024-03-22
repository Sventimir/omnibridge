use std::cmp::{PartialEq, Eq, PartialOrd, Ord};
use std::str::FromStr;
use num::FromPrimitive;
use num_derive::FromPrimitive;
use super::display::Display;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, FromPrimitive)]
pub enum Suit {
  Club = 0,
  Diamond = 1,
  Heart = 2,
  Spade = 3
}

impl Display for Suit {
  fn show(&self) -> String {
    match self {
      Suit::Club => "C".to_string(),
      Suit::Diamond => "D".to_string(),
      Suit::Heart => "H".to_string(),
      Suit::Spade => "S".to_string(),
    }
  }

  fn display(&self) -> String {
    match self {
      Suit::Club => "♣".to_string(),
      Suit::Diamond => "♦".to_string(),
      Suit::Heart => "♥".to_string(),
      Suit::Spade => "♠".to_string(),
    }
  }
}

impl FromStr for Suit {
  type Err = ();

  fn from_str(s: &str) -> Result<Suit, ()> {
    match s {
      "C" | "c" | "♣" => Ok(Suit::Club),
      "D" | "d" | "♦" => Ok(Suit::Diamond),
      "H" | "h" | "♥" => Ok(Suit::Heart),
      "S" | "s" | "♠" => Ok(Suit::Spade),
      _ => Err(()),
    }
  }
}


#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, FromPrimitive)]
pub enum Rank {
  Two = 2,
  Three = 3,
  Four = 4,
  Five = 5,
  Six = 6,
  Seven = 7,
  Eight = 8,
  Nine = 9,
  Ten = 10,
  Jack = 11,
  Queen = 12,
  King = 13,
  Ace = 14,
}


impl Display for Rank {
  fn show(&self) -> String {
    match self {
      Rank::Ace => "A".to_string(),
      Rank::King => "K".to_string(),
      Rank::Queen => "Q".to_string(),
      Rank::Jack => "J".to_string(),
      Rank::Ten => "T".to_string(),
      v => (*v as isize).to_string(),
    }
  }
}

impl FromStr for Rank {
  type Err = ();

  fn from_str(s: &str) -> Result<Rank, ()> {
    match s {
      "A" | "a" => Ok(Rank::Ace),
      "K" | "k" => Ok(Rank::King),
      "Q" | "q" => Ok(Rank::Queen),
      "J" | "j" => Ok(Rank::Jack),
      "T" | "t" | "10" | "1" | "0" => Ok(Rank::Ten),
      "2" => Ok(Rank::Two),
      "3" => Ok(Rank::Three),
      "4" => Ok(Rank::Four),
      "5" => Ok(Rank::Five),
      "6" => Ok(Rank::Six),
      "7" => Ok(Rank::Seven),
      "8" => Ok(Rank::Eight),
      "9" => Ok(Rank::Nine),
      _ => Err(()),
    }
  }
}

impl Rank {
  pub fn from_int(i: u8) -> Rank {
    FromPrimitive::from_u8(i).unwrap()
  }
}


#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, FromPrimitive)]
pub struct Card(u8);

impl Card {
  pub fn new(suit: Suit, rank: Rank) -> Card {
    Card(((suit as u8) << 4) | (rank as u8))
  }

  pub fn suit(&self) -> Suit {
    FromPrimitive::from_u8(self.0 >> 4).unwrap()
  }

  pub fn rank(&self) -> Rank {
    FromPrimitive::from_u8(self.0 & 0x0F).unwrap()
  }

  pub fn ord(&self) -> u8 {
    let suit = self.0 >> 4;
    let rank = (self.0 & 0x0F) - 2;
    suit * 13 + rank
  }

  pub fn from_ord(ord: u8) -> Card {
    let suit = (ord / 13) << 4;
    let rank = (ord % 13) + 2;
    Card(suit | rank)
  }

  pub fn to_u8(&self) -> u8 {
    self.0
  }
}

impl Display for Card {
  fn show(&self) -> String {
    format!("{}{}", self.suit().show(), self.rank().show())
  }

  fn display(&self) -> String {
    format!("{}{}", self.suit().display(), self.rank().display())
  }
}

impl FromStr for Card {
  type Err = ();

  fn from_str(s: &str) -> Result<Card, ()> {
    let mut chars = s.chars();
    let suit = match chars.next() {
      Some(c) => c.to_string(),
      None => return Err(()),
    };
    let rank = match chars.next() {
      Some(c) => c.to_string(),
      None => return Err(()),
    };
    Suit::from_str(&suit).and_then(|s| {
      Rank::from_str(&rank).map(|r| Card::new(s, r))
    })
  }
}

pub struct Deck(u8);

impl Iterator for Deck {
  type Item = Card;

  fn next(&mut self) -> Option<Card> {
    if self.0 < 52 {
      let c = Some(Card::from_ord(self.0));
      self.0 += 1;
      c
    } else {
      None
    }
  }
}

impl Deck {
  pub fn new() -> Deck {
    Deck(0)
  }
}

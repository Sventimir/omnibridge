use std::cmp::{PartialEq, Eq, PartialOrd, Ord, Ordering};
use super::display::Display;
use super::numeric::Numeric;

pub struct Suit(u8);

pub const CLUB : Suit = Suit(0);
pub const DIAMOND : Suit = Suit(1);
pub const HEART : Suit = Suit(2);
pub const SPADE : Suit = Suit(3);
pub const NO_TRUMP : Suit = Suit(4);

impl Clone for Suit {
  fn clone(&self) -> Suit {
    Suit(self.0)
  }
}
impl Copy for Suit {}

impl Numeric<u8> for Suit {
  fn to_int(&self) -> u8 {
    self.0
  }

  fn from_int(i: u8) -> Suit {
    Suit(i)
  }
}

impl Display for Suit {
  fn show(&self) -> String {
    match self.0 {
      0 => "C".to_string(),
      1 => "D".to_string(),
      2 => "H".to_string(),
      3 => "S".to_string(),
      4 => "NT".to_string(),
      _ => panic!("Invalid suit"),
    }
  }

  fn display(&self) -> String {
    match self.0 {
      0 => "♣".to_string(),
      1 => "♦".to_string(),
      2 => "♥".to_string(),
      3 => "♠".to_string(),
      4 => "NT".to_string(),
      _ => panic!("Invalid suit"),
    }
  }
}

impl PartialEq for Suit {
  fn eq(&self, other : &Suit) -> bool {
    self.0 == other.0
  }
}

impl Eq for Suit {}

impl PartialOrd for Suit {
  fn partial_cmp(&self, other : &Suit) -> Option<Ordering> {
    self.0.partial_cmp(&other.0)
  }
}

impl Ord for Suit {
  fn cmp(&self, other : &Suit) -> Ordering {
    self.0.cmp(&other.0)
  }
}

pub struct Suits(u8);

impl Iterator for Suits {
  type Item = Suit;

  fn next(&mut self) -> Option<Suit> {
    self.0 += 1;
    if self.0 < 5 {
      Some(Suit(self.0))
    } else {
      None
    }
  }
}

pub fn suits() -> Suits {
  Suits(0)
}

pub struct Rank(u8);

pub const ACE : Rank = Rank(14);
pub const KING : Rank = Rank(13);
pub const QUEEN : Rank = Rank(12);
pub const JACK : Rank = Rank(11);
pub const TEN : Rank = Rank(10);
pub const NINE : Rank = Rank(9);
pub const EIGHT : Rank = Rank(8);
pub const SEVEN : Rank = Rank(7);
pub const SIX : Rank = Rank(6);
pub const FIVE : Rank = Rank(5);
pub const FOUR : Rank = Rank(4);
pub const THREE : Rank = Rank(3);
pub const TWO : Rank = Rank(2);

impl Clone for Rank {
  fn clone(&self) -> Rank {
    Rank(self.0)
  }
}

impl Copy for Rank {}

impl Numeric<u8> for Rank {
  fn to_int(&self) -> u8 {
    self.0
  }

  fn from_int(i: u8) -> Rank {
    Rank(i)
  }
}

impl Display for Rank {
  fn show(&self) -> String {
    match self.0 {
      14 => "A".to_string(),
      13 => "K".to_string(),
      12 => "Q".to_string(),
      11 => "J".to_string(),
      10 => "T".to_string(),
      2..=9 => self.0.to_string(),
      _ => panic!("Invalid rank"),
    }
  }
}

impl PartialEq for Rank {
  fn eq(&self, other : &Rank) -> bool {
    self.0 == other.0
  }
}

impl Eq for Rank {}

impl PartialOrd for Rank {
  fn partial_cmp(&self, other : &Rank) -> Option<Ordering> {
    self.0.partial_cmp(&other.0)
  }
}

impl Ord for Rank {
  fn cmp(&self, other : &Rank) -> Ordering {
    self.0.cmp(&other.0)
  }
}

pub struct Ranks(u8);

impl Iterator for Ranks {
  type Item = Rank;

  fn next(&mut self) -> Option<Rank> {
    self.0 += 1;
    if self.0 < 15 {
      Some(Rank(self.0))
    } else {
      None
    }
  }
}

pub fn ranks() -> Ranks {
  Ranks(2)
}

pub struct Card(u8);

impl Card {
  pub fn new(suit: Suit, rank: Rank) -> Card {
    Card((suit.to_int() << 4) | rank.to_int())
  }

  pub fn suit(&self) -> Suit {
    Suit(self.0 >> 4)
  }

  pub fn rank(&self) -> Rank {
    Rank(self.0 & 0x0F)
  }
}

impl Clone for Card {
  fn clone(&self) -> Card {
    Card(self.0)
  }
}

impl Copy for Card {}

impl Display for Card {
  fn show(&self) -> String {
    format!("{}{}", self.suit().show(), self.rank().show())
  }

  fn display(&self) -> String {
    format!("{}{}", self.suit().display(), self.rank().display())
  }
}

impl Numeric<u8> for Card {
  fn to_int(&self) -> u8 {
    self.0
  }

  fn from_int(i: u8) -> Card {
    Card(i)
  }
}

impl PartialEq for Card {
  fn eq(&self, other : &Card) -> bool {
    self.0 == other.0
  }
}

impl Eq for Card {}

impl PartialOrd for Card {
  fn partial_cmp(&self, other : &Card) -> Option<Ordering> {
    self.0.partial_cmp(&other.0)
  }
}

impl Ord for Card {
  fn cmp(&self, other : &Card) -> Ordering {
    self.0.cmp(&other.0)
  }
}

pub struct Deck(u8);

impl Iterator for Deck {
  type Item = Card;

  fn next(&mut self) -> Option<Card> {
    self.0 += 1;
    if self.0 < 52 {
      Some(Card(self.0))
    } else {
      None
    }
  }
}

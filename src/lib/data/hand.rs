use num::FromPrimitive;
use rand::Rng;
use super::card::Card;
use super::display::Display;


#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Hand(u64);

pub struct IterHand {
  hand: Hand,
  mask: u64
}

impl Iterator for IterHand {
  type Item = Card;

  fn next(&mut self) -> Option<Card> {
    if self.mask == 0x0 {
      return None
    }
    if self.hand.0 & self.mask == 0 {
      self.mask >>= 1;
      self.next()
    } else {
      let card = self.mask.trailing_zeros() as u8;
      self.mask >>= 1;
      Some(FromPrimitive::from_u8(card).unwrap())
    }
  }
}

impl Hand {
  pub fn new() -> Hand {
    Hand(0) // empty
  }

  pub fn iter(&self) -> IterHand {
    IterHand {
      hand: *self,
      mask: 0x4000000000000000
    }
  }

  pub fn add(&mut self, card: Card) {
    self.0 |= 1 << card.to_u8() as u64;
  }

  pub fn has_card(&self, card: Card) -> bool {
    self.0 & (1 << card.to_u8() as u64) != 0
  }

  pub fn remove(&mut self, card: Card) {
    self.0 &= !(1 << card.to_u8() as u64);
  }
}


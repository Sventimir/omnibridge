use num_derive::FromPrimitive;
use num::FromPrimitive;
use rand::seq::SliceRandom;
use sexp;
use sexp::Sexp;

use super::card::{Card, Deck, Rank, Suit, SUITS};
use super::display::Display;
use super::hand_eval::*;
use super::holding::Holding;
use super::sexpable;
use super::sexpable::{Sexpable, SexpError};


/* Layout:
   _AKQ JT98 7654 32__ _AKQ JT98 7654 32__ _AKQ JT98 7654 32__ _AKQ JT98 7654 32__*/
#[derive(PartialEq, Eq, Clone, Copy, FromPrimitive, Debug)]
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

  pub fn length(&self) -> usize {
    self.0.count_ones() as usize
  }

  pub fn add(&mut self, card: &Card) {
    self.0 |= 1 << card.to_u8() as u64;
  }

  pub fn has_card(&self, card: &Card) -> bool {
    self.0 & (1 << card.to_u8() as u64) != 0
  }

  pub fn remove(&mut self, card: &Card) {
    self.0 &= !(1 << card.to_u8() as u64);
  }

  pub fn holding(&self, s : &Suit) -> Holding {
    match s {
      Suit::Club =>
        FromPrimitive::from_u64(self.0 & 0x0000000000007ffc).unwrap(),
      Suit::Diamond =>
        FromPrimitive::from_u64((self.0 & 0x000000007ffc0000) >> 16).unwrap(),
      Suit::Heart =>
        FromPrimitive::from_u64((self.0 & 0x00007ffc00000000) >> 32).unwrap(),
      Suit::Spade =>
        FromPrimitive::from_u64((self.0 & 0x7ffc000000000000) >> 48).unwrap(),
    }
  }

  pub fn random() -> Hand {
    let mut rng = rand::thread_rng();
    let mut hand = Hand::new();
    let mut deck = Deck::new().collect::<Vec<Card>>();
    deck.shuffle(&mut rng);
    for c in deck.iter().take(13) {
      hand.add(c);
    }
    hand
  }

  pub fn eval(&self) -> Eval {
    let mut hcp = Milton(0);
    let mut shape = Shape::empty();
    for card in self.iter() {
      hcp = hcp + hcp_per_rank(&card.rank());
      shape.add_card(&card.suit());
    }
    let mut dist_points = Milton::from_int(0);
    for l in shape.iter() {
      dist_points = dist_points + initial_dist_points_per_length(l);
    }
    Eval { hcp, dist_points, shape }
  }
}

impl Display for Hand {
  fn show(&self) -> String {
    let mut ret = String::with_capacity(self.length() + 12);
    for s in SUITS.iter().rev() {
      ret.push_str(&s.show());
      ret.push(' ');
      let h = self.holding(s);
      if h.length() == 0 {
        ret.push('-');
      } else {
        ret.push_str(h.show().as_str());
      }
      ret.push(' ');
    }
    ret.trim().to_string()
  }

  fn display(&self) -> String {
    let mut ret = String::with_capacity(self.length() + 12);
    for s in SUITS.iter().rev() {
      ret.push_str(&s.display());
      ret.push(' ');
      let h = self.holding(s);
      if h.length() == 0 {
        ret.push('-');
      } else {
        ret.push_str(h.display().as_str());
      }
      ret.push(' ');
    }
    ret.trim().to_string()
  }
}

impl Sexpable for Hand {
  fn to_sexp(&self) -> Sexp {
      let mut hand_sexp = Vec::with_capacity(4);
      let mut current_holding = Vec::with_capacity(13);
      let mut current_suit = Suit::Spade;
      for card in self.iter() {
          println!("Card: {:?}", card);
          if card.suit() == current_suit {
              current_holding.push(card.rank().to_sexp())
          } else {
              hand_sexp.push(sexp::list(&[
                  current_suit.to_sexp(),
                  sexp::list(&current_holding)
              ]));
              current_holding.clear();
              current_holding.push(card.rank().to_sexp());
              current_suit = card.suit();
          }
      }
      hand_sexp.push(sexp::list(&[
          current_suit.to_sexp(),
          sexp::list(&current_holding)
      ]));
      sexp::list(&hand_sexp)
  }

  fn from_sexp(sexp: &Sexp) -> Result<Hand, SexpError> {
      let holdings = sexpable::list(sexp)?;
      let mut hand = Hand::new();
      for h in holdings.iter() {
          let suit_and_holding = sexpable::list(h)?;
          if let [suit, holding] = suit_and_holding {
              let s = Suit::from_sexp(suit)?;
              let rs = sexpable::list(holding)?;
              for r in rs.iter() {
                  let rank = Rank::from_sexp(r)?;
                  hand.add(&Card::new(s, rank));
              }
          } else {
              return Err(SexpError::UnexpectedValue(&h))
          }
      }
      Ok(hand)
  }
}

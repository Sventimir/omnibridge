#[cfg(test)]
use num::FromPrimitive;
use super::super::card::*;
use super::super::display::*;
use super::super::hand::Hand;
use super::super::sexpable::Sexpable;

#[test]
fn display_empty_hand() {
  let hand = Hand::new();
  assert_eq!(hand.display(), "♠ - ♥ - ♦ - ♣ -");
}

#[test]
fn display_full_hand() {
  let mut hand = Hand::new();
  for card in Deck::new() {
    hand.add(&card);
  }
  assert_eq!(hand.display(), "♠ AKQJT98765432 ♥ AKQJT98765432 ♦ AKQJT98765432 ♣ AKQJT98765432");
}

#[test]
fn to_sexp_conv_roundtrip() {
    let hand = Hand::from_u64(0x70000fc00c06000).unwrap();
    let sexp = hand.to_sexp();
    assert_eq!(hand, Hand::from_sexp(&sexp).unwrap());
}

#[test]
fn from_sexp_conv_roundtrip() {
    let sexp = sexp::parse("((S (A K 4 3)) (H (K 2)) (D (A 10 9 8 7)) (C (A 2)))").unwrap();
    let hand = Hand::from_sexp(&sexp).unwrap();
    assert_eq!(sexp, hand.to_sexp());
}

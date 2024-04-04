#[cfg(test)]
use super::super::card::*;
use super::super::display::*;
use super::super::hand::Hand;

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

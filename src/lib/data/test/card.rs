#[cfg(test)]
// use num::FromPrimitive;
use super::super::card::*;
// use super::super::display::*;
// use super::super::holding::Holding;

#[test]
fn int_to_card_roundtrip() {
  for i in 0..52 {
    let card = Card::from_ord(i);
    assert_eq!(i, card.ord());
  }
}

#[test]
fn deck_to_ord_roundtrip() {
  for card in Deck::new() {
    let ord = card.ord();
    assert_eq!(card, Card::from_ord(ord));
  }
}

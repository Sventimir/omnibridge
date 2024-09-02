use crate::data::card::*;
use crate::data::hand::Hand;

#[test]
fn display_empty_hand() {
    let hand = Hand::new();
    assert_eq!(hand.to_string(), "♠ - ♥ - ♦ - ♣ -");
}

#[test]
fn display_full_hand() {
    let mut hand = Hand::new();
    for card in Deck::new() {
        hand.add(&card);
    }
    assert_eq!(
        hand.to_string(),
        "♠ AKQJT98765432 ♥ AKQJT98765432 ♦ AKQJT98765432 ♣ AKQJT98765432"
    );
}

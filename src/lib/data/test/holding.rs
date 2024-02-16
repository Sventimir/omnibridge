#[cfg(test)]
use super::super::card::*;
use super::super::display::*;
use super::super::numeric::*;
use super::super::holding::Holding;

#[test]
fn empty_has_no_high_cards() {
  let holding = Holding::new();
  assert!(!holding.contains_high_card());
}

#[test]
fn added_high_card_detected() {
  for rank in ranks().skip(9) {
    assert!(Holding::singleton(rank).contains_high_card())
  }
}

#[test]
fn no_high_cards_contained() {
  for rank in ranks().take(9) {
    assert!(!Holding::singleton(rank).contains_high_card())
  }
}

#[test]
fn many_high_cards_also_detected() {
  let mut holding = Holding::new();
  for rank in ranks().skip(9) {
    holding.add(rank);
  }
  assert!(holding.contains_high_card())
}

#[test]
fn no_sequence_in_empty_holding() {
  assert!(Holding::new().best_sequence().is_empty())
}

#[test]
fn two_consecutive_ranks_form_a_sequence() {
  let holding = Holding::from_iter(vec![FOUR, FIVE]);
  assert!(holding == holding.best_sequence())
}

#[test]
fn sequence_can_be_long() {
  let holding = Holding::from_iter(vec![JACK, TEN, NINE, EIGHT]);
  assert!(holding == holding.best_sequence())
}

#[test]
fn sequence_discards_other_cards() {
  let holding = Holding::from_iter(vec![ACE, QUEEN, JACK, TEN, SEVEN, FIVE]);
  let mut expected = holding.clone();
  expected.remove(ACE);
  expected.remove(SEVEN);
  expected.remove(FIVE);
  assert!(holding.best_sequence() == expected)
}

#[test]
fn of_two_or_more_sequences_highest_one_prevails() {
  let holding = Holding::from_iter(vec![ACE, QUEEN, JACK, SEVEN, SIX, FIVE]);
  let mut expected = holding.clone();
  expected.remove(ACE);
  expected.remove(SEVEN);
  expected.remove(SIX);
  expected.remove(FIVE);
  assert!(holding.best_sequence() == expected)
}

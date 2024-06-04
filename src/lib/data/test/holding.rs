use super::super::card::*;
use super::super::holding::Holding;

#[test]
fn empty_has_no_high_cards() {
    let holding = Holding::new();
    assert!(!holding.contains_high_card());
}

#[test]
fn added_high_card_detected() {
    for rank in 11..14 {
        assert!(Holding::singleton(Rank::from_int(rank)).contains_high_card())
    }
}

#[test]
fn no_high_cards_contained() {
    for rank in 2..9 {
        assert!(!Holding::singleton(Rank::from_int(rank)).contains_high_card())
    }
}

#[test]
fn many_high_cards_also_detected() {
    let mut holding = Holding::new();
    for rank in 11..14 {
        holding.add(Rank::from_int(rank));
    }
    assert!(holding.contains_high_card())
}

#[test]
fn no_sequence_in_empty_holding() {
    assert!(Holding::new().best_sequence().is_empty())
}

#[test]
fn no_sequence_in_singletons() {
    for rank in 2..14 {
        assert!(Holding::singleton(Rank::from_int(rank))
            .best_sequence()
            .is_empty())
    }
}

#[test]
fn two_consecutive_ranks_form_a_sequence() {
    let holding = Holding::from_iter(vec![Rank::Four, Rank::Five]);
    assert!(holding == holding.best_sequence())
}

#[test]
fn sequence_can_be_long() {
    let holding = Holding::from_iter(vec![Rank::Jack, Rank::Ten, Rank::Nine, Rank::Eight]);
    assert!(holding == holding.best_sequence())
}

#[test]
fn sequence_discards_other_cards() {
    let holding = Holding::from_iter(vec![
        Rank::Ace,
        Rank::Queen,
        Rank::Jack,
        Rank::Ten,
        Rank::Seven,
        Rank::Five,
    ]);
    let mut expected = holding.clone();
    expected.remove(Rank::Ace);
    expected.remove(Rank::Seven);
    expected.remove(Rank::Five);
    assert!(holding.best_sequence() == expected)
}

#[test]
fn of_two_or_more_sequences_highest_one_prevails() {
    let holding = Holding::from_iter(vec![
        Rank::Ace,
        Rank::Queen,
        Rank::Jack,
        Rank::Seven,
        Rank::Six,
        Rank::Five,
    ]);
    let mut expected = holding.clone();
    expected.remove(Rank::Ace);
    expected.remove(Rank::Seven);
    expected.remove(Rank::Six);
    expected.remove(Rank::Five);
    assert!(holding.best_sequence() == expected)
}

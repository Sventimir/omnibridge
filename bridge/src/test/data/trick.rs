use crate::data::{
    card::{Card, Rank, Suit},
    table::Dir,
    trick::Trick,
};

#[test]
fn fresh_trick_empty() {
    let t = Trick::new(&Dir::North);
    assert_eq!(t.card_played(&Dir::North), None);
    assert_eq!(t.card_played(&Dir::East), None);
    assert_eq!(t.card_played(&Dir::South), None);
    assert_eq!(t.card_played(&Dir::West), None);
}

#[test]
fn card_is_played() {
    let mut t = Trick::new(&Dir::North);
    let card = Card::new(Suit::Spade, Rank::Ace);
    t.play_card(&card);
    assert_eq!(t.card_played(&Dir::North), Some(card));
    assert_eq!(t.card_played(&Dir::East), None);
    assert_eq!(t.card_played(&Dir::South), None);
    assert_eq!(t.card_played(&Dir::West), None);
}

#[test]
fn at_the_begining_leader_is_to_play() {
    let t = Trick::new(&Dir::North);
    assert_eq!(t.leader(), Dir::North);
    assert_eq!(t.current_player(), Dir::North);
}

#[test]
fn after_card_is_played_current_player_changes() {
    let mut t = Trick::new(&Dir::North);
    t.play_card(&Card::new(Suit::Spade, Rank::Ace));
    assert_eq!(t.leader(), Dir::North);
    assert_eq!(t.current_player(), Dir::East);
}

#[test]
fn when_all_player_player_current_is_leader_and_all_cards_can_be_retrieved() {
    let mut t = Trick::new(&Dir::South);
    let card = Card::new(Suit::Heart, Rank::Queen);
    t.play_card(&card);
    t.play_card(&card);
    assert_eq!(t.current_player(), Dir::North);
    t.play_card(&card);
    t.play_card(&card);
    assert_eq!(t.leader(), Dir::South);
    assert_eq!(t.current_player(), t.leader());
    assert_eq!(t.card_played(&Dir::South), Some(card));
    assert_eq!(t.card_played(&Dir::West), Some(card));
    assert_eq!(t.card_played(&Dir::North), Some(card));
    assert_eq!(t.card_played(&Dir::East), Some(card))
}

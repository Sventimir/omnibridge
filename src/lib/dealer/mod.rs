use rand::seq::SliceRandom;

use crate::data::board::Board;
use crate::data::card::{Card, Deck};


pub fn deal(number: u8) -> Board {
    let mut rng = rand::thread_rng();
    let mut deck = Deck::new().collect::<Vec<Card>>();
    deck.shuffle(&mut rng);
    let mut board = Board::new(number);
    let mut hands = vec![
        &mut board.east,
        &mut board.south,
        &mut board.west
    ].into_iter();
    let mut current_hand = &mut board.north;
    let mut cards_dealt = 0;
    for card in deck.iter() {
        if cards_dealt < 13 {
            current_hand.add(card);
        } else {
            current_hand = hands.next().unwrap();
            cards_dealt = 0;
            current_hand.add(card);
        }
        cards_dealt += 1;
    }
    board
}

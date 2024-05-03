extern crate bridge;

use bridge::data::card::Card;
use bridge::data::hand::Hand;

fn main() {
    let mut hand = Hand::new();
    for i in 0..52 {
        let card = Card::from_ord(i);
        println!("{} : {} : {} : {}", i, card.to_u8(), card.ord(), card);
        hand.add(&card);
    }
    println!("****");
    for card in hand.iter() {
        let ord = card.ord();
        println!(
            "{} : {} : {} : {}",
            card.to_u8(),
            ord,
            Card::from_ord(ord).ord(),
            card
        );
    }
}

extern crate bridge;
mod bidding_state;

use bidding_state::*;
use bridge::data::bid::Bid;
use bridge::data::hand::Hand;
use bridge::data::table::{Dir, Vulnerability};

fn input_bid() -> Bid {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    match input.trim().parse::<Bid>() {
        Ok(c) => c,
        Err(_) => {
            println!("Invalid bid. Please try again.");
            input_bid()
        }
    }
}

fn show_expected_bids(expected: &ProposedBids) {
    if expected.is_empty() {
        return;
    }
    println!("Other options include:");
    for (bid, score) in expected.iter() {
        println!("* {}({})", bid, score);
    }
}

fn main() {
    let h = Hand::random();
    println!("Your hand: {}", h);
    let state = Opening::new(Dir::North, Vulnerability::None, h);
    let mut expected = state.next_bid();
    println!("What is your opening bid?");
    let bid = input_bid();
    match expected.pop_last() {
        Some((best, score)) if best == bid => {
            println!("Perfect, {} is the best bid!", bid);
            show_expected_bids(&expected);
            println!("Score: {}.", score)
        }
        Some((best, best_score)) => match expected.get(&bid) {
            Some(score) => {
                println!(
                    "Good bid. {} scores: {}.\n{}({}) was the best.",
                    bid,
                    score,
                    best,
                    best_score
                );
                show_expected_bids(&expected)
            }
            None => {
                println!(
                    "{} is not the best bid. {}({}) was best.",
                    bid,
                    best,
                    best_score
                );
                show_expected_bids(&expected);
                println!("Score: 0.")
            }
        },
        None => {
            println!("The engine does not know what to think of it :( Please go, talk to a human.")
        }
    }
}

use bridge::data::bid::{Bid, Call};
use bridge::data::card::Suit;
use bridge::data::hand::Hand;
use bridge::data::hand_eval::*;
use bridge::data::table::{Dir, Vulnerability};
use std::collections::BTreeMap;

pub type ProposedBids = BTreeMap<Bid, u64>;

/* next_bid() returns a mapping of proposed bids to scores
assigned to them by the engine. The higher the score, the
better the bid according to the engine's evaluation.

observe_bid() let's us feed the engine with information
coming from other participants of the bidding. */
pub trait BiddingState {
    fn next_bid(&self) -> ProposedBids;
    fn observe_bid(&mut self, dir: &Dir, bid: &Bid);
}

pub fn normalize_bid_scores(bids: ProposedBids) -> ProposedBids {
    let max_score: u64;
    {
        max_score = *bids.values().max().unwrap();
    }
    let mut bids = bids;
    for (_, score) in bids.iter_mut() {
        *score = *score * 100 / max_score;
    }
    bids
}

pub struct Opening {
    dir: Dir,
    // hand: Hand,
    // vuln: Vulnerability,
    hand_eval: Eval,
}

impl Opening {
    pub fn new(dir: Dir, _vuln: Vulnerability, hand: Hand) -> Opening {
        let hand_eval = hand.eval();
        Opening {
            dir,
            // hand,
            // vuln,
            hand_eval,
        }
    }
}

impl BiddingState for Opening {
    fn observe_bid(&mut self, dir: &Dir, bid: &Bid) {
        if self.dir == *dir {
            panic!("Cannot observe own bid!")
        }
        match bid {
            Bid::Pass => (),
            _ => panic!("Transition not implemented yet!"),
        }
    }

    fn next_bid(&self) -> ProposedBids {
        let mut response = BTreeMap::new();
        let total_points = self.hand_eval.total_points();
        let shape = self.hand_eval.shape.shape_type();
        if total_points < OPENING_THRESHOLD {
            response.insert(Bid::Pass, 1);
        }
        if total_points > GF_OPENING_THRESHOLD {
            response.insert(
                Bid::Call(Call {
                    level: 2,
                    trump: Some(Suit::Club),
                }),
                1,
            );
        }
        if shape == ShapeType::Balanced {
            if ONE_NO_TRUMP_RANGE.contains(&total_points) {
                response.insert(
                    Bid::Call(Call {
                        level: 1,
                        trump: None,
                    }),
                    10,
                );
            } else if TWO_NO_TRUMP_RANGE.contains(&total_points) {
                response.insert(
                    Bid::Call(Call {
                        level: 2,
                        trump: None,
                    }),
                    10,
                );
            }
        }
        match self.hand_eval.shape {
            Shape([_, _, _, 5..=13]) =>
            // 5+ Spades
            {
                response.insert(
                    Bid::Call(Call {
                        level: 1,
                        trump: Some(Suit::Spade),
                    }),
                    5,
                )
            }
            Shape([_, _, 5..=13, _]) =>
            // 5+ Hearts
            {
                response.insert(
                    Bid::Call(Call {
                        level: 1,
                        trump: Some(Suit::Heart),
                    }),
                    5,
                )
            }
            Shape([_, 4..=13, _, _]) =>
            // 4+ Diamonds
            {
                response.insert(
                    Bid::Call(Call {
                        level: 1,
                        trump: Some(Suit::Diamond),
                    }),
                    5,
                )
            }
            _ => response.insert(
                Bid::Call(Call {
                    level: 1,
                    trump: Some(Suit::Club),
                }),
                5,
            ),
        };
        normalize_bid_scores(response)
    }
}

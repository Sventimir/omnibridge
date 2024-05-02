use num::FromPrimitive;
use num_derive::FromPrimitive;
use rand::seq::SliceRandom;
use serde::ser::SerializeStruct;
use serde::{Deserialize, Serialize};
use std::fmt::{self, Debug, Display, Formatter};

use super::card::{Card, Deck, Suit, SUITS};
use super::hand_eval::*;
use super::holding::Holding;

/* Layout:
_AKQ JT98 7654 32__ _AKQ JT98 7654 32__ _AKQ JT98 7654 32__ _AKQ JT98 7654 32__*/
#[derive(PartialEq, Eq, Clone, Copy, FromPrimitive)]
pub struct Hand(u64);

pub struct IterHand {
    hand: Hand,
    mask: u64,
}

impl Iterator for IterHand {
    type Item = Card;

    fn next(&mut self) -> Option<Card> {
        if self.mask == 0x0 {
            return None;
        }
        if self.hand.0 & self.mask == 0 {
            self.mask >>= 1;
            self.next()
        } else {
            let card = self.mask.trailing_zeros() as u8;
            self.mask >>= 1;
            Some(FromPrimitive::from_u8(card).unwrap())
        }
    }
}

impl Hand {
    pub fn new() -> Hand {
        Hand(0) // empty
    }

    pub fn iter(&self) -> IterHand {
        IterHand {
            hand: *self,
            mask: 0x4000000000000000,
        }
    }

    pub fn length(&self) -> usize {
        self.0.count_ones() as usize
    }

    pub fn add(&mut self, card: &Card) {
        self.0 |= 1 << card.to_u8() as u64;
    }

    pub fn has_card(&self, card: &Card) -> bool {
        self.0 & (1 << card.to_u8() as u64) != 0
    }

    pub fn remove(&mut self, card: &Card) {
        self.0 &= !(1 << card.to_u8() as u64);
    }

    pub fn holding(&self, s: &Suit) -> Holding {
        match s {
            Suit::Club => FromPrimitive::from_u64(self.0 & 0x0000000000007ffc).unwrap(),
            Suit::Diamond => FromPrimitive::from_u64((self.0 & 0x000000007ffc0000) >> 16).unwrap(),
            Suit::Heart => FromPrimitive::from_u64((self.0 & 0x00007ffc00000000) >> 32).unwrap(),
            Suit::Spade => FromPrimitive::from_u64((self.0 & 0x7ffc000000000000) >> 48).unwrap(),
        }
    }

    pub fn random() -> Hand {
        let mut rng = rand::thread_rng();
        let mut hand = Hand::new();
        let mut deck = Deck::new().collect::<Vec<Card>>();
        deck.shuffle(&mut rng);
        for c in deck.iter().take(13) {
            hand.add(c);
        }
        hand
    }

    pub fn eval(&self) -> Eval {
        let mut hcp = Milton(0);
        let mut shape = Shape::empty();
        for card in self.iter() {
            hcp = hcp + hcp_per_rank(&card.rank());
            shape.add_card(&card.suit());
        }
        let mut dist_points = Milton::from_int(0);
        for l in shape.iter() {
            dist_points = dist_points + initial_dist_points_per_length(l);
        }
        Eval {
            hcp,
            dist_points,
            shape,
        }
    }
}

impl Debug for Hand {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for s in SUITS.iter().rev() {
            write!(f, "{:?} ", s)?;
            let h = self.holding(s);
            write!(f, "{:?}", h)?;
            if s != &Suit::Club {
                write!(f, " ")?;
            }
        }
        Ok(())
    }
}

impl Display for Hand {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for s in SUITS.iter().rev() {
            write!(f, "{} ", s)?;
            write!(f, "{}", self.holding(s))?;
            if s != &Suit::Club {
                write!(f, " ")?;
            }
        }
        Ok(())
    }
}

impl Serialize for Hand {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        let mut obj = serializer.serialize_struct("hand", 4)?;
        obj.serialize_field("C", &self.holding(&Suit::Club))?;
        obj.serialize_field("D", &self.holding(&Suit::Diamond))?;
        obj.serialize_field("H", &self.holding(&Suit::Heart))?;
        obj.serialize_field("S", &self.holding(&Suit::Spade))?;
        obj.end()
    }
}

struct HandVisitor;

impl<'de> serde::de::Visitor<'de> for HandVisitor {
    type Value = Hand;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a hand")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Hand, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let mut hand = Hand::new();
        while let Some((key, value)) = map.next_entry()? {
            let suit = match key {
                "C" => Suit::Club,
                "D" => Suit::Diamond,
                "H" => Suit::Heart,
                "S" => Suit::Spade,
                _ => return Err(serde::de::Error::custom("invalid suit")),
            };
            let holding: Holding = value;
            for rank in holding.iter() {
                hand.add(&Card::new(suit, rank));
            }
        }
        Ok(hand)
    }
}

impl<'de> Deserialize<'de> for Hand {
    fn deserialize<D>(deserializer: D) -> Result<Hand, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        deserializer.deserialize_map(HandVisitor)
    }
}

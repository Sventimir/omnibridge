use num::FromPrimitive;
use num_derive::FromPrimitive;
use serde::{Deserialize, Serialize};
use std::cmp::{Eq, Ord, PartialEq, PartialOrd};
use std::fmt::{self, Debug, Display, Formatter};
use std::str::FromStr;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, FromPrimitive)]
pub enum Suit {
    Club = 0,
    Diamond = 1,
    Heart = 2,
    Spade = 3,
}

impl Debug for Suit {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Suit::Club => write!(f, "C"),
            Suit::Diamond => write!(f, "D"),
            Suit::Heart => write!(f, "H"),
            Suit::Spade => write!(f, "S"),
        }
    }
}

impl Display for Suit {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Suit::Club => write!(f, "♣"),
            Suit::Diamond => write!(f, "♦"),
            Suit::Heart => write!(f, "♥"),
            Suit::Spade => write!(f, "♠"),
        }
    }
}

impl FromStr for Suit {
    type Err = ();

    fn from_str(s: &str) -> Result<Suit, ()> {
        match s {
            "C" | "c" | "♣" => Ok(Suit::Club),
            "D" | "d" | "♦" => Ok(Suit::Diamond),
            "H" | "h" | "♥" => Ok(Suit::Heart),
            "S" | "s" | "♠" => Ok(Suit::Spade),
            _ => Err(()),
        }
    }
}

impl Serialize for Suit {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Suit {
    fn deserialize<D>(deserializer: D) -> Result<Suit, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Suit::from_str(&s).map_err(|_| serde::de::Error::unknown_variant(&s, &["C", "D", "H", "S"]))
    }
}

pub const SUITS: [Suit; 4] = [Suit::Club, Suit::Diamond, Suit::Heart, Suit::Spade];

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, FromPrimitive)]
pub enum Rank {
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
    Seven = 7,
    Eight = 8,
    Nine = 9,
    Ten = 10,
    Jack = 11,
    Queen = 12,
    King = 13,
    Ace = 14,
}

impl Debug for Rank {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Rank::Ace => write!(f, "A"),
            Rank::King => write!(f, "K"),
            Rank::Queen => write!(f, "Q"),
            Rank::Jack => write!(f, "J"),
            Rank::Ten => write!(f, "T"),
            v => write!(f, "{}", *v as isize),
        }
    }
}

impl Display for Rank {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Rank::Ace => write!(f, "A"),
            Rank::King => write!(f, "K"),
            Rank::Queen => write!(f, "Q"),
            Rank::Jack => write!(f, "J"),
            Rank::Ten => write!(f, "T"),
            v => write!(f, "{}", *v as isize),
        }
    }
}

impl FromStr for Rank {
    type Err = ();

    fn from_str(s: &str) -> Result<Rank, ()> {
        match s {
            "A" | "a" => Ok(Rank::Ace),
            "K" | "k" => Ok(Rank::King),
            "Q" | "q" => Ok(Rank::Queen),
            "J" | "j" => Ok(Rank::Jack),
            "T" | "t" | "10" | "1" | "0" => Ok(Rank::Ten),
            "2" => Ok(Rank::Two),
            "3" => Ok(Rank::Three),
            "4" => Ok(Rank::Four),
            "5" => Ok(Rank::Five),
            "6" => Ok(Rank::Six),
            "7" => Ok(Rank::Seven),
            "8" => Ok(Rank::Eight),
            "9" => Ok(Rank::Nine),
            _ => Err(()),
        }
    }
}

impl Rank {
    pub fn from_int(i: u8) -> Rank {
        FromPrimitive::from_u8(i).unwrap()
    }
}

impl Serialize for Rank {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Rank {
    fn deserialize<D>(deserializer: D) -> Result<Rank, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Rank::from_str(&s).map_err(|_| {
            serde::de::Error::unknown_variant(
                &s,
                &[
                    "A", "K", "Q", "J", "T", "2", "3", "4", "5", "6", "7", "8", "9",
                ],
            )
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, FromPrimitive)]
pub struct Card(u8);

impl Card {
    pub fn new(suit: Suit, rank: Rank) -> Card {
        Card(((suit as u8) << 4) | (rank as u8))
    }

    pub fn suit(&self) -> Suit {
        FromPrimitive::from_u8(self.0 >> 4).unwrap()
    }

    pub fn rank(&self) -> Rank {
        FromPrimitive::from_u8(self.0 & 0x0F).unwrap()
    }

    pub fn ord(&self) -> u8 {
        let suit = self.0 >> 4;
        let rank = (self.0 & 0x0F) - 2;
        suit * 13 + rank
    }

    pub fn from_ord(ord: u8) -> Card {
        let suit = (ord / 13) << 4;
        let rank = (ord % 13) + 2;
        Card(suit | rank)
    }

    pub fn to_u8(&self) -> u8 {
        self.0
    }
}

impl Debug for Card {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{:?}{:?}", self.suit(), self.rank())
    }
}

impl Display for Card {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}{}", self.suit(), self.rank())
    }
}

impl FromStr for Card {
    type Err = ();

    fn from_str(s: &str) -> Result<Card, ()> {
        let mut chars = s.chars();
        let suit = match chars.next() {
            Some(c) => c.to_string(),
            None => return Err(()),
        };
        let rank = match chars.next() {
            Some(c) => c.to_string(),
            None => return Err(()),
        };
        let s = Suit::from_str(&suit)?;
        let r = Rank::from_str(&rank)?;
        Ok(Card::new(s, r))
    }
}

impl Serialize for Card {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Card {
    fn deserialize<D>(deserializer: D) -> Result<Card, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Card::from_str(&s).map_err(|_| {
            serde::de::Error::invalid_value(
                serde::de::Unexpected::Str(&s),
                &"a string in format: '<suit><rank>'",
            )
        })
    }
}

pub struct Deck(u8);

impl Iterator for Deck {
    type Item = Card;

    fn next(&mut self) -> Option<Card> {
        if self.0 < 52 {
            let c = Some(Card::from_ord(self.0));
            self.0 += 1;
            c
        } else {
            None
        }
    }
}

impl Deck {
    pub fn new() -> Deck {
        Deck(0)
    }
}

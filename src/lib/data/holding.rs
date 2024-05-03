use num::FromPrimitive;
use num_derive::FromPrimitive;
use rand::Rng;
use serde::ser::SerializeSeq;
use serde::{Deserialize, Serialize};
use sexp::{self, Sexp};
use std::fmt::{self, Debug, Display, Formatter};

use super::card::Rank;
use crate::sexpr::*;

/* Bits 2-14 tell if corresponding rank is a part of the holding.
The rest is unused. Layout:
_AKQ,JT98,7654,32__ */
#[derive(PartialEq, Eq, Clone, Copy, FromPrimitive)]
pub struct Holding(u16);

pub struct IterHolding {
    holding: Holding,
    mask: u16,
}

impl Iterator for IterHolding {
    type Item = Rank;

    fn next(&mut self) -> Option<Rank> {
        if self.mask < 0x0004 {
            return None;
        }
        if self.holding.0 & self.mask == 0 {
            self.mask >>= 1;
            self.next()
        } else {
            let rank = self.mask.trailing_zeros() as u8;
            self.mask >>= 1;
            Some(FromPrimitive::from_u8(rank).unwrap())
        }
    }
}

impl Holding {
    pub fn new() -> Holding {
        Holding(0)
    }

    // Iterates ranks from best to worst.
    pub fn iter(&self) -> IterHolding {
        IterHolding {
            holding: *self,
            mask: 0x4000, // Ace
        }
    }

    pub fn singleton(rank: Rank) -> Holding {
        Holding(1 << (rank as u8))
    }

    pub fn union(&self, other: &Holding) -> Holding {
        Holding(self.0 | other.0)
    }

    pub fn add(&mut self, rank: Rank) {
        self.0 |= 1 << (rank as u8)
    }

    pub fn remove(&mut self, rank: Rank) {
        self.0 &= !(1 << (rank as u8))
    }

    pub fn contains(&self, rank: Rank) -> bool {
        self.0 & (1 << (rank as u8)) != 0
    }

    // Returns true if the holding contains Ace, King, Queen or Jack.
    pub fn contains_high_card(&self) -> bool {
        self.0 & 0x7800 != 0
    }

    pub fn is_empty(&self) -> bool {
        self.0 == 0
    }

    pub fn length(&self) -> usize {
        self.0.count_ones() as usize
    }

    pub fn clear(&mut self) {
        self.0 = 0
    }

    pub fn random() -> Holding {
        Holding(rand::random::<u16>() & 0x7FFC) // clear unused bits
    }

    pub fn random_non_empty() -> Holding {
        Holding(rand::thread_rng().gen_range(1..0xFFFF) & 0x7FFC) // clear unused bits
    }

    pub fn best_sequence(&self) -> Holding {
        let h = self
            .iter()
            .fold(Holding::new(), |holding, rank| match rank {
                Rank::Ace => Holding::singleton(rank),
                _ if holding.contains(Rank::from_int((rank as u8) + 1)) => {
                    let mut h = holding.clone();
                    h.add(rank);
                    h
                }
                _ if holding.length() < 2 => Holding::singleton(rank),
                _ => holding,
            });
        if h.length() < 2 {
            Holding::new()
        } else {
            h
        }
    }
}

impl FromIterator<Rank> for Holding {
    fn from_iter<I: IntoIterator<Item = Rank>>(iter: I) -> Holding {
        let mut holding = Holding::new();
        for rank in iter {
            holding.add(rank)
        }
        holding
    }
}

impl Debug for Holding {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            write!(f, "-")
        } else {
            for r in self.iter() {
                write!(f, "{:?}", r)?;
            }
            Ok(())
        }
    }
}

impl Display for Holding {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            write!(f, "-")
        } else {
            for r in self.iter() {
                write!(f, "{}", r)?;
            }
            Ok(())
        }
    }
}

impl Sexpable for Holding {
    fn to_sexp(&self) -> Sexp {
        iter_into_sexp(self.iter())
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        iter_sexp(sexp)?.map(Rank::from_sexp).collect()
    }
}

impl Serialize for Holding {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.length()))?;
        for rank in self.iter() {
            seq.serialize_element(&rank)?;
        }
        seq.end()
    }
}

struct HoldingVisitor;

impl<'de> serde::de::Visitor<'de> for HoldingVisitor {
    type Value = Holding;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "a sequence of ranks")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Holding, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let mut holding = Holding::new();
        while let Some(rank) = seq.next_element()? {
            holding.add(rank);
        }
        Ok(holding)
    }
}

impl<'de> Deserialize<'de> for Holding {
    fn deserialize<D>(deserializer: D) -> Result<Holding, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_seq(HoldingVisitor)
    }
}

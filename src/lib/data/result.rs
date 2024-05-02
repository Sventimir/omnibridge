use std::fmt::{self, Debug, Display, Formatter};

use super::bid::{Call, Contract, Doubled};
use super::board;
use super::card::{Card, Suit};
use super::table::Dir;

pub struct ContractResult {
    pub board: u8,
    pub contract: Contract,
    pub lead: Option<Card>,
    // number of tricks relative to the contract level.
    pub tricks: i8,
}

fn undertricks_doubled(lacking_tricks: u8, vulnerable: bool) -> i16 {
    let mut tricks = lacking_tricks as i16;
    let mut score = if vulnerable {200} else {100};
    tricks -= 1;
    if tricks < 1 {
        return score;
    }
    score += (if vulnerable {300} else {200}) * tricks;
    tricks -= 2;
    if tricks < 1 {
        return score;
    }
    score += (if vulnerable {0} else {100}) * tricks;
    score
}

fn undertrick_score(lacking_tricks: u8, double: Doubled, vulnerable: bool) -> i16 {
    match double {
        Doubled::Undoubled => (if vulnerable { 100 } else { 50 }) * (lacking_tricks as i16),
        Doubled::Doubled => undertricks_doubled(lacking_tricks, vulnerable),
        Doubled::Redoubled => 2 * undertricks_doubled(lacking_tricks, vulnerable)
    }
}

fn trick_value(trump: Option<Suit>) -> i16 {
    match trump {
        None | Some(Suit::Spade | Suit::Heart) => 30,
        Some(Suit::Club | Suit::Diamond) => 20
    }
}

fn trick_score(call: Call) -> i16 {
    trick_value(call.trump) * call.level as i16
        + if call.trump.is_none() { 10 } else { 0 }
}

fn overtrick_score(contract: &Contract, tricks: u8, vulnerable: bool) -> i16 {
    match contract.doubled {
        Doubled::Undoubled => trick_value(contract.call.trump) * tricks as i16,
        Doubled::Doubled => (if vulnerable { 200 } else { 100 }) * tricks as i16,
        Doubled::Redoubled => (if vulnerable { 400 } else { 200 }) * tricks as i16
    }
}

fn slam_bonus(level: u8, vulnerable: bool) -> i16 {
    match level {
        6 => if vulnerable { 750 } else { 500 },
        7 => if vulnerable { 1500 } else { 1000 },
        _ => 0
    }
}

impl ContractResult {
    pub fn total_ticks(&self) -> u8 {
        ((self.contract.call.level as i8) + 6 + self.tricks) as u8
    }

    pub fn vulnerable(&self) -> bool {
        match self.contract.declarer {
            Dir::North | Dir::South =>
                board::vulnerability(&self.board) as u8 & 1 != 0,
            Dir::East | Dir::West =>
                board::vulnerability(&self.board) as u8 & 2 != 0
        }
    }

    pub fn score(&self) -> i16 {
        if self.tricks < 0 {
            undertrick_score(
                -self.tricks as u8,
                self.contract.doubled,
                self.vulnerable()
            )
        } else {
            let mut score = trick_score(self.contract.call);
            if score >= 100 {
                score += if self.vulnerable() { 500 } else { 300 }; 
            } else {
                score += 50;
            }
            score += overtrick_score(
                &self.contract,
                self.tricks as u8,
                self.vulnerable()
            );
            score += slam_bonus(
                self.contract.call.level,
                self.vulnerable()
            );
            match self.contract.doubled {
                Doubled::Undoubled => score,
                Doubled::Doubled => score + 50,
                Doubled::Redoubled => score + 100
            }
        }
    }
}

impl Debug for ContractResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {:?}", self.board, self.contract)?;
        match self.lead {
            None => Ok(()),
            Some(card) => write!(f, "{:?}", card)
        }?;
        write!(f, " {:+?}", self.tricks)
    }
}

impl Display for ContractResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:}: {:}", self.board, self.contract)?;
        match self.lead {
            None => Ok(()),
            Some(card) => write!(f, "{}", card)
        }?;
        write!(f, " {:+}", self.tricks)
    }
}

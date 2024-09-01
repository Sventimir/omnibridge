use serde::{Deserialize, Serialize};
use std::fmt::{self, Debug, Display, Formatter};

use crate::language::ast::expect::{self, ExpectError};
use crate::language::ast::AST;
use crate::language::{int, nil, IntoSexp, Sexp};

use super::bid::{Call, Contract, Doubled};
use super::board;
use super::card::{Card, Suit};
use super::scoring::{Scorable, Score};
use super::table::Dir;

#[derive(Serialize, Deserialize)]
pub struct ContractResult {
    pub board: u8,
    pub contract: Contract,
    pub lead: Option<Card>,
    // number of tricks relative to the contract level.
    pub tricks: i8,
}

fn undertricks_doubled(lacking_tricks: u8, vulnerable: bool) -> i16 {
    let mut tricks = lacking_tricks as i16;
    let mut score = if vulnerable { 200 } else { 100 };
    tricks -= 1;
    if tricks < 1 {
        return score;
    }
    score += (if vulnerable { 300 } else { 200 }) * tricks;
    tricks -= 2;
    if tricks < 1 {
        return score;
    }
    score += (if vulnerable { 0 } else { 100 }) * tricks;
    score
}

fn undertrick_score(lacking_tricks: u8, double: &Doubled, vulnerable: bool) -> i16 {
    match double {
        Doubled::Undoubled => (if vulnerable { 100 } else { 50 }) * (lacking_tricks as i16),
        Doubled::Doubled => undertricks_doubled(lacking_tricks, vulnerable),
        Doubled::Redoubled => 2 * undertricks_doubled(lacking_tricks, vulnerable),
    }
}

fn trick_value(trump: &Option<Suit>) -> i16 {
    match trump {
        None | Some(Suit::Spade | Suit::Heart) => 30,
        Some(Suit::Club | Suit::Diamond) => 20,
    }
}

fn trick_score(call: &Call) -> i16 {
    trick_value(&call.trump) * call.level as i16 + if call.trump.is_none() { 10 } else { 0 }
}

fn overtrick_score(call: &Call, doubled: &Doubled, tricks: u8, vulnerable: bool) -> i16 {
    match doubled {
        Doubled::Undoubled => trick_value(&call.trump) * tricks as i16,
        Doubled::Doubled => (if vulnerable { 200 } else { 100 }) * tricks as i16,
        Doubled::Redoubled => (if vulnerable { 400 } else { 200 }) * tricks as i16,
    }
}

fn slam_bonus(level: u8, vulnerable: bool) -> i16 {
    match level {
        6 => {
            if vulnerable {
                750
            } else {
                500
            }
        }
        7 => {
            if vulnerable {
                1500
            } else {
                1000
            }
        }
        _ => 0,
    }
}

fn score_base(call: &Call, doubled: &Doubled, vulnerable: bool, tricks: i8) -> i16 {
    if tricks < 0 {
        -undertrick_score(-tricks as u8, doubled, vulnerable)
    } else {
        let mut score = trick_score(call);
        score *= match doubled {
            Doubled::Undoubled => 1,
            Doubled::Doubled => 2,
            Doubled::Redoubled => 4,
        };
        score += if score >= 100 {
            if vulnerable {
                500
            } else {
                300
            }
        } else {
            50
        };
        score += overtrick_score(call, doubled, tricks as u8, vulnerable);
        score += slam_bonus(call.level, vulnerable);
        match doubled {
            Doubled::Undoubled => score,
            Doubled::Doubled => score + 50,
            Doubled::Redoubled => score + 100,
        }
    }
}

impl ContractResult {
    pub fn total_ticks(&self) -> u8 {
        match self.contract {
            Contract::Passed => 0,
            Contract::Contract { call, .. } => ((call.level as i8) + 6 + self.tricks) as u8,
        }
    }
}

impl Scorable for ContractResult {
    fn score(&self) -> Score {
        match self.contract {
            Contract::Passed => Score::ZERO,
            Contract::Contract {
                call,
                doubled,
                declarer,
            } => {
                let (vulnerability_mask, side_multiplier) = match declarer {
                    Dir::North | Dir::South => (1, 1),
                    Dir::East | Dir::West => (2, -1),
                };
                let vulnerable = vulnerability_mask & board::vulnerability(&self.board) as u8 != 0;
                let score = score_base(&call, &doubled, vulnerable, self.tricks);
                Score::from_i16(score * side_multiplier)
            }
        }
    }
}

impl Debug for ContractResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {:?}", self.board, self.contract)?;
        match self.lead {
            None => Ok(()),
            Some(card) => write!(f, "{:?}", card),
        }?;
        write!(f, " {:+?}", self.tricks)
    }
}

impl Display for ContractResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:}: {:}", self.board, self.contract)?;
        match self.lead {
            None => Ok(()),
            Some(card) => write!(f, "{}", card),
        }?;
        write!(f, " {:+}", self.tricks)
    }
}

impl IntoSexp for ContractResult {
    fn into_sexp<S: Sexp>(self) -> S {
        S::list(vec![
            S::nat(self.board as u64),
            self.contract.into_sexp(),
            self.lead.map(Card::into_sexp).unwrap_or(nil()),
            int(self.tricks as i64),
        ])
    }
}

impl<M: Clone> TryFrom<&AST<M>> for ContractResult {
    type Error = ExpectError<M>;

    fn try_from(ast: &AST<M>) -> Result<ContractResult, Self::Error> {
        let contents = expect::list(&ast)?;
        match contents {
            [b, c, l, t] => {
                let board = expect::nat(b)?;
                let contract = Contract::try_from(c)?;
                let lead = expect::optional(&Card::try_from, l)?;
                let tricks = expect::int(t)?;
                Ok(ContractResult {
                    board: board as u8,
                    contract,
                    lead,
                    tricks: tricks as i8,
                })
            }
            _ => Err(ExpectError::WrongLength(4, contents.to_vec())),
        }
    }
}

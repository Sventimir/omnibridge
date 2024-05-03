use crate::sexpr::*;
use super::card::Suit;
use super::table::Dir;

use serde::{Serialize, Deserialize};
use sexp::{self, Sexp};
use std::cmp::Ordering;
use std::fmt::{self, Debug, Display, Formatter};
use std::str::FromStr;

#[derive(PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub struct Call {
    pub trump: Option<Suit>,
    pub level: u8,
}

impl Debug for Call {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.level)?;
        match self.trump {
            None => write!(f, "NT"),
            Some(suit) => {
                write!(f, "{} ", suit)
            }
        }
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.level)?;
        match self.trump {
            None => write!(f, "NT"),
            Some(suit) => {
                write!(f, "{} ", suit)
            }
        }
    }
}

impl FromStr for Call {
    type Err = String;

    fn from_str(s: &str) -> Result<Call, String> {
        let mut chars = s.chars();
        let level = chars.next().unwrap().to_digit(10).unwrap() as u8;
        if level > 7 {
            return Err("Invalid level".to_string());
        }
        let suit = match chars.next() {
            Some('C' | 'c') => Some(Suit::Club),
            Some('D' | 'd') => Some(Suit::Diamond),
            Some('H' | 'h') => Some(Suit::Heart),
            Some('S' | 's') => Some(Suit::Spade),
            Some('N' | 'n') => None,
            _ => return Err("Invalid suit".to_string()),
        };
        Ok(Call { trump: suit, level })
    }
}

fn trump_to_sexp(trump: &Option<Suit>) -> Sexp {
    match trump {
        None => sexp::atom_s("NT"),
        Some(suit) => suit.to_sexp(),
    }
}

fn trump_from_sexp(sexp: &Sexp) -> Result<Option<Suit>, SexpError> {
    let s = expect_string(sexp)?;
    match s {
        "NT" => Ok(None),
        _ => Suit::from_str(&s).map_err(|()| SexpError::InvalidTag(s.to_string())).map(Some),
    }
}

impl Sexpable for Call {
    fn to_sexp(&self) -> Sexp {
        sexp::list(&[
            sexp::atom_i(self.level as i64),
            trump_to_sexp(&self.trump)
        ])
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        let (l, t): (u64, Sexp) = Sexpable::from_sexp(sexp)?;
        let level = l as u8;
        let trump = trump_from_sexp(&t)?;
        Ok(Call { level, trump })
    }
}

#[derive(PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Bid {
    Pass,
    Double,
    Redouble,
    Call(Call),
}

impl Bid {
    /* Check if the bid can be made by the declarer given the current
    contract. Return the new contract if so, or None otherwise. */
    pub fn apply(&self, declarer: &Dir, contract: Contract) -> Option<Contract> {
        match self {
            Bid::Pass => Some(contract),
            Bid::Double
                if contract.doubled == Doubled::Undoubled
                    && contract.declarer.opponent_of(declarer) =>
            {
                Some(Contract {
                    doubled: Doubled::Doubled,
                    ..contract
                })
            }
            Bid::Redouble
                if contract.doubled == Doubled::Doubled
                    && (!contract.declarer.opponent_of(declarer)) =>
            {
                Some(Contract {
                    doubled: Doubled::Redoubled,
                    ..contract
                })
            }
            Bid::Call(call) if call > &contract.call => Some(Contract {
                call: *call,
                declarer: *declarer,
                doubled: Doubled::Undoubled,
            }),
            _ => None,
        }
    }
}

impl FromStr for Bid {
    type Err = String;

    fn from_str(s: &str) -> Result<Bid, String> {
        match s {
            "Pass" | "p" => Ok(Bid::Pass),
            "Dbl" | "x" => Ok(Bid::Double),
            "Rdbl" | "xx" => Ok(Bid::Redouble),
            _ => Call::from_str(s).map(Bid::Call),
        }
    }
}

impl Debug for Bid {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pass => write!(f, "pass"),
            Self::Double => write!(f, "x"),
            Self::Redouble => write!(f, "xx"),
            Self::Call(call) => write!(f, "{}", call)
        }
    }
}

impl Display for Bid {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pass => write!(f, "pass"),
            Self::Double => write!(f, "x"),
            Self::Redouble => write!(f, "xx"),
            Self::Call(call) => write!(f, "{}", call)
        }
    }
}

impl Sexpable for Bid {
    fn to_sexp(&self) -> Sexp {
        sexp::atom_s(&self.to_string())
    }

    fn from_sexp(sexp: &Sexp) -> Result<Self, SexpError> {
        let s = expect_string(sexp)?;
        Bid::from_str(s).map_err(|_| SexpError::InvalidTag(s.to_string()))
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub enum Doubled {
    Undoubled,
    Doubled,
    Redoubled,
}

impl Debug for Doubled {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Undoubled => Ok(()),
            Self::Doubled => write!(f, "x"),
            Self::Redoubled => write!(f, "xx")
        }
    }
}

impl Display for Doubled {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, f)
    }
}

impl FromStr for Doubled {
    type Err = String;

    fn from_str(s: &str) -> Result<Doubled, String> {
        match s {
            "" => Ok(Doubled::Undoubled),
            "x" => Ok(Doubled::Doubled),
            "xx" => Ok(Doubled::Redoubled),
            _ => Err("Invalid doubled".to_string()),
        }
    }
}

impl Sexpable for Doubled {
    fn to_sexp(&self) -> Sexp {
        sexp::atom_s(&self.to_string())
    }

    fn from_sexp(sexp: &Sexp) -> Result<Doubled, SexpError> {
        expect_nil(sexp).map(|()| Doubled::Undoubled)
            .or_else(|_| {
                let s = expect_string(sexp)?;
                Doubled::from_str(s).map_err(|_| SexpError::InvalidTag(s.to_string()))
            })
    }
}

#[derive(PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct Contract {
    pub declarer: Dir,
    pub call: Call,
    pub doubled: Doubled,
}

impl Contract {
    pub fn new(call: Call, doubled: Doubled, decl: Dir) -> Contract {
        Contract {
            call,
            doubled,
            declarer: decl,
        }
    }

    pub fn as_sexp_list(&self) -> Vec<Sexp> {
        let mut contents = Vec::with_capacity(4);
        contents.push((self.call.level as u64).to_sexp());
        contents.push(trump_to_sexp(&self.call.trump));
        match self.doubled {
            Doubled::Undoubled => (),
            d => contents.push(d.to_sexp())
        };
        contents.push(self.declarer.to_sexp());
        contents
    }
}

impl Debug for Contract {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}", self.call, self.doubled, self.declarer)
    }
}

impl Display for Contract {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, f)
    }
}

impl Sexpable for Contract {
    fn to_sexp(&self) -> Sexp {
        sexp::list(self.as_sexp_list().as_slice())
    }

    fn from_sexp(sexp: &Sexp) -> Result<Contract, SexpError> {
        let l = expect_list(sexp)?;
        match l {
            [lvl, tr, dbl, dcl] => {
                let level: u64 = Sexpable::from_sexp(lvl)?;
                Ok(Contract {
                    call: Call { level: level as u8, trump: trump_from_sexp(tr)? },
                    doubled: Doubled::from_sexp(dbl)?,
                    declarer: Dir::from_sexp(dcl)?
                })
            },
            [lvl, tr, dcl] => {
                let level: u64 = Sexpable::from_sexp(lvl)?;
                Ok(Contract {
                    call: Call { level: level as u8, trump: trump_from_sexp(tr)? },
                    doubled: Doubled::Undoubled,
                    declarer: Dir::from_sexp(dcl)?
                })
            },
            _ => Err(SexpError::InvalidValue(sexp.clone(), "contract".to_string()))
        }
    }
}

impl PartialOrd for Call {
    fn partial_cmp(&self, other: &Call) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Call {
    fn cmp(&self, other: &Call) -> Ordering {
        if self.level == other.level {
            match (self.trump, other.trump) {
                (None, None) => Ordering::Equal,
                (None, Some(_)) => Ordering::Greater,
                (Some(_), None) => Ordering::Less,
                (Some(s1), Some(s2)) => s1.cmp(&s2),
            }
        } else {
            self.level.cmp(&other.level)
        }
    }
}

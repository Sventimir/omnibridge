use super::card::Suit;
use super::table::Dir;
use language::{
    self,
    ast::{
        expect::{self, ExpectError},
        AST,
    },
    IntoSexp, Sexp,
};

use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt::{self, Debug, Display, Formatter};
use std::str::FromStr;

#[derive(PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub struct Bid {
    pub trump: Option<Suit>,
    pub level: u8,
}

impl Debug for Bid {
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

impl Display for Bid {
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

impl FromStr for Bid {
    type Err = String;

    fn from_str(s: &str) -> Result<Bid, String> {
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
        Ok(Bid { trump: suit, level })
    }
}

fn trump_to_sexp<S>(trump: &Option<Suit>) -> S
where
    S: Sexp,
{
    match trump {
        None => S::symbol("NT".to_string()),
        Some(suit) => suit.clone().into_sexp(),
    }
}

fn trump_from_sexp<M: Clone>(ast: &AST<M>) -> Result<Option<Suit>, ExpectError<M>> {
    let s = expect::symbol(&ast)?;
    match s {
        "NT" | "nt" => Ok(None),
        _ => Suit::from_str(&s)
            .map_err(|()| ExpectError::InvalidSymbol(s.to_string(), ast.meta().clone()))
            .map(Some),
    }
}

impl IntoSexp for Bid {
    fn into_sexp<S: Sexp>(self) -> S {
        S::list(vec![S::nat(self.level as u64), trump_to_sexp(&self.trump)])
    }
}

impl<M: Clone> TryFrom<&AST<M>> for Bid {
    type Error = ExpectError<M>;

    fn try_from(ast: &AST<M>) -> Result<Self, Self::Error> {
        let (l, t) = expect::pair(ast)?;
        let level = expect::nat(l)? as u8;
        let trump: Option<Suit> = trump_from_sexp(t)?;
        Ok(Bid { level, trump })
    }
}

#[derive(PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Call {
    Pass,
    Double,
    Redouble,
    Bid(Bid),
}

impl Call {
    /* Check if the bid can be made by the declarer given the current
    contract. Return the new contract if so, or None otherwise. */
    pub fn apply(&self, bidder: &Dir, contract: Contract) -> Option<Contract> {
        match (self, &contract) {
            (Call::Pass, _) => Some(contract),
            (
                Call::Double,
                Contract::Contract {
                    doubled: Doubled::Undoubled,
                    call,
                    declarer,
                },
            ) if declarer.opponent_of(bidder) => Some(Contract::Contract {
                call: *call,
                doubled: Doubled::Doubled,
                declarer: *declarer,
            }),
            (
                Call::Redouble,
                Contract::Contract {
                    doubled: Doubled::Doubled,
                    call,
                    declarer,
                },
            ) if (!declarer.opponent_of(bidder)) => Some(Contract::Contract {
                call: *call,
                doubled: Doubled::Redoubled,
                declarer: *declarer,
            }),
            (Call::Bid(call), Contract::Passed) => Some(Contract::Contract {
                call: *call,
                doubled: Doubled::Undoubled,
                declarer: *bidder,
            }),
            (Call::Bid(overcall), Contract::Contract { call, .. }) if overcall > &call => {
                Some(Contract::Contract {
                    call: *overcall,
                    doubled: Doubled::Undoubled,
                    declarer: *bidder,
                })
            }
            _ => None,
        }
    }
}

impl FromStr for Call {
    type Err = String;

    fn from_str(s: &str) -> Result<Call, String> {
        match s {
            "Pass" | "p" => Ok(Call::Pass),
            "Dbl" | "x" => Ok(Call::Double),
            "Rdbl" | "xx" => Ok(Call::Redouble),
            _ => Bid::from_str(s).map(Call::Bid),
        }
    }
}

impl Debug for Call {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pass => write!(f, "pass"),
            Self::Double => write!(f, "x"),
            Self::Redouble => write!(f, "xx"),
            Self::Bid(call) => write!(f, "{}", call),
        }
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pass => write!(f, "pass"),
            Self::Double => write!(f, "x"),
            Self::Redouble => write!(f, "xx"),
            Self::Bid(call) => write!(f, "{}", call),
        }
    }
}

impl IntoSexp for Call {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            Self::Pass => S::symbol("pass".to_string()),
            Self::Double => S::symbol("dbl".to_string()),
            Self::Redouble => S::symbol("rdbl".to_string()),
            Self::Bid(call) => call.into_sexp(),
        }
    }
}

impl<M: Clone> TryFrom<&AST<M>> for Call {
    type Error = ExpectError<M>;

    fn try_from(ast: &AST<M>) -> Result<Self, Self::Error> {
        match ast {
            AST::Symbol { content, .. } => match content.as_str() {
                "pass" | "PASS" => Ok(Call::Pass),
                "dbl" | "DBL" => Ok(Call::Double),
                "rdbl" | "RDBL" => Ok(Call::Redouble),
                _ => Err(ExpectError::InvalidSymbol(
                    content.clone(),
                    ast.meta().clone(),
                )),
            },
            _ => Bid::try_from(ast).map(Call::Bid),
        }
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
            Self::Redoubled => write!(f, "xx"),
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
            "x" | "X" => Ok(Doubled::Doubled),
            "xx" | "XX" => Ok(Doubled::Redoubled),
            _ => Err("Invalid doubled".to_string()),
        }
    }
}

impl IntoSexp for Doubled {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            Doubled::Undoubled => language::nil(),
            Doubled::Doubled => S::symbol("x".to_string()),
            Doubled::Redoubled => S::symbol("xx".to_string()),
        }
    }
}

impl<M: Clone> TryFrom<&AST<M>> for Doubled {
    type Error = ExpectError<M>;

    fn try_from(ast: &AST<M>) -> Result<Doubled, Self::Error> {
        expect::nil(ast).map(|()| Doubled::Undoubled).or_else(|_| {
            let s = expect::symbol(&ast)?;
            Doubled::from_str(s)
                .map_err(|_| ExpectError::InvalidSymbol(s.to_string(), ast.meta().clone()))
        })
    }
}

#[derive(PartialEq, Eq, Clone, Serialize, Deserialize)]
pub enum Contract {
    Passed,
    Contract {
        declarer: Dir,
        call: Bid,
        doubled: Doubled,
    },
}

impl Contract {
    pub fn new(call: Bid, doubled: Doubled, decl: Dir) -> Contract {
        Contract::Contract {
            call,
            doubled,
            declarer: decl,
        }
    }
}

impl Debug for Contract {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Contract::Passed => write!(f, "pass"),
            Contract::Contract {
                call,
                doubled,
                declarer,
            } => write!(f, "{}{}{}", call, doubled, declarer),
        }
    }
}

impl Display for Contract {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, f)
    }
}

impl IntoSexp for Contract {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            Self::Passed => language::nil(),
            Self::Contract {
                call,
                doubled,
                declarer,
            } => S::list(vec![
                S::nat(call.level as u64),
                trump_to_sexp(&call.trump),
                doubled.into_sexp(),
                declarer.into_sexp(),
            ]),
        }
    }
}

impl<M: Clone> TryFrom<&AST<M>> for Contract {
    type Error = ExpectError<M>;

    fn try_from(ast: &AST<M>) -> Result<Contract, Self::Error> {
        let l = expect::list(&ast)?;
        match l {
            [] => Ok(Contract::Passed),
            [lvl, tr, dbl, dcl] => {
                let level: u64 = expect::nat(lvl)?;
                Ok(Contract::Contract {
                    call: Bid {
                        level: level as u8,
                        trump: trump_from_sexp(&tr)?,
                    },
                    doubled: Doubled::try_from(dbl)?,
                    declarer: Dir::try_from(dcl)?,
                })
            }
            [lvl, tr, dcl] => {
                let level: u64 = expect::nat(lvl)?;
                Ok(Contract::Contract {
                    call: Bid {
                        level: level as u8,
                        trump: trump_from_sexp(&tr)?,
                    },
                    doubled: Doubled::Undoubled,
                    declarer: Dir::try_from(dcl)?,
                })
            }
            _ => Err(ExpectError::WrongLength(4, l.to_vec(), ast.meta().clone())),
        }
    }
}

impl PartialOrd for Bid {
    fn partial_cmp(&self, other: &Bid) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Bid {
    fn cmp(&self, other: &Bid) -> Ordering {
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

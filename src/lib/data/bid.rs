use super::card::Suit;
use super::table::Dir;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Display, Formatter};
use std::str::FromStr;

#[derive(PartialEq, Eq, Clone, Copy)]
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

#[derive(PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
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

#[derive(PartialEq, Eq, Clone, Copy)]
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

#[derive(PartialEq, Eq, Clone)]
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

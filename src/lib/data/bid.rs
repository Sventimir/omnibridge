use std::cmp::Ordering;
use std::str::FromStr;
use super::card::Suit;
use super::display::Display;
use super::table::Dir;


#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Call {
  pub trump : Option<Suit>,
  pub level: u8
}

#[derive(PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum Bid {
  Pass,
  Double,
  Redouble,
  Call(Call)
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Doubled {
  Undoubled,
  Doubled,
  Redoubled
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Contract {
  pub declarer: Dir,
  pub call: Call,
  pub doubled: Doubled
}

impl FromStr for Call {
  type Err = String;

  fn from_str(s: &str) -> Result<Call, String> {
    let mut chars = s.chars();
    let level = chars.next().unwrap().to_digit(10).unwrap() as u8;
    if level > 7 { return Err("Invalid level".to_string()) }
    let suit = match chars.next() {
      Some('C' | 'c') => Some(Suit::Club),
      Some('D' | 'd') => Some(Suit::Diamond),
      Some('H' | 'h') => Some(Suit::Heart),
      Some('S' | 's') => Some(Suit::Spade),
      Some('N' | 'n') => None,
      _ => return Err("Invalid suit".to_string())
    };
    Ok(Call { trump: suit, level })
  }
}

impl Bid {
  /* Check if the bid can be made by the declarer given the current
     contract. Return the new contract if so, or None otherwise. */
  pub fn apply(&self, declarer: &Dir, contract: Contract) -> Option<Contract> {
    match self {
      Bid::Pass => Some(contract),
      Bid::Double if contract.doubled == Doubled::Undoubled
                     && contract.declarer.opponent_of(declarer) =>
        Some(Contract { doubled: Doubled::Doubled, .. contract }),
      Bid::Redouble if contract.doubled == Doubled::Doubled
                       && (! contract.declarer.opponent_of(declarer)) =>
        Some(Contract { doubled: Doubled::Redoubled, .. contract }),
      Bid::Call(call) if call > &contract.call =>
        Some(Contract { call: *call, declarer: *declarer, doubled: Doubled::Undoubled }),
      _ => None
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
      _ => Call::from_str(s).map(Bid::Call)
    }
  }
}

impl Display for Call {
  fn show(&self) -> String {
    match self.trump {
      None => format!("{}NT", self.level),
      Some(suit) => format!("{}{}", self.level, suit.show())
    }
  }

  fn display(&self) -> String {
    match self.trump {
      None => format!("{}NT", self.level),
      Some(suit) => format!("{}{}", self.level, suit.display())
    }
  }
}

impl Display for Bid {
  fn show(&self) -> String {
    match self {
      Bid::Pass => "Pass".to_string(),
      Bid::Double => "Dbl".to_string(),
      Bid::Redouble => "Rdbl".to_string(),
      Bid::Call(call) => call.show()
    }
  }

  fn display(&self) -> String {
    match self {
      Bid::Pass => "Pass".to_string(),
      Bid::Double => "x".to_string(),
      Bid::Redouble => "xx".to_string(),
      Bid::Call(call) => call.display()
    }
  }
}

impl Display for Doubled {
  fn show(&self) -> String {
    match self {
      Doubled::Undoubled => "".to_string(),
      Doubled::Doubled => "Dbl".to_string(),
      Doubled::Redoubled => "Rdbl".to_string()
    }
  }

  fn display(&self) -> String {
    match self {
      Doubled::Undoubled => "".to_string(),
      Doubled::Doubled => "x".to_string(),
      Doubled::Redoubled => "xx".to_string()
    }
  }
}

impl Display for Contract {
  fn show(&self) -> String {
    format!("{}{}{}", self.call.show(), self.doubled.show(), self.declarer.show())
  }

  fn display(&self) -> String {
    format!("{}{}{}", self.call.display(), self.doubled.display(), self.declarer.display())
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
        (Some(s1), Some(s2)) => s1.cmp(&s2)
      }
    } else {
      self.level.cmp(&other.level)
    }
  }
}

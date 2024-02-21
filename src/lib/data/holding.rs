use num::FromPrimitive;
use rand::Rng;
use super::card::Rank;
use super::display::Display;

/* Bits 2-14 tell if corresponding rank is a part of the holding.
   The rest is unused. */
#[derive(PartialEq, Eq)]
pub struct Holding(u16);

impl Clone for Holding {
  fn clone(&self) -> Holding {
    Holding(self.0)
  }
}

impl Copy for Holding {}

pub struct IterHolding {
  holding: Holding,
  mask: u16
}

impl Iterator for IterHolding {
  type Item = Rank;

  fn next(&mut self) -> Option<Rank> {
    if self.mask < 0x0004 {
      return None
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
      mask: 0x4000 // Ace
    }
  }

  pub fn singleton(rank: Rank) -> Holding {
    Holding(1 << (rank as u8))
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

  pub fn len(&self) -> u8 {
    self.0.count_ones() as u8
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
    self.iter().fold(Holding::new(), |holding, rank| {
      match rank {
        Rank::Ace => Holding::singleton(rank),
        _ if holding.contains(Rank::from_int((rank as u8) + 1)) => {
          let mut h = holding.clone();
          h.add(rank);
          h
        },
        _ if holding.len() < 2 => {
          Holding::singleton(rank)
        },
        _ => holding
      }
    })
  }
}

impl FromIterator<Rank> for Holding {
  fn from_iter<I: IntoIterator<Item=Rank>>(iter: I) -> Holding {
    let mut holding = Holding::new();
    for rank in iter {
      holding.add(rank)
    }
    holding
  }
}

impl Display for Holding {
  fn show(&self) -> String {
    let mut acc = String::new();
    for r in self.iter() {
      acc.push_str(&r.show());
    }
    acc
  }

  fn display(&self) -> String {
    let mut acc = String::new();
    for r in self.iter() {
      acc.push_str(&r.display());
    }
    acc
  }
}

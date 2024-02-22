use num::FromPrimitive;

use super::card::Rank;
use super::table::Dir;
use super::display::Display;

/* Rightmost 4 bits represent N's card, next 4 represent E's card and so on. */
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct SuitlessTrick(u16);

impl SuitlessTrick {
  pub fn new() -> SuitlessTrick {
    SuitlessTrick(0)
  }

  fn shift(dir: Dir) -> u16 {
    4 * (dir as u16)
  }

  fn mask_of(dir: Dir) -> u16 {
    0xf << SuitlessTrick::shift(dir)
  }

  fn card_of(dir: Dir, rank: Rank) -> u16 {
    (rank as u16) << SuitlessTrick::shift(dir)
  }

  pub fn played_by(&self, dir: Dir) -> Option<Rank> {
    FromPrimitive::from_u16(SuitlessTrick::mask_of(dir) >> SuitlessTrick::shift(dir))
  }

  pub fn with_played(&self, dir: Dir, rank: Rank) -> Result<SuitlessTrick, Rank> {
    match self.played_by(dir) {
      None => Ok(SuitlessTrick(self.0 | SuitlessTrick::card_of(dir, rank))),
      Some(r) => Err(r)
    }
  }
}

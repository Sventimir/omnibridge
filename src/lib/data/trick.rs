use std::num::Wrapping;
use num::FromPrimitive;

use super::card::Card;
use super::table::Dir;

/* Layout:
   ____ |__|__| ____ __|__ ____| ____ __|__ ____
   idle   l  c   card    card     card   l. card
 where l = leader and c = curent */
pub struct Trick(u32);

impl Trick {
    pub fn new(leader: &Dir) -> Trick {
        Trick(((*leader as u32) << 24) | ((*leader as u32) << 26))
    }

    pub fn leader(&self) -> Dir {
        FromPrimitive::from_u32(self.0 >> 26 & 0x4).expect("Invalid trick leader")
    }

    pub fn current_player(&self) -> Dir {
        FromPrimitive::from_u32(self.0 >> 24 & 0x4).expect("Invalid trick current player")
    }

    pub fn card_played(&self, player: &Dir) -> Option<Card> {
        let leader = Wrapping(self.leader() as u32);
        let player = Wrapping(*player as u32);
        let diff = (player - leader).0 % 4;
        let shift = diff * 6;
        let mask = 0x3f << shift;
        let c = ((self.0 & mask) >> shift) as u8;
        // Sadly, FromPrimitive does not understand which values are valid.
        if c == 0 { None } else { FromPrimitive::from_u8(c) }
    }

    pub fn play_card(&mut self, card: &Card) {
        let leader = Wrapping(self.leader() as u32);
        let player = Wrapping(self.current_player() as u32);
        let diff = (player - leader).0 % 4;
        let shift = diff * 6;
        self.0 |= (card.to_u8() as u32) << shift
    }
}

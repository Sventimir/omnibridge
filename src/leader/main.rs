extern crate bridge;

use bridge::data::display::Display;
use bridge::data::card::{Rank, ranks};
use rand::prelude::*;

fn show_holding(holding : &[Rank]) -> String {
  let mut s = String::new();
  for r in holding {
    s.push_str(&format!("{}", r.display()));
  }
  s
}

fn main() {
  let mut rng = thread_rng();
  let mut ranks : Vec<Rank> = ranks().collect();
  ranks.shuffle(&mut rng);
  let length = rng.gen_range(2..13);
  let mut holding : Vec<Rank> = ranks.iter().take(length).map(|r| *r).collect();
  holding.sort_by_key(|&r| std::cmp::Reverse(r));
  let holding = holding;
  println!("Hello, Leader!\nThis is your suit holding: {}.\nWhich card do you lead?", show_holding(&holding));
}

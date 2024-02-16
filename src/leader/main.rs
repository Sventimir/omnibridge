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

fn input_lead() -> Rank {
  let mut input = String::new();
  std::io::stdin().read_line(&mut input).unwrap();
  match input.trim().parse::<Rank>() {
    Ok(c) => c,
    Err(_) => {
      println!("Invalid card. Please try again.");
      input_lead()
    }
  }
}

fn is_lead_correct(lead : Rank, holding : &[Rank]) -> Option<&Rank> {
  if holding.contains(&lead) {
    None
  } else {
    holding.first()
  }
}

fn main() {
  let mut score = 0;
  let mut max_score = 0;
  let mut rng = thread_rng();
  println!("Hello! Let's check your leading skills!");
  loop {
    let mut ranks : Vec<Rank> = ranks().collect();
    ranks.shuffle(&mut rng);
    let length = rng.gen_range(2..13);
    let mut holding : Vec<Rank> = ranks.iter().take(length).map(|r| *r).collect();
    holding.sort_by_key(|&r| std::cmp::Reverse(r));
    let holding = holding;
    println!(
      "This is your suit holding: {}.\nWhich card do you lead?",
      show_holding(&holding)
    );
    let lead = input_lead();
    match is_lead_correct(lead, &holding) {
      None => {
        println!("Correct!");
        score += 1;
      },
      Some(r) =>
        println!("Incorrect! You should have led a {}.", r.display())
    }
    max_score += 1;
    println!("Your score is: {}/{}", score, max_score);
  }
}

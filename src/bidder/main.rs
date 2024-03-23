extern crate bridge;

use bridge::data::display::Display;
use bridge::data::hand::Hand;


fn main() {
  let h = Hand::random();
  println!("Your hand: {}", h.display());
  println!("What is your opening bid?");
}

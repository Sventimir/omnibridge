extern crate bridge;

use bridge::data::display::Display;
use bridge::data::hand::Hand;


fn main() {
  let h = Hand::random();
  let eval = h.eval();
  println!("Your hand: {}", h.display());
  println!("HCP: {}; distr. points: {}; shape: {}",
           eval.hcp.display(),
           eval.dist_points.display(),
           eval.shape.shape_type().display()
  );
  println!("What is your opening bid?");
}

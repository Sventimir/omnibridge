extern crate bridge;

use bridge::data::sexpable::Sexpable;


fn main() {
    let board = bridge::dealer::deal(1);
    println!("{}", board.to_sexp())
}

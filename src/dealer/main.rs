extern crate bridge;
extern crate clap;

use bridge::data::sexpable::Sexpable;
use bridge::dealer::deal;


fn main() {
    let matches = clap::command!()
        .arg(clap::arg!([board] "Board numbers to generate")
             .value_parser(clap::value_parser!(u8))
             .num_args(0..))
        .get_matches();
    match matches.get_many::<u8>("board") {
        Some(boards) => {
            for b in boards {
                let board = deal(*b);
                println!("{}", board.to_sexp())
            }
        },
        None => {
            let board = deal(1);
            println!("{}", board.to_sexp())
        }
    }
}

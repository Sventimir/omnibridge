use crate::data::{
    bid::{Bid, Contract, Doubled},
    card::Suit,
    result::ContractResult,
    scoring::{Scorable, Score},
    table::Dir,
};
use language::{test_utils::exec, IntoSexp};

quickcheck! {
    #[ignore]
    fn test_score_computation(
        board: u8,
        level: u8,
        trump: Option<Suit>,
        declarer: Dir,
        dbl: Doubled,
        tricks: u8
    ) -> bool {
        let result = exec(
            &format!(
                "(score {} {} {} {} {} {})",
                board.into_sexp::<String>(),
                level.into_sexp::<String>(),
                match trump {
                    Some(suit) => suit.into_sexp::<String>(),
                    None => "()".to_string(),
                },
                declarer.into_sexp::<String>(),
                dbl.into_sexp::<String>(),
                tricks.into_sexp::<String>(),
            )
        );
        let contract_result = ContractResult {
            board,
            contract: Contract::new(Bid { level, trump }, dbl, declarer),
            lead: None,
            tricks: tricks as i8 - (level as i8 + 6),
        };
        result.value::<Score>().unwrap() == contract_result.score()
    }
}

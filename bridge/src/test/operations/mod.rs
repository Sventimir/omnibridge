use crate::data::{
    bid::{Bid, Contract, Doubled},
    card::Suit,
    result::ContractResult,
    scoring::{Scorable, Score},
    table::Dir,
};
use language::{test_utils::exec, IntoSexp};
use proptest::prelude::{Just, Strategy};

#[allow(dead_code)]
fn arb_trump() -> impl Strategy<Value = Option<Suit>> {
    prop_oneof![
        Just(Some(Suit::Club)),
        Just(Some(Suit::Diamond)),
        Just(Some(Suit::Heart)),
        Just(Some(Suit::Spade)),
        Just(None),
    ]
}

proptest! {
    #[ignore]
    fn test_score_computation(
        board in 0..32u8,
        level in 1..7u8,
        trump in arb_trump(),
        declarer in prop_oneof![
            Just(Dir::North),
            Just(Dir::East),
            Just(Dir::South),
            Just(Dir::West),
        ],
        dbl in prop_oneof![
            Just(Doubled::Undoubled),
            Just(Doubled::Doubled),
            Just(Doubled::Redoubled),
        ],
        tricks in 0..13u8
    ) {
        let result: Score = exec(
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
        assert_eq!(result, contract_result.score())
    }
}

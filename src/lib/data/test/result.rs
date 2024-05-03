#![allow(unused_imports)]
use crate::data::result::*;
use crate::sexpr::*;

#[cfg(test)]
fn sexp_contract(s: &str) -> ContractResult  {
    ContractResult::from_sexp(&sexp::parse(s).unwrap()).unwrap()
}

#[test]
fn test_passed_board_scores_zero() {
    let contract = sexp_contract("(1 pass 0)");
    assert_eq!(contract.score(), 0)
}

use std::ops::Range;

use crate::data::result::*;
use crate::data::scoring::{Scorable, Score};
use crate::language::{self, ast::AST};

type Meta = Range<usize>;

fn sexp_contract(s: &str) -> ContractResult {
    let exprs = language::parse::<AST<Meta>>(s).unwrap();
    let ast = exprs.get(0).unwrap();
    ContractResult::try_from(ast).unwrap()
}

#[test]
fn test_passed_board_scores_zero() {
    let contract = sexp_contract("(1 () () 0)");
    assert_eq!(contract.score(), Score::ZERO)
}

#[test]
fn test_major_partscore_by_ns() {
    let contract = sexp_contract("(1 (2 S N) () 0)");
    assert_eq!(contract.score(), Score::from_i16(110))
}

#[test]
fn test_minor_partscore_by_we() {
    let contract = sexp_contract("(1 (2 C W) () 0)");
    assert_eq!(contract.score(), Score::from_i16(-90))
}

#[test]
fn test_notrump_score_with_overtricks() {
    let contract = sexp_contract("(1 (2 NT N) () 2)");
    assert_eq!(contract.score(), Score::from_i16(180))
}

#[test]
fn test_vulnerable_partscore() {
    let contract = sexp_contract("(4 (2 NT S) () 2)");
    assert_eq!(contract.score(), Score::from_i16(180))
}

#[test]
fn test_invulnerable_game() {
    let contract = sexp_contract("(1 (3 NT N) () 0)");
    assert_eq!(contract.score(), Score::from_i16(400))
}

#[test]
fn test_vulerable_game() {
    let contract = sexp_contract("(4 (3 NT E) () 0)");
    assert_eq!(contract.score(), Score::from_i16(-600))
}

#[test]
fn test_partscore_doubled_making_game() {
    let contract = sexp_contract("(1 (2 S x N) () 0)");
    assert_eq!(contract.score(), Score::from_i16(470))
}

#[test]
fn test_partsocre_doubled() {
    let contract = sexp_contract("(1 (1 S X N) () 1)");
    assert_eq!(contract.score(), Score::from_i16(260))
}

#[test]
fn test_game_doubled() {
    let contract = sexp_contract("(3 (4 S x W) () 0)");
    assert_eq!(contract.score(), Score::from_i16(-790))
}

#[test]
fn test_minor_slam() {
    let contract = sexp_contract("(1 (6 D N) () 0)");
    assert_eq!(contract.score(), Score::from_i16(920))
}

#[test]
fn test_major_grand_slam() {
    let contract = sexp_contract("(4 (7 C E) () 0)");
    assert_eq!(contract.score(), Score::from_i16(-2140))
}

#[test]
fn test_simple_undertrick() {
    let contract = sexp_contract("(1 (3 S N) () (- 1))");
    assert_eq!(contract.score(), Score::from_i16(-50))
}

#[test]
fn test_vulnerable_undertrick() {
    let contract = sexp_contract("(3 (3 S E) () (- 1))");
    assert_eq!(contract.score(), Score::from_i16(100))
}

#[test]
fn test_many_undertricks() {
    let contract = sexp_contract("(3 (3 S E) () (- 4))");
    assert_eq!(contract.score(), Score::from_i16(400))
}

#[test]
fn test_invulnerable_doubled_undertricks() {
    let contract = sexp_contract("(1 (3 S x N) () (- 2))");
    assert_eq!(contract.score(), Score::from_i16(-300));
    let contract = sexp_contract("(1 (3 S x N) () (- 5))");
    assert_eq!(contract.score(), Score::from_i16(-1100))
}

#[test]
fn test_vulnerable_doubled_undertricks() {
    let contract = sexp_contract("(3 (3 S x E) () (- 2))");
    assert_eq!(contract.score(), Score::from_i16(500));
    let contract = sexp_contract("(3 (3 S x E) () (- 5))");
    assert_eq!(contract.score(), Score::from_i16(1400))
}

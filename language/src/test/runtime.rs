use crate::{instr, program::Program, var::Var};

#[test]
fn not_instr_flips_bool() {
    let prog = Program::new();
    let arg: Var = Var::new(false);
    let (not_instr, result) = instr::bool::not(arg.clone());
    prog.push_instr(not_instr);
    prog.exec();
    assert_eq!(result.value(), Some(true));
}

quickcheck! {
    fn de_morgan_law_holds(x: bool, y: bool) -> bool {
        let prog1 = Program::new();
        let a = Var::new(x);
        let b = Var::new(y);
        let (and_instr, a_and_b) = instr::bool::and(a.clone(), b.clone());
        prog1.push_instr(and_instr);
        let (not_instr, result1) = instr::bool::not(a_and_b);
        prog1.push_instr(not_instr);
        prog1.exec();

        // Note that we cen reuse vars from prog1 in prog2.
        // This may (and likely will) change in the future, though.
        let prog2 = Program::new();
        let (not_a_instr, not_a) = instr::bool::not(a.clone());
        prog2.push_instr(not_a_instr);
        let (not_b_instr, not_b) = instr::bool::not(b.clone());
        prog2.push_instr(not_b_instr);
        let (or_instr, result2) = instr::bool::or(not_a, not_b);
        prog2.push_instr(or_instr);
        prog2.exec();

        result1.value::<bool>() == result2.value()
    }
}

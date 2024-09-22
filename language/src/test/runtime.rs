use crate::{instr, program::Program, var::Var};

#[test]
fn not_instr_flips_bool() {
    let mut prog = Program::new();
    let arg: Var = prog.alloc(false);
    let (not_instr, result) = instr::bool::not(&mut prog, arg.clone());
    prog.push_instr(not_instr);
    prog.exec();
    assert_eq!(result.value(), Some(true));
}

proptest! {
    #[test]
    fn de_morgan_law_holds(x: bool, y: bool) {
        let mut prog1 = Program::new();
        let a = prog1.alloc(x);
        let b = prog1.alloc(y);
        let (and_instr, a_and_b) = instr::binop::and(&mut prog1, a.clone(), b.clone());
        prog1.push_instr(and_instr);
        let (not_instr, result1) = instr::bool::not(&mut prog1, a_and_b);
        prog1.push_instr(not_instr);
        prog1.exec();

        // Note that we cen reuse vars from prog1 in prog2.
        // This may (and likely will) change in the future, though.
        let mut prog2 = Program::new();
        let (not_a_instr, not_a) = instr::bool::not(&mut prog2, a.clone());
        prog2.push_instr(not_a_instr);
        let (not_b_instr, not_b) = instr::bool::not(&mut prog2, b.clone());
        prog2.push_instr(not_b_instr);
        let (or_instr, result2) = instr::binop::or(&mut prog2, not_a, not_b);
        prog2.push_instr(or_instr);
        prog2.exec();

        assert_eq!(result1.value::<bool>(), result2.value())
    }
}

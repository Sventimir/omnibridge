use crate::program::{Program, Var};

#[test]
fn not_instr_flips_bool() {
    let prog = Program::new();
    let arg: Var = prog.alloc(vec![0]);
    let result = prog.push_instr_not(&arg);
    prog.exec();
    assert_eq!(result.value(), vec![255]);
}

#[test]
fn de_morgan_law_test() {
    let prog1 = Program::new();
    let a = prog1.alloc(vec![255]);
    let b = prog1.alloc(vec![0]);
    let a_and_b = prog1.push_instr_and(&a, &b);
    let result1 = prog1.push_instr_not(&a_and_b);
    prog1.exec();

    // Note that we cen reuse vars from prog1 in prog2.
    // This may (and likely will) change in the future, though.
    let prog2 = Program::new();
    let not_a = prog2.push_instr_not(&a);
    let not_b = prog2.push_instr_not(&b);
    let result2 = prog2.push_instr_or(&not_a, &not_b);
    prog2.exec();

    assert_eq!(result1.value(), result2.value());
}

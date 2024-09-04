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
fn instrs_can_depend_on_each_ther() {
    let prog = Program::new();
    let arg1 = prog.alloc(vec![0]);
    let arg2 = prog.alloc(vec![255]);
    let neg_arg1 = prog.push_instr_not(&arg1);
    let result = prog.push_instr_and(&neg_arg1, &arg2);
    prog.exec();
    println!("{:?}", prog);
    assert_eq!(result.value(), vec![255]);
}

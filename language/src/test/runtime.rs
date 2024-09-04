use crate::program::{Instr, Program};

#[test]
fn not_instr_flips_bool() {
    let mut prog = Program::new();
    let arg: u8 = 0;
    let instr = Instr::Not { arg: &arg, result: 0 };
    prog.push_instr(instr);
    prog.exec();
    assert_eq!(*prog.result().unwrap(), 255);
}

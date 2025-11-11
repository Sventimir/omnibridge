use std::sync::Arc;

use crate::{builtin_instr::BuiltinInstr, test_utils::bool_prog};

#[test]
fn not_instr_flips_bool() {
    let prog = bool_prog(vec![BuiltinInstr::Push(Arc::new(false)), BuiltinInstr::Not]);
    assert_eq!(prog.eval::<String>(), "t".to_string());
}

proptest! {
    #[test]
    fn de_morgan_law_holds(x: bool, y: bool) {
        let prog1 = bool_prog(vec![
            BuiltinInstr::Push(Arc::new(y)),
            BuiltinInstr::Push(Arc::new(x)),
            BuiltinInstr::And(2),
            BuiltinInstr::Not,
        ]);

        let prog2 = bool_prog(vec![
            BuiltinInstr::Push(Arc::new(y)),
            BuiltinInstr::Not,
            BuiltinInstr::Push(Arc::new(x)),
            BuiltinInstr::Not,
            BuiltinInstr::Or(2),
        ]);

        assert_eq!(prog1.eval::<String>(), prog2.eval::<String>())
    }
}

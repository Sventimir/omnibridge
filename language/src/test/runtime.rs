use std::{any::Any, sync::Arc};

use crate::{builtin_instr::BuiltinInstr, interpreter, test_utils::get_result};


#[test]
fn not_instr_flips_bool() {
    let prog = vec![
        BuiltinInstr::Push(Arc::new(false)),
        BuiltinInstr::Not,
    ];
    let stack: Vec<Arc<dyn Any>> = interpreter::execute(prog.as_slice());
    assert_eq!(get_result::<bool>(stack.as_slice()), true);
}

proptest! {
    #[test]
    fn de_morgan_law_holds(x: bool, y: bool) {
        let prog1 = vec![
            BuiltinInstr::Push(Arc::new(y)),
            BuiltinInstr::Push(Arc::new(x)),
            BuiltinInstr::And(2),
            BuiltinInstr::Not,
        ];
        let stack1 = interpreter::execute(prog1.as_slice());
        
        let prog2 = vec![
            BuiltinInstr::Push(Arc::new(y)),
            BuiltinInstr::Not,
            BuiltinInstr::Push(Arc::new(x)),
            BuiltinInstr::Not,
            BuiltinInstr::Or(2),
        ];
        let stack2 = interpreter::execute(prog2.as_slice());
        
        assert_eq!(get_result::<bool>(&stack1), get_result::<bool>(&stack2))
    }
}

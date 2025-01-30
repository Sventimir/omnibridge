use std::collections::HashMap;

use crate::{type_checker::Environment, type_var::TypeVar};

#[derive(Debug)]
pub struct Value<T, I> {
    pub ty: fn() -> TypeVar<T>,
    pub prog: fn(&T) -> Vec<I>,
}

pub struct Env<T, I> {
    vars: HashMap<String, Value<T, I>>,
}

impl<T, I> Env<T, I> {
    pub fn new() -> Self {
        Env {
            vars: HashMap::new(),
        }
    }
}

impl<T, I> Environment<T, I> for Env<T, I> {
    fn type_of(&self, name: &str) -> Option<TypeVar<T>> {
        self.vars.get(name).map(|v| (v.ty)())
    }

    fn get_instr(&self, name: &str, ty: &T) -> Option<Vec<I>> {
        self.vars.get(name).map(|v| (v.prog)(ty))
    }
}

mod built_in {
    use std::sync::Arc;

    use crate::{builtin_instr::BuiltinInstr, builtin_type::BuiltinType, type_var::TypeVar};

    use super::{Env, Value};

    impl Env<BuiltinType, BuiltinInstr> {
        pub fn init(&mut self) {
            self.vars.insert(
                "t".to_string(),
                Value {
                    ty: || TypeVar::constant(BuiltinType::Bool),
                    prog: |_| vec![BuiltinInstr::Push(Arc::new(true))],
                },
            );
            self.vars.insert(
                "f".to_string(),
                Value {
                    ty: || TypeVar::constant(BuiltinType::Bool),
                    prog: |_| vec![BuiltinInstr::Push(Arc::new(false))],
                },
            );
            self.vars.insert(
                "nil".to_string(),
                Value {
                    ty: || TypeVar::constant(BuiltinType::Nil),
                    prog: |_| vec![BuiltinInstr::Push(Arc::new(false))],
                },
            );
            self.vars.insert(
                "not".to_string(),
                Value {
                    ty: || {
                        TypeVar::constant(BuiltinType::Fun {
                            args: vec![TypeVar::constant(BuiltinType::Bool)],
                            ret: Box::new(TypeVar::constant(BuiltinType::Bool)),
                        })
                    },
                    prog: |_| vec![BuiltinInstr::Not],
                },
            );
            self.vars.insert(
                "and".to_string(),
                Value {
                    ty: || {
                        TypeVar::constant(BuiltinType::Fun {
                            args: vec![
                                TypeVar::constant(BuiltinType::Bool),
                                TypeVar::constant(BuiltinType::Bool),
                            ],
                            ret: Box::new(TypeVar::constant(BuiltinType::Bool)),
                        })
                    },
                    prog: |_| vec![BuiltinInstr::And(2)],
                },
            );
            self.vars.insert(
                "or".to_string(),
                Value {
                    ty: || {
                        TypeVar::constant(BuiltinType::Fun {
                            args: vec![
                                TypeVar::constant(BuiltinType::Bool),
                                TypeVar::constant(BuiltinType::Bool),
                            ],
                            ret: Box::new(TypeVar::constant(BuiltinType::Bool)),
                        })
                    },
                    prog: |_| vec![BuiltinInstr::Or(2)],
                },
            );
            self.vars.insert(
                "+".to_string(),
                Value {
                    ty: || {
                        TypeVar::constant(BuiltinType::Fun {
                            args: vec![
                                TypeVar::constant(BuiltinType::Int),
                                TypeVar::constant(BuiltinType::Int),
                            ],
                            ret: Box::new(TypeVar::constant(BuiltinType::Int)),
                        })
                    },
                    prog: |_| vec![BuiltinInstr::Add(2)],
                },
            );
            self.vars.insert(
                "*".to_string(),
                Value {
                    ty: || {
                        TypeVar::constant(BuiltinType::Fun {
                            args: vec![
                                TypeVar::constant(BuiltinType::Int),
                                TypeVar::constant(BuiltinType::Int),
                            ],
                            ret: Box::new(TypeVar::constant(BuiltinType::Int)),
                        })
                    },
                    prog: |_| vec![BuiltinInstr::Mul(2)],
                },
            );
            self.vars.insert(
                "=".to_string(),
                Value {
                    ty: || {
                        TypeVar::constant(BuiltinType::Fun {
                            args: vec![
                                TypeVar::constant(BuiltinType::Int),
                                TypeVar::constant(BuiltinType::Int),
                            ],
                            ret: Box::new(TypeVar::constant(BuiltinType::Int)),
                        })
                    },
                    prog: |_| vec![BuiltinInstr::Eq],
                },
            );
            self.vars.insert(
                "id".to_string(),
                Value {
                    ty: || {
                        let tvar = TypeVar::unknown();
                        TypeVar::constant(BuiltinType::Fun {
                            args: vec![tvar.make_ref()],
                            ret: Box::new(tvar),
                        })
                    },
                    prog: |_| vec![],
                },
            );
        }
    }
}

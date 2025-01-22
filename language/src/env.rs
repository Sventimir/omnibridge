pub mod old {
    use std::collections::HashMap;

    use crate::{
        instr,
        program::Program,
        typed::{Type, TypeConstr, TypePrimitive},
        var::Var,
    };

    #[derive(Debug)]
    pub struct Value {
        pub ty: Type,
        pub constr: fn(prog: &mut Program, args: &[Var]) -> Var,
    }

    pub struct Env {
        vars: HashMap<String, Value>,
    }

    impl Env {
        pub fn new() -> Self {
            Env {
                vars: HashMap::new(),
            }
        }

        pub fn get(&self, name: &str) -> Option<&Value> {
            self.vars.get(name)
        }

        pub fn ty(&self, name: &str) -> Option<Type> {
            self.get(name).map(|v| v.ty.clone())
        }

        pub fn initialize(&mut self) {
            self.vars.insert(
                "t".to_string(),
                Value {
                    ty: Type(TypeConstr::Prim(TypePrimitive::Bool)),
                    constr: |prog, _| prog.alloc(true),
                },
            );
            self.vars.insert(
                "f".to_string(),
                Value {
                    ty: Type(TypeConstr::Prim(TypePrimitive::Bool)),
                    constr: |prog, _| prog.alloc(false),
                },
            );
            self.vars.insert(
                "nil".to_string(),
                Value {
                    ty: Type(TypeConstr::Prim(TypePrimitive::Nil)),
                    constr: |prog, _| prog.alloc(()),
                },
            );
            self.vars.insert(
                "not".to_string(),
                Value {
                    ty: Type::func(
                        &[TypeConstr::Prim(TypePrimitive::Bool)],
                        TypeConstr::Prim(TypePrimitive::Bool),
                    ),
                    constr: |prog, args| {
                        let (instr, ret) = instr::bool::not(prog, args[0].clone());
                        prog.push_instr(instr);
                        ret
                    },
                },
            );
            self.vars.insert(
                "and".to_string(),
                Value {
                    ty: Type::func(
                        &[
                            TypeConstr::Prim(TypePrimitive::Bool),
                            TypeConstr::Prim(TypePrimitive::Bool),
                        ],
                        TypeConstr::Prim(TypePrimitive::Bool),
                    ),
                    constr: |prog, args| {
                        let (instr, ret) =
                            instr::binop::and(prog, args[0].clone(), args[1].clone());
                        prog.push_instr(instr);
                        ret
                    },
                },
            );
            self.vars.insert(
                "or".to_string(),
                Value {
                    ty: Type::func(
                        &[
                            TypeConstr::Prim(TypePrimitive::Bool),
                            TypeConstr::Prim(TypePrimitive::Bool),
                        ],
                        TypeConstr::Prim(TypePrimitive::Bool),
                    ),
                    constr: |prog, args| {
                        let (instr, ret) = instr::binop::or(prog, args[0].clone(), args[1].clone());
                        prog.push_instr(instr);
                        ret
                    },
                },
            );
            self.vars.insert(
                "+".to_string(),
                Value {
                    ty: Type::func(
                        &[
                            TypeConstr::Prim(TypePrimitive::Decimal),
                            TypeConstr::Prim(TypePrimitive::Decimal),
                        ],
                        TypeConstr::Prim(TypePrimitive::Decimal),
                    ),
                    constr: |prog, args| {
                        let (instr, ret) =
                            instr::binop::add(prog, args[0].clone(), args[1].clone());
                        prog.push_instr(instr);
                        ret
                    },
                },
            );
            self.vars.insert(
                "*".to_string(),
                Value {
                    ty: Type::func(
                        &[
                            TypeConstr::Prim(TypePrimitive::Decimal),
                            TypeConstr::Prim(TypePrimitive::Decimal),
                        ],
                        TypeConstr::Prim(TypePrimitive::Decimal),
                    ),
                    constr: |prog, args| {
                        let (instr, ret) =
                            instr::binop::mul(prog, args[0].clone(), args[1].clone());
                        prog.push_instr(instr);
                        ret
                    },
                },
            );
            self.vars.insert(
                "=".to_string(),
                Value {
                    ty: Type::func(
                        &[
                            TypeConstr::Prim(TypePrimitive::Decimal),
                            TypeConstr::Prim(TypePrimitive::Decimal),
                        ],
                        TypeConstr::Prim(TypePrimitive::Decimal),
                    ),
                    constr: |prog, args| {
                        let (instr, ret) =
                            instr::binop::equal(prog, args[0].clone(), args[1].clone());
                        prog.push_instr(instr);
                        ret
                    },
                },
            );
        }
    }
}

use std::collections::HashMap;

use crate::{type_checker::Environment, type_var::TypeVar};

#[derive(Debug)]
pub struct Value<T> {
    pub ty: TypeVar<T>,
}

pub struct Env<T> {
    vars: HashMap<String, Value<T>>,
}


impl<T> Env<T> {
    pub fn new() -> Self {
        Env {
            vars: HashMap::new(),
        }
    }
}

impl<T> Environment<T> for Env<T> {
    fn type_of(&self, name: &str) -> Option<TypeVar<T>> {
        self.vars.get(name).map(|v| v.ty.clone())
    }
}

mod built_in {
    use crate::{builtin_type::BuiltinType, type_var::TypeVar};

    use super::{Env, Value};

    impl Env<BuiltinType> {
        pub fn init(&mut self) {
            self.vars.insert(
                "t".to_string(),
                Value {
                    ty: TypeVar::constant(BuiltinType::Bool),
                },
            );
            self.vars.insert(
                "f".to_string(),
                Value {
                    ty: TypeVar::constant(BuiltinType::Bool),
                },
            );
            self.vars.insert(
                "nil".to_string(),
                Value {
                    ty: TypeVar::constant(BuiltinType::Nil),
                },
            );
            self.vars.insert(
                "not".to_string(),
                Value {
                    ty: TypeVar::constant(BuiltinType::Fun {
                        args: vec![TypeVar::constant(BuiltinType::Bool)],
                        ret: Box::new(TypeVar::constant(BuiltinType::Bool)),
                    }),
                },
            );
            self.vars.insert(
                "and".to_string(),
                Value {
                    ty: TypeVar::constant(BuiltinType::Fun {
                        args: vec![
                            TypeVar::constant(BuiltinType::Bool),
                            TypeVar::constant(BuiltinType::Bool),
                        ],
                        ret: Box::new(TypeVar::constant(BuiltinType::Bool)),
                    }),
                },
            );
            self.vars.insert(
                "or".to_string(),
                Value {
                    ty: TypeVar::constant(BuiltinType::Fun {
                        args: vec![
                            TypeVar::constant(BuiltinType::Bool),
                            TypeVar::constant(BuiltinType::Bool),
                        ],
                        ret: Box::new(TypeVar::constant(BuiltinType::Bool)),
                    }),
                },
            );
            self.vars.insert(
                "+".to_string(),
                Value {
                    ty: TypeVar::constant(BuiltinType::Fun {
                        args: vec![
                            TypeVar::constant(BuiltinType::Float),
                            TypeVar::constant(BuiltinType::Float),
                        ],
                        ret: Box::new(TypeVar::constant(BuiltinType::Float)),
                    }),
                },
            );
            self.vars.insert(
                "*".to_string(),
                Value {
                    ty: TypeVar::constant(BuiltinType::Fun {
                        args: vec![
                            TypeVar::constant(BuiltinType::Float),
                            TypeVar::constant(BuiltinType::Float),
                        ],
                        ret: Box::new(TypeVar::constant(BuiltinType::Float)),
                    }),
                },
            );
            {
                let tvar = TypeVar::unknown();
                    self.vars.insert(
                        "id".to_string(),
                        Value {
                            ty: TypeVar::constant(BuiltinType::Fun {
                                args: vec![tvar.make_ref()],
                                ret: Box::new(tvar),
                            }),
                        }
                    );
            }
        }
    }
}

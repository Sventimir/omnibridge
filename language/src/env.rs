use std::{
    collections::{BTreeMap, HashMap},
    fmt::Debug,
    sync::Arc,
};

use crate::{
    constraint::{ConstrainedTypeVar, Constraint, Implementation},
    type_checker::Environment,
    type_error::TypeError,
    type_var::{TypeEnv, TypeVar},
};

#[derive(Debug)]
pub struct Value<T, I> {
    ty: Arc<ConstrainedTypeVar<T>>,
    prog: fn(&Value<T, I>, &T) -> Vec<I>,
    impls: Arc<BTreeMap<T, Implementation<I>>>,
    constr_name: String,
    meth_name: String,
}

pub struct Env<T, I> {
    vars: HashMap<String, Value<T, I>>,
    constraints: HashMap<String, Constraint<T>>,
    implementations: HashMap<String, Arc<BTreeMap<T, Implementation<I>>>>,
}

impl<T: Clone + Ord, I> TypeEnv<T> for Env<T, I> {
    fn check_constraint<M: Clone>(&self, c: &str, t: &T, meta: &M) -> Result<(), TypeError<M, T>> {
        let i = self.implementations.get(c).ok_or(TypeError::Undefined {
            symbol: c.to_string(),
            meta: meta.clone(),
        })?;
        i.get(t).ok_or(TypeError::Unimplemented {
            constraint: c.to_string(),
            t: t.clone(),
            meta: meta.clone(),
        })?;
        Ok(())
    }
}

impl<T: Clone + Debug + Ord, I> Env<T, I> {
    pub fn new() -> Self {
        Env {
            vars: HashMap::new(),
            constraints: HashMap::new(),
            implementations: HashMap::new(),
        }
    }

    fn add_constraint(&mut self, constr: Constraint<T>) {
        let constr_name = constr.name().to_string();
        let err_msg = format!("No implementation found for constraint: {}", &constr_name);
        let impls = self.implementations.get(&constr_name).expect(&err_msg);
        for meth in constr.iter_methods() {
            self.vars.insert(
                meth.0.to_string(),
                Value {
                    ty: meth.1.clone(),
                    meth_name: meth.0.to_string(),
                    constr_name: constr_name.clone(),
                    impls: impls.clone(),
                    prog: |this, t| {
                        let i = this.impls.get(t).expect(&format!(
                            "No implementation of {} found for {:?}!.",
                            this.constr_name, &t
                        ));
                        i.get(&this.meth_name)
                            .expect(&format!("No method {} found!", this.meth_name))
                    },
                },
            );
        }
        self.constraints.insert(constr.name().to_string(), constr);
    }
}

impl<T, I> Environment<T, I> for Env<T, I> {
    fn type_of(&self, name: &str) -> Option<TypeVar<T>> {
        self.vars.get(name).map(|v| (v.ty.fresh)(v.ty.as_ref()))
    }

    fn get_instr(&self, name: &str, ty: &T) -> Option<Vec<I>> {
        self.vars.get(name).map(|v| (v.prog)(&v, ty))
    }
}

mod built_in {
    use std::{collections::BTreeMap, sync::Arc};

    use crate::{
        builtin_instr::BuiltinInstr,
        builtin_type::BuiltinType,
        constraint::{ConstrainedTypeVar, Constraint, Implementation},
        type_var::TypeVar,
    };

    use super::{Env, Value};

    impl Env<BuiltinType, BuiltinInstr> {
        pub fn init(&mut self) {
            let no_impls: Arc<BTreeMap<BuiltinType, Implementation<BuiltinInstr>>> =
                Arc::new(BTreeMap::new());

            let mut additive_impl: BTreeMap<BuiltinType, Implementation<BuiltinInstr>> =
                BTreeMap::new();
            let mut additive_nat: Implementation<BuiltinInstr> = Implementation::new();
            additive_nat.add_method("+".to_string(), || vec![BuiltinInstr::AddNat(2)]);
            additive_impl.insert(BuiltinType::Nat, additive_nat);

            let mut additive_int: Implementation<BuiltinInstr> = Implementation::new();
            additive_int.add_method("+".to_string(), || vec![BuiltinInstr::AddInt(2)]);
            additive_impl.insert(BuiltinType::Int, additive_int);

            let mut additive_float: Implementation<BuiltinInstr> = Implementation::new();
            additive_float.add_method("+".to_string(), || vec![BuiltinInstr::AddFlt(2)]);
            additive_impl.insert(BuiltinType::Float, additive_float);
            self.implementations
                .insert("Additive".to_string(), Arc::new(additive_impl));

            let mut additive = Constraint::new("Additive".to_string());
            let plus_t = ConstrainedTypeVar {
                constraints: vec!["Additive".to_string()],
                fresh: |this| {
                    let t = TypeVar::unknown(vec![this.constraints[0].clone()]);
                    TypeVar::constant(BuiltinType::Fun {
                        args: vec![t.make_ref(), t.make_ref()],
                        ret: Box::new(t),
                    })
                },
            };
            {
                additive.add_method("+".to_string(), plus_t);
                additive.add_impl(BuiltinType::Nat);
                additive.add_impl(BuiltinType::Int);
                additive.add_impl(BuiltinType::Float);
            }
            self.add_constraint(additive);

            self.vars.insert(
                "t".to_string(),
                Value {
                    ty: Arc::new(ConstrainedTypeVar {
                        constraints: vec![],
                        fresh: |_| TypeVar::constant(BuiltinType::Bool),
                    }),
                    prog: |_, _| vec![BuiltinInstr::Push(Arc::new(true))],
                    impls: no_impls.clone(),
                    constr_name: "".to_string(),
                    meth_name: "t".to_string(),
                },
            );
            self.vars.insert(
                "f".to_string(),
                Value {
                    ty: Arc::new(ConstrainedTypeVar {
                        constraints: vec![],
                        fresh: |_| TypeVar::constant(BuiltinType::Bool),
                    }),
                    prog: |_, _| vec![BuiltinInstr::Push(Arc::new(false))],
                    impls: no_impls.clone(),
                    constr_name: "".to_string(),
                    meth_name: "f".to_string(),
                },
            );
            self.vars.insert(
                "nil".to_string(),
                Value {
                    ty: Arc::new(ConstrainedTypeVar {
                        constraints: vec![],
                        fresh: |_| TypeVar::constant(BuiltinType::Nil),
                    }),
                    prog: |_, _| vec![BuiltinInstr::Push(Arc::new(false))],
                    impls: no_impls.clone(),
                    constr_name: "".to_string(),
                    meth_name: "nil".to_string(),
                },
            );
            self.vars.insert(
                "not".to_string(),
                Value {
                    ty: Arc::new(ConstrainedTypeVar {
                        constraints: vec![],
                        fresh: |_| {
                            TypeVar::constant(BuiltinType::Fun {
                                args: vec![TypeVar::constant(BuiltinType::Bool)],
                                ret: Box::new(TypeVar::constant(BuiltinType::Bool)),
                            })
                        },
                    }),
                    prog: |_, _| vec![BuiltinInstr::Not],
                    impls: no_impls.clone(),
                    constr_name: "".to_string(),
                    meth_name: "not".to_string(),
                },
            );
            self.vars.insert(
                "and".to_string(),
                Value {
                    ty: Arc::new(ConstrainedTypeVar {
                        constraints: vec![],
                        fresh: |_| {
                            TypeVar::constant(BuiltinType::Fun {
                                args: vec![
                                    TypeVar::constant(BuiltinType::Bool),
                                    TypeVar::constant(BuiltinType::Bool),
                                ],
                                ret: Box::new(TypeVar::constant(BuiltinType::Bool)),
                            })
                        },
                    }),
                    prog: |_, _| vec![BuiltinInstr::And(2)],
                    impls: no_impls.clone(),
                    constr_name: "".to_string(),
                    meth_name: "and".to_string(),
                },
            );
            self.vars.insert(
                "or".to_string(),
                Value {
                    ty: Arc::new(ConstrainedTypeVar {
                        constraints: vec![],
                        fresh: |_| {
                            TypeVar::constant(BuiltinType::Fun {
                                args: vec![
                                    TypeVar::constant(BuiltinType::Bool),
                                    TypeVar::constant(BuiltinType::Bool),
                                ],
                                ret: Box::new(TypeVar::constant(BuiltinType::Bool)),
                            })
                        },
                    }),
                    prog: |_, _| vec![BuiltinInstr::Or(2)],
                    impls: no_impls.clone(),
                    constr_name: "".to_string(),
                    meth_name: "or".to_string(),
                },
            );
            self.vars.insert(
                "*".to_string(),
                Value {
                    ty: Arc::new(ConstrainedTypeVar {
                        constraints: vec![],
                        fresh: |_| {
                            TypeVar::constant(BuiltinType::Fun {
                                args: vec![
                                    TypeVar::constant(BuiltinType::Int),
                                    TypeVar::constant(BuiltinType::Int),
                                ],
                                ret: Box::new(TypeVar::constant(BuiltinType::Int)),
                            })
                        },
                    }),
                    prog: |_, _| vec![BuiltinInstr::Mul(2)],
                    impls: no_impls.clone(),
                    constr_name: "".to_string(),
                    meth_name: "*".to_string(),
                },
            );
            self.vars.insert(
                "=".to_string(),
                Value {
                    ty: Arc::new(ConstrainedTypeVar {
                        constraints: vec![],
                        fresh: |_| {
                            TypeVar::constant(BuiltinType::Fun {
                                args: vec![
                                    TypeVar::constant(BuiltinType::Int),
                                    TypeVar::constant(BuiltinType::Int),
                                ],
                                ret: Box::new(TypeVar::constant(BuiltinType::Int)),
                            })
                        },
                    }),
                    prog: |_, _| vec![BuiltinInstr::Eq],
                    impls: no_impls.clone(),
                    constr_name: "".to_string(),
                    meth_name: "=".to_string(),
                },
            );
            self.vars.insert(
                "id".to_string(),
                Value {
                    ty: Arc::new(ConstrainedTypeVar {
                        constraints: vec![],
                        fresh: |_| {
                            let tvar = TypeVar::unknown(vec![]);
                            TypeVar::constant(BuiltinType::Fun {
                                args: vec![tvar.make_ref()],
                                ret: Box::new(tvar),
                            })
                        },
                    }),
                    prog: |_, _| vec![],
                    impls: no_impls.clone(),
                    constr_name: "".to_string(),
                    meth_name: "id".to_string(),
                },
            );
        }
    }
}

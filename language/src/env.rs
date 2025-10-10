use std::{
    collections::{BTreeMap, HashMap},
    fmt::Debug,
    sync::Arc,
};

use crate::{
    constraint::{Constraint, Implementation},
    type_checker::Environment,
    type_error::TypeError,
    type_var::{PrimType, TypeEnv, TypeExpr, TypeExprGen, TypeVar, TypedMeta},
};

#[derive(Debug)]
pub struct Value<T, I> {
    ty: Arc<TypeExprGen<T>>,
    prog: fn(&Value<T, I>, &TypeExpr<T>) -> Vec<I>,
    impls: Arc<BTreeMap<T, Implementation<I>>>,
    constr_name: String,
    meth_name: String,
}

pub struct Env<T, I> {
    vars: HashMap<String, Value<T, I>>,
    constraints: HashMap<String, Constraint<T>>,
    implementations: HashMap<String, Arc<BTreeMap<T, Implementation<I>>>>,
}

impl<T: Clone + Ord + PrimType, I> TypeEnv<T> for Env<T, I> {
    fn set_default_type<M>(&self, var: &TypeVar<T>, meta: &mut M) -> Result<(), TypeError<M, T>>
    where
        M: Clone + TypedMeta<Type = T>,
    {
        let mut cs = var.constraints();
        if let Some(cname) = cs.pop_first() {
            if let Some(constr) = self.constraints.get(&cname) {
                for t in constr.iter_impls() {
                    if let Ok(()) = all_ok(cs.iter().map(|c| self.check_constraint(c, t, meta))) {
                        return Ok(var.set_val(t.clone()));
                    }
                }
                Err(TypeError::Unresolved { meta: meta.clone() })
            } else {
                Err(TypeError::Undefined {
                    symbol: cname.to_string(),
                    meta: meta.clone(),
                })
            }
        } else {
            var.set_val(T::nil());
            Ok(())
        }
    }

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

fn all_ok<I: Iterator<Item = Result<(), E>>, E>(it: I) -> Result<(), E> {
    for elem in it {
        match elem {
            Ok(_) => (),
            Err(e) => return Err(e),
        }
    }
    return Ok(());
}

impl<T: Clone + Debug + Ord, I> Env<T, I> {
    pub fn new() -> Self {
        Env {
            vars: HashMap::new(),
            constraints: HashMap::new(),
            implementations: HashMap::new(),
        }
    }

    fn add_constraint(
        &mut self,
        constr: Constraint<T>,
        prog: fn(&Value<T, I>, &TypeExpr<T>) -> Vec<I>,
    ) {
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
                    prog,
                },
            );
        }
        self.constraints.insert(constr.name().to_string(), constr);
    }
}

impl<T, I> Environment<T, I> for Env<T, I> {
    fn type_of(&self, name: &str) -> Option<TypeExpr<T>> {
        self.vars.get(name).map(|v| v.ty.make())
    }

    fn get_instr(&self, name: &str, ty: &TypeExpr<T>) -> Option<Vec<I>> {
        self.vars.get(name).map(|v| (v.prog)(&v, ty))
    }

    fn poly_nat(&self) -> TypeExpr<T> {
        let t = TypeVar::unknown(vec!["FromNat".to_string()]);
        TypeExpr { body: t.make_ref(), vars: vec![t] }
    }

    fn poly_int(&self) -> TypeExpr<T> {
        let t = TypeVar::unknown(vec!["FromInt".to_string()]);
        TypeExpr { body: t.make_ref(), vars: vec![t] }
    }

    fn poly_float(&self) -> TypeExpr<T> {
        let t = TypeVar::unknown(vec!["FromFloat".to_string()]);
        TypeExpr {
            body: t.make_ref(),
            vars: vec![t],
        }
    }
}

mod built_in {
    use std::{collections::BTreeMap, sync::Arc};

    use crate::{
        builtin_instr::BuiltinInstr,
        builtin_type::BuiltinType,
        constraint::{Constraint, Implementation},
        type_var::{TypeExpr, TypeExprGen, TypeVar},
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
            let plus_t = TypeExprGen {
                gen: |_this| {
                    let t = TypeVar::unknown(vec!["Additive".to_string()]);
                    TypeExpr {
                        body: TypeVar::constant(BuiltinType::Fun {
                            args: vec![t.make_ref(), t.make_ref()],
                            ret: Box::new(t.make_ref()),
                        }),
                        vars: vec![t],
                    }
                },
            };
            additive.add_method("+".to_string(), plus_t);
            additive.add_impl(BuiltinType::Nat);
            additive.add_impl(BuiltinType::Int);
            additive.add_impl(BuiltinType::Float);
            self.add_constraint(additive, |this, t| {
                let i = this.impls.get(&t.vars[0].value().unwrap()).expect(&format!(
                    "No implementation of {} found for {:?}!.",
                    this.constr_name, &t
                ));
                i.get(&this.meth_name)
                    .expect(&format!("No method {} found!", this.meth_name))
            });

            let mut multiplicative_impl: BTreeMap<BuiltinType, Implementation<BuiltinInstr>> =
                BTreeMap::new();
            let mut multiplicative_nat: Implementation<BuiltinInstr> = Implementation::new();
            multiplicative_nat.add_method("*".to_string(), || vec![BuiltinInstr::MulNat(2)]);
            multiplicative_impl.insert(BuiltinType::Nat, multiplicative_nat);

            let mut multiplicative_int: Implementation<BuiltinInstr> = Implementation::new();
            multiplicative_int.add_method("*".to_string(), || vec![BuiltinInstr::MulInt(2)]);
            multiplicative_impl.insert(BuiltinType::Int, multiplicative_int);

            let mut multiplicative_float: Implementation<BuiltinInstr> = Implementation::new();
            multiplicative_float.add_method("*".to_string(), || vec![BuiltinInstr::MulFlt(2)]);
            multiplicative_impl.insert(BuiltinType::Float, multiplicative_float);
            self.implementations
                .insert("Multiplicative".to_string(), Arc::new(multiplicative_impl));

            let mut multiplicative = Constraint::new("Multiplicative".to_string());
            let mult_t = TypeExprGen {
                gen: |_this| {
                    let t = TypeVar::unknown(vec!["Multiplicative".to_string()]);
                    TypeExpr {
                        body: TypeVar::constant(BuiltinType::Fun {
                            args: vec![t.make_ref(), t.make_ref()],
                            ret: Box::new(t.make_ref()),
                        }),
                        vars: vec![t],
                    }
                },
            };
            multiplicative.add_method("*".to_string(), mult_t);
            multiplicative.add_impl(BuiltinType::Nat);
            multiplicative.add_impl(BuiltinType::Int);
            multiplicative.add_impl(BuiltinType::Float);
            self.add_constraint(multiplicative, |this, t| {
                let i = this.impls.get(&t.vars[0].value().unwrap()).expect(&format!(
                    "No implementation of {} found for {:?}!.",
                    this.constr_name, &t
                ));
                i.get(&this.meth_name)
                    .expect(&format!("No method {} found!", this.meth_name))
            });

            let mut equality_impl: BTreeMap<BuiltinType, Implementation<BuiltinInstr>> =
                BTreeMap::new();
            let mut equality_nat: Implementation<BuiltinInstr> = Implementation::new();
            equality_nat.add_method("=".to_string(), || vec![BuiltinInstr::EqNat]);
            equality_impl.insert(BuiltinType::Nat, equality_nat);
            let mut equality_int: Implementation<BuiltinInstr> = Implementation::new();
            equality_int.add_method("=".to_string(), || vec![BuiltinInstr::EqInt]);
            equality_impl.insert(BuiltinType::Int, equality_int);
            let mut equality_flt: Implementation<BuiltinInstr> = Implementation::new();
            equality_flt.add_method("=".to_string(), || vec![BuiltinInstr::EqFlt]);
            equality_impl.insert(BuiltinType::Float, equality_flt);
            let mut equality_bool: Implementation<BuiltinInstr> = Implementation::new();
            equality_bool.add_method("=".to_string(), || vec![BuiltinInstr::EqBool]);
            equality_impl.insert(BuiltinType::Bool, equality_bool);
            let mut equality_str: Implementation<BuiltinInstr> = Implementation::new();
            equality_str.add_method("=".to_string(), || vec![BuiltinInstr::EqString]);
            equality_impl.insert(BuiltinType::String, equality_str);

            self.implementations
                .insert("Equality".to_string(), Arc::new(equality_impl));

            let mut equality = Constraint::new("Equality".to_string());
            let eq_t = TypeExprGen {
                gen: |_this| {
                    let t = TypeVar::unknown(vec!["Equality".to_string()]);
                    TypeExpr {
                        body: TypeVar::constant(BuiltinType::Fun {
                            args: vec![t.make_ref(), t.make_ref()],
                            ret: Box::new(TypeVar::constant(BuiltinType::Bool)),
                        }),
                        vars: vec![t],
                    }
                },
            };
            equality.add_method("=".to_string(), eq_t);
            equality.add_impl(BuiltinType::Nat);
            equality.add_impl(BuiltinType::Int);
            equality.add_impl(BuiltinType::Float);
            equality.add_impl(BuiltinType::Bool);
            equality.add_impl(BuiltinType::String);
            self.add_constraint(equality, |this, t| {
                let i = this.impls.get(&t.vars[0].value().unwrap()).expect(&format!(
                    "No implementation of {} found for {:?}!.",
                    this.constr_name, &t
                ));
                i.get(&this.meth_name)
                    .expect(&format!("No method {} found!", this.meth_name))
            });

            let mut from_flt_impl: BTreeMap<BuiltinType, Implementation<BuiltinInstr>> =
                BTreeMap::new();
            let mut from_flt_float: Implementation<BuiltinInstr> = Implementation::new();
            from_flt_float.add_method("from-float".to_string(), || vec![]);
            from_flt_impl.insert(BuiltinType::Float, from_flt_float);

            self.implementations
                .insert("FromFloat".to_string(), Arc::new(from_flt_impl));

            let mut from_flt = Constraint::new("FromFloat".to_string());
            let from_flt_t = TypeExprGen {
                gen: |_this| {
                    let t = TypeVar::unknown(vec!["FromFloat".to_string()]);
                    TypeExpr {
                        body: TypeVar::constant(BuiltinType::Fun {
                            args: vec![TypeVar::constant(BuiltinType::Float)],
                            ret: Box::new(t.make_ref()),
                        }),
                        vars: vec![t],
                    }
                },
            };
            from_flt.add_method("from-float".to_string(), from_flt_t);
            from_flt.add_impl(BuiltinType::Float);
            self.add_constraint(from_flt, |this, t| {
                let i = this.impls.get(&t.vars[0].value().unwrap()).expect(&format!(
                    "No implementation of {} found for {:?}!.",
                    this.constr_name, &t
                ));
                i.get(&this.meth_name)
                    .expect(&format!("No method {} found!", this.meth_name))
            });

            let mut from_int_impl: BTreeMap<BuiltinType, Implementation<BuiltinInstr>> =
                BTreeMap::new();
            let mut from_int_float: Implementation<BuiltinInstr> = Implementation::new();
            from_int_float.add_method("from-int".to_string(), || vec![
                BuiltinInstr::Conv(|a| Arc::new(a.downcast_ref::<i64>().unwrap().clone() as f64))
            ]);
            from_int_impl.insert(BuiltinType::Float, from_int_float);

            let mut from_int_int: Implementation<BuiltinInstr> = Implementation::new();
            from_int_int.add_method("from-int".to_string(), || vec![]);
            from_int_impl.insert(BuiltinType::Int, from_int_int);

            self.implementations
                .insert("FromInt".to_string(), Arc::new(from_int_impl));

            let mut from_int = Constraint::new("FromInt".to_string());
            let from_int_t = TypeExprGen {
                gen: |_this| {
                    let t = TypeVar::unknown(vec!["FromInt".to_string()]);
                    TypeExpr {
                        body: TypeVar::constant(BuiltinType::Fun {
                            args: vec![TypeVar::constant(BuiltinType::Int)],
                            ret: Box::new(t.make_ref()),
                        }),
                        vars: vec![t],
                    }
                },
            };
            from_int.add_method("from-int".to_string(), from_int_t);
            from_int.add_impl(BuiltinType::Float);
            from_int.add_impl(BuiltinType::Int);
            self.add_constraint(from_int, |this, t| {
                let i = this.impls.get(&t.vars[0].value().unwrap()).expect(&format!(
                    "No implementation of {} found for {:?}!.",
                    this.constr_name, &t
                ));
                i.get(&this.meth_name)
                    .expect(&format!("No method {} found!", this.meth_name))
            });

            let mut from_nat_impl: BTreeMap<BuiltinType, Implementation<BuiltinInstr>> =
                BTreeMap::new();
            let mut from_nat_float: Implementation<BuiltinInstr> = Implementation::new();
            from_nat_float.add_method("from-nat".to_string(), || vec![
                BuiltinInstr::Conv(|a| Arc::new(a.downcast_ref::<u64>().unwrap().clone() as f64))
            ]);
            from_nat_impl.insert(BuiltinType::Float, from_nat_float);

            let mut from_nat_int: Implementation<BuiltinInstr> = Implementation::new();
            from_nat_int.add_method("from-nat".to_string(), || vec![
                BuiltinInstr::Conv(|a| Arc::new(a.downcast_ref::<u64>().unwrap().clone() as i64))
            ]);
            from_nat_impl.insert(BuiltinType::Int, from_nat_int);

            let mut from_nat_nat: Implementation<BuiltinInstr> = Implementation::new();
            from_nat_nat.add_method("from-nat".to_string(), || vec![]);
            from_nat_impl.insert(BuiltinType::Nat, from_nat_nat);

            self.implementations
                .insert("FromNat".to_string(), Arc::new(from_nat_impl));

            let mut from_nat = Constraint::new("FromNat".to_string());
            let from_nat_t = TypeExprGen {
                gen: |_this| {
                    let t = TypeVar::unknown(vec!["FromNat".to_string()]);
                    TypeExpr {
                        body: TypeVar::constant(BuiltinType::Fun {
                            args: vec![TypeVar::constant(BuiltinType::Nat)],
                            ret: Box::new(t.make_ref()),
                        }),
                        vars: vec![t],
                    }
                },
            };
            from_nat.add_method("from-nat".to_string(), from_nat_t);
            from_nat.add_impl(BuiltinType::Float);
            from_nat.add_impl(BuiltinType::Int);
            from_nat.add_impl(BuiltinType::Nat);
            self.add_constraint(from_nat, |this, t| {
                let i = this.impls.get(&t.vars[0].value().unwrap()).expect(&format!(
                    "No implementation of {} found for {:?}!.",
                    this.constr_name, &t
                ));
                i.get(&this.meth_name)
                    .expect(&format!("No method {} found!", this.meth_name))
            });

            self.vars.insert(
                "t".to_string(),
                Value {
                    ty: Arc::new(TypeExprGen {
                        gen: |_| TypeExpr {
                            body: TypeVar::constant(BuiltinType::Bool),
                            vars: vec![],
                        },
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
                    ty: Arc::new(TypeExprGen {
                        gen: |_| TypeExpr {
                            body: TypeVar::constant(BuiltinType::Bool),
                            vars: vec![],
                        },
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
                    ty: Arc::new(TypeExprGen {
                        gen: |_| TypeExpr {
                            body: TypeVar::constant(BuiltinType::Nil),
                            vars: vec![],
                        },
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
                    ty: Arc::new(TypeExprGen {
                        gen: |_| TypeExpr {
                            body: TypeVar::constant(BuiltinType::Fun {
                                args: vec![TypeVar::constant(BuiltinType::Bool)],
                                ret: Box::new(TypeVar::constant(BuiltinType::Bool)),
                            }),
                            vars: vec![],
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
                    ty: Arc::new(TypeExprGen {
                        gen: |_| TypeExpr {
                            body: TypeVar::constant(BuiltinType::Fun {
                                args: vec![
                                    TypeVar::constant(BuiltinType::Bool),
                                    TypeVar::constant(BuiltinType::Bool),
                                ],
                                ret: Box::new(TypeVar::constant(BuiltinType::Bool)),
                            }),
                            vars: vec![],
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
                    ty: Arc::new(TypeExprGen {
                        gen: |_| TypeExpr {
                            body: TypeVar::constant(BuiltinType::Fun {
                                args: vec![
                                    TypeVar::constant(BuiltinType::Bool),
                                    TypeVar::constant(BuiltinType::Bool),
                                ],
                                ret: Box::new(TypeVar::constant(BuiltinType::Bool)),
                            }),
                            vars: vec![],
                        },
                    }),
                    prog: |_, _| vec![BuiltinInstr::Or(2)],
                    impls: no_impls.clone(),
                    constr_name: "".to_string(),
                    meth_name: "or".to_string(),
                },
            );
            self.vars.insert(
                "id".to_string(),
                Value {
                    ty: Arc::new(TypeExprGen {
                        gen: |_| {
                            let tvar = TypeVar::unknown(vec![]);
                            TypeExpr {
                                body: TypeVar::constant(BuiltinType::Fun {
                                    args: vec![tvar.make_ref()],
                                    ret: Box::new(tvar.make_ref()),
                                }),
                                vars: vec![tvar],
                            }
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

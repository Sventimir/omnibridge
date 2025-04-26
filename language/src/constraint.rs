use std::{collections::BTreeMap, sync::Arc};

use crate::type_var::TypeVar;

#[derive(Debug)]
pub struct Implementation<I> {
    methods: BTreeMap<String, fn() -> Vec<I>>,
}

impl<I> Implementation<I> {
    pub fn new() -> Self {
        Implementation {
            methods: BTreeMap::new(),
        }
    }

    pub fn add_method(&mut self, name: String, method: fn() -> Vec<I>) {
        self.methods.insert(name, method);
    }

    pub fn get(&self, name: &str) -> Option<Vec<I>> {
        self.methods.get(name).map(|f| f())
    }
}

#[derive(Debug)]
pub struct ConstrainedTypeVar<T> {
    pub constraints: Vec<Arc<Constraint<T>>>,
    pub fresh: fn(Arc<ConstrainedTypeVar<T>>) -> TypeVar<T>,
}

#[derive(Debug)]
pub struct Constraint<T> {
    name: String,
    methods: BTreeMap<String, Arc<ConstrainedTypeVar<T>>>,
}

impl<T> Constraint<T> {
    pub fn new(name: String) -> Self {
        Constraint {
            name,
            methods: BTreeMap::new(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn add_method(&mut self, name: String, method: Arc<ConstrainedTypeVar<T>>) {
        self.methods.insert(name, method);
    }

    pub fn iter_methods(&self) -> impl Iterator<Item = (&String, &Arc<ConstrainedTypeVar<T>>)> {
        self.methods.iter()
    }
}

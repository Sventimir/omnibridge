use std::collections::BTreeMap;

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

pub struct Constraint<T> {
    name: String,
    methods: BTreeMap<String, fn() -> TypeVar<T>>,
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

    pub fn add_method(&mut self, name: String, method: fn() -> TypeVar<T>) {
        self.methods.insert(name, method);
    }

    pub fn iter_methods(&self) -> impl Iterator<Item = (&String, &fn() -> TypeVar<T>)> {
        self.methods.iter()
    }
}

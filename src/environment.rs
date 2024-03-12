use crate::object::Object;
use std::collections::HashMap;

#[derive(Clone, PartialEq, Eq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<std::rc::Rc<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Environment) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(std::rc::Rc::new(outer)),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        match self.store.get(name) {
            Some(o) => Some(o),
            None => {
                match &self.outer {
                    Some(outer) => outer.get(name),
                    None => None,
                }
            }
        }
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }
}

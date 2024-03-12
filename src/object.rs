
#[derive(PartialEq, Eq)]
pub enum Object {
    Null,
    Int(i64),
    Bool(bool),
}

impl Object {
    pub fn get_type(&self) -> &'static str {
        match self {
            Object::Null => "Null",
            Object::Int(_) => "Integer",
            Object::Bool(_) => "Bool",
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Null => "Null".to_string(),
            Object::Int(val) => val.to_string(),
            Object::Bool(val) => val.to_string(),
        }
    }
}

impl Default for Object {
    fn default() -> Self {
        Object::Null
    }
}

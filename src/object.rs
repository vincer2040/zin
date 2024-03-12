#[derive(PartialEq, Eq)]
pub enum ObjectType {
    Null,
    Int,
    Bool,
    Return,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Object {
    Null,
    Int(i64),
    Bool(bool),
    Return(Box<Object>),
}

impl Object {
    pub fn get_type(&self) -> ObjectType {
        match self {
            Object::Null => ObjectType::Null,
            Object::Int(_) => ObjectType::Int,
            Object::Bool(_) => ObjectType::Bool,
            Object::Return(_) => ObjectType::Return,
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Null => "Null".to_string(),
            Object::Int(val) => val.to_string(),
            Object::Bool(val) => val.to_string(),
            Object::Return(val) => val.inspect(),
        }
    }
}

impl Default for Object {
    fn default() -> Self {
        Object::Null
    }
}

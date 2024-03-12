use crate::{ast::Block, environment::Environment};

#[derive(PartialEq, Eq)]
pub enum ObjectType {
    Null,
    Int,
    Bool,
    Return,
    Error,
    Function,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub body: Block,
    pub env: Environment,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Object {
    Null,
    Int(i64),
    Bool(bool),
    Return(Box<Object>),
    Error(String),
    Function(Function),
}

impl Object {
    pub fn get_type(&self) -> ObjectType {
        match self {
            Object::Null => ObjectType::Null,
            Object::Int(_) => ObjectType::Int,
            Object::Bool(_) => ObjectType::Bool,
            Object::Return(_) => ObjectType::Return,
            Object::Error(_) => ObjectType::Error,
            Object::Function(_) => ObjectType::Function,
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Null => "Null".to_string(),
            Object::Int(val) => val.to_string(),
            Object::Bool(val) => val.to_string(),
            Object::Return(val) => val.inspect(),
            Object::Error(val) => val.to_owned(),
            Object::Function(val) => {
                let mut res = String::new();
                res += "fn ";
                res += &val.name;
                res += "(";
                for (i, param) in val.params.iter().enumerate() {
                    res += &param;
                    if i != val.params.len() - 1 {
                        res += ", ";
                    }
                }
                res += ") {\n";
                res += &val.body.to_string();
                res += "\n}";
                return res;
            },
        }
    }

    pub fn is_error(&self) -> bool {
        return matches!(self.get_type(), ObjectType::Error);
    }
}

impl Default for Object {
    fn default() -> Self {
        Object::Null
    }
}

impl ToString for ObjectType {
    fn to_string(&self) -> String {
        match self {
            ObjectType::Null => "NULL".to_string(),
            ObjectType::Int => "INTEGER".to_string(),
            ObjectType::Bool => "BOOLEAN".to_string(),
            ObjectType::Return => "RETURN".to_string(),
            ObjectType::Error => "ERROR".to_string(),
            ObjectType::Function => "FUNCTION".to_string(),
        }
    }
}

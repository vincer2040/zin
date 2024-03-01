pub struct Ast {
    pub statements: Vec<Statement>,
}

pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
}

pub struct LetStatement {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Eq)]
pub enum PrefixOperator {
    Minus,
    Bang,
}

pub struct Prefix {
    pub oper: PrefixOperator,
    pub right: std::rc::Rc<Expression>,
}

pub enum Expression {
    Ident(String),
    Int(i64),
    Prefix(Prefix),
}

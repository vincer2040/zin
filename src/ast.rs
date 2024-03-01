pub struct Program {
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

pub enum Expression {
    Ident(String),
    Int(i64),
}

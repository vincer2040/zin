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

#[derive(Debug, PartialEq, Eq)]
pub enum InfixOperator {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,
}

pub struct Infix {
    pub oper: InfixOperator,
    pub left: std::rc::Rc<Expression>,
    pub right: std::rc::Rc<Expression>,
}

pub enum Expression {
    Boolean(bool),
    Ident(String),
    Int(i64),
    Prefix(Prefix),
    Infix(Infix),
}

impl ToString for Ast {
    fn to_string(&self) -> String {
        let mut res = String::new();
        for stmt in &self.statements {
            res.push_str(&stmt.to_string());
        }
        return res;
    }
}

impl ToString for Statement {
    fn to_string(&self) -> String {
        match self {
            Statement::LetStatement(ls) => ls.to_string(),
            Statement::ReturnStatement(rs) => String::from("return ") + &rs.to_string() + ";",
            Statement::ExpressionStatement(es) => es.to_string(),
        }
    }
}

impl ToString for LetStatement {
    fn to_string(&self) -> String {
        let mut res = String::new();
        res.push_str("let ");
        res.push_str(&self.name);
        res.push_str(" = ");
        res.push_str(&self.value.to_string());
        res.push(';');
        return res;
    }
}

impl ToString for Expression {
    fn to_string(&self) -> String {
        match self {
            Expression::Boolean(v) => {
                if *v {
                    return "true".into();
                }
                return "false".into();
            }
            Expression::Ident(ident) => ident.to_string(),
            Expression::Int(i) => i.to_string(),
            Expression::Prefix(prefix) => prefix.to_string(),
            Expression::Infix(infix) => infix.to_string(),
        }
    }
}

impl ToString for Prefix {
    fn to_string(&self) -> String {
        let mut res = String::new();
        res += "(";
        match self.oper {
            PrefixOperator::Minus => res += "-",
            PrefixOperator::Bang => res += "!",
        };
        res += &self.right.to_string();
        res += ")";
        return res;
    }
}

impl ToString for Infix {
    fn to_string(&self) -> String {
        let mut res = String::new();
        res += "(";
        res += &self.left.to_string();
        match self.oper {
            InfixOperator::Plus => res += " + ",
            InfixOperator::Minus => res += " - ",
            InfixOperator::Asterisk => res += " * ",
            InfixOperator::Slash => res += " / ",
            InfixOperator::Lt => res += " < ",
            InfixOperator::Gt => res += " > ",
            InfixOperator::Eq => res += " == ",
            InfixOperator::NotEq => res += " != ",
        }
        res += &self.right.to_string();
        res += ")";
        return res;
    }
}

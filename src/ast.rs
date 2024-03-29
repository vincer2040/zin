pub struct Ast {
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PrefixOperator {
    Minus,
    Bang,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Prefix {
    pub oper: PrefixOperator,
    pub right: std::rc::Rc<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Infix {
    pub oper: InfixOperator,
    pub left: std::rc::Rc<Expression>,
    pub right: std::rc::Rc<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    pub block: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IfExpression {
    pub cond: std::rc::Rc<Expression>,
    pub consequence: Block,
    pub alternative: Option<Block>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionExpression {
    pub name: String,
    pub params: Vec<String>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Call {
    pub name: String,
    pub args: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    Boolean(bool),
    Ident(String),
    Int(i64),
    String(String),
    Prefix(Prefix),
    Infix(Infix),
    IfExpression(IfExpression),
    Function(FunctionExpression),
    Call(Call),
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
            Expression::String(s) => s.to_string(),
            Expression::Prefix(prefix) => prefix.to_string(),
            Expression::Infix(infix) => infix.to_string(),
            Expression::Function(func) => func.to_string(),
            Expression::IfExpression(if_expression) => if_expression.to_string(),
            Expression::Call(call) => call.to_string(),
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

impl ToString for InfixOperator {
    fn to_string(&self) -> String {
        match self {
            InfixOperator::Plus => "+".to_string(),
            InfixOperator::Minus => "-".to_string(),
            InfixOperator::Asterisk => "*".to_string(),
            InfixOperator::Slash => "/".to_string(),
            InfixOperator::Lt => "<".to_string(),
            InfixOperator::Gt => ">".to_string(),
            InfixOperator::Eq => "==".to_string(),
            InfixOperator::NotEq => "!=".to_string(),
        }
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

impl ToString for IfExpression {
    fn to_string(&self) -> String {
        let mut res = String::new();
        res += "if(";
        res += &self.cond.to_string();
        res += ")";
        res += &self.consequence.to_string();
        match &self.alternative {
            Some(body) => {
                res += "else";
                res += &body.to_string();
            }
            None => (),
        };
        return res;
    }
}

impl ToString for FunctionExpression {
    fn to_string(&self) -> String {
        let mut res = String::new();
        res += "fn ";
        res += &self.name;
        res += "(";
        for (i, arg) in self.params.iter().enumerate() {
            res += arg;
            if i < self.params.len() - 1 {
                res += ", ";
            }
        }
        res += &self.body.to_string();
        return res;
    }
}

impl ToString for Call {
    fn to_string(&self) -> String {
        let mut res = String::new();
        res += &self.name;
        res += "(";
        for (i, arg) in self.args.iter().enumerate() {
            let s = arg.to_string();
            res += &s;
            if i > self.args.len() {
                res += ", ";
            }
        }
        return res;
    }
}

impl ToString for Block {
    fn to_string(&self) -> String {
        let mut res = String::new();
        for stmt in &self.block {
            res += &stmt.to_string();
        }
        return res;
    }
}

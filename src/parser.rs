use std::error::Error;

use crate::{
    ast::{
        Ast, Block, Call, Expression, FunctionExpression, IfExpression, Infix, InfixOperator,
        LetStatement, Prefix, PrefixOperator, Statement,
    },
    lexer::Lexer,
    token::Token,
};

#[derive(PartialOrd, Ord, PartialEq, Eq)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    pub fn new(tok: &Token) -> Self {
        match tok {
            Token::Eq | Token::NotEq => Self::Equals,
            Token::Lt | Token::Gt => Self::LessGreater,
            Token::Plus | Token::Minus => Self::Sum,
            Token::Asterisk | Token::Slash => Self::Product,
            Token::LParen => Self::Call,
            _ => Self::Lowest,
        }
    }
}

pub struct Parser<'a> {
    l: Lexer<'a>,
    cur: Token,
    peek: Token,
    errors: Vec<String>,
}

type ParserError = Box<dyn Error>;

impl<'a> Parser<'a> {
    pub fn new(l: Lexer<'a>) -> Self {
        let mut p = Self {
            l,
            cur: Token::Eof,
            peek: Token::Eof,
            errors: Vec::new(),
        };
        p.next_token();
        p.next_token();
        return p;
    }

    pub fn parse(&mut self) -> Ast {
        let mut stmts = Vec::new();

        while self.cur != Token::Eof {
            let stmt = self.parse_statement();
            match stmt {
                Ok(s) => stmts.push(s),
                Err(e) => {
                    let s = e.to_string();
                    self.errors.push(s);
                }
            };
            self.next_token();
        }

        return Ast { statements: stmts };
    }

    pub fn errors(&self) -> &Vec<String> {
        return &self.errors;
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.cur {
            Token::Let => self.parse_let(),
            Token::Return => self.parse_return(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let(&mut self) -> Result<Statement, ParserError> {
        self.next_token();
        let name = match std::mem::take(&mut self.cur) {
            Token::Ident(ident) => ident,
            _ => {
                let err = format!("expected Ident, got {:#?}", self.cur);
                return Err(err.into());
            }
        };
        if !self.expect_peek(Token::Assign) {
            let err = format!("expectd peek token to be Assign, got {:#?}", self.peek);
            return Err(err.into());
        }
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;
        let ls = LetStatement { name, value };
        if self.peek == Token::Semicolon {
            self.next_token();
        }
        return Ok(Statement::LetStatement(ls));
    }

    fn parse_return(&mut self) -> Result<Statement, ParserError> {
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;
        if self.peek == Token::Semicolon {
            self.next_token();
        }
        return Ok(Statement::ReturnStatement(value));
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let exp = self.parse_expression(Precedence::Lowest)?;
        if self.peek == Token::Semicolon {
            self.next_token();
        }
        return Ok(Statement::ExpressionStatement(exp));
    }

    fn parse_expression(&mut self, prec: Precedence) -> Result<Expression, ParserError> {
        let cur = std::mem::take(&mut self.cur);
        let mut exp: Expression;
        match cur {
            Token::Ident(ident) => exp = Expression::Ident(ident),
            Token::Int(int) => {
                let val: i64 = int.parse()?;
                exp = Expression::Int(val);
            }
            Token::String(s) => exp = Expression::String(s.to_string()),
            Token::True => exp = Expression::Boolean(true),
            Token::False => exp = Expression::Boolean(false),
            Token::Minus => exp = self.parse_prefix(PrefixOperator::Minus)?,
            Token::Bang => exp = self.parse_prefix(PrefixOperator::Bang)?,
            Token::LParen => exp = self.parse_group()?,
            Token::If => exp = self.parse_if_expression()?,
            Token::Function => exp = self.parse_function()?,
            _ => {
                let err = format!("unknown token {:#?}", cur);
                return Err(err.into());
            }
        };

        while self.peek != Token::Semicolon && prec < self.peek_precedence() {
            match self.peek {
                Token::Plus => {
                    self.next_token();
                    exp = self.parse_infix(InfixOperator::Plus, exp)?;
                }
                Token::Minus => {
                    self.next_token();
                    exp = self.parse_infix(InfixOperator::Minus, exp)?;
                }
                Token::Asterisk => {
                    self.next_token();
                    exp = self.parse_infix(InfixOperator::Asterisk, exp)?;
                }
                Token::Slash => {
                    self.next_token();
                    exp = self.parse_infix(InfixOperator::Slash, exp)?;
                }
                Token::Lt => {
                    self.next_token();
                    exp = self.parse_infix(InfixOperator::Lt, exp)?;
                }
                Token::Gt => {
                    self.next_token();
                    exp = self.parse_infix(InfixOperator::Gt, exp)?;
                }
                Token::Eq => {
                    self.next_token();
                    exp = self.parse_infix(InfixOperator::Eq, exp)?;
                }
                Token::NotEq => {
                    self.next_token();
                    exp = self.parse_infix(InfixOperator::NotEq, exp)?;
                }
                Token::LParen => {
                    self.next_token();
                    exp = self.parse_call(exp)?;
                }
                _ => return Ok(exp),
            }
        }
        return Ok(exp);
    }

    fn parse_group(&mut self) -> Result<Expression, ParserError> {
        self.next_token();
        let e = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(Token::RParen) {
            let err = format!("expected peek token to be RParen, got {:#?}", self.peek);
            return Err(err.into());
        }
        return Ok(e);
    }

    fn parse_prefix(&mut self, oper: PrefixOperator) -> Result<Expression, ParserError> {
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;
        let prefix = Prefix {
            oper,
            right: std::rc::Rc::new(right),
        };
        return Ok(Expression::Prefix(prefix));
    }

    fn parse_infix(
        &mut self,
        oper: InfixOperator,
        left: Expression,
    ) -> Result<Expression, ParserError> {
        let cur_prec = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(cur_prec)?;
        let infix = Infix {
            oper,
            left: std::rc::Rc::new(left),
            right: std::rc::Rc::new(right),
        };
        return Ok(Expression::Infix(infix));
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        self.next_token();
        let cond = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(Token::LSquirly) {
            let err = format!("expected LSquirly, got {:#?}", self.peek);
            return Err(err.into());
        }
        let conseq = self.parse_block()?;
        let mut alt = None;
        if self.peek == Token::Else {
            self.next_token();
            if !self.expect_peek(Token::LSquirly) {
                let err = format!("expected LSquirly, got {:#?}", self.peek);
                return Err(err.into());
            }
            let alt_body = self.parse_block()?;
            alt = Some(alt_body);
        }

        let if_expression = IfExpression {
            cond: std::rc::Rc::new(cond),
            consequence: conseq,
            alternative: alt,
        };

        let res = Expression::IfExpression(if_expression);
        return Ok(res);
    }

    fn parse_function(&mut self) -> Result<Expression, ParserError> {
        self.next_token();
        let name = match std::mem::take(&mut self.cur) {
            Token::Ident(ident) => ident,
            _ => {
                let err = format!("expected Ident, got {:#?}", self.cur);
                return Err(err.into());
            }
        };
        if !self.expect_peek(Token::LParen) {
            let err = format!("expected LParen, got {:#?}", self.peek);
            return Err(err.into());
        }
        let params = self.parse_function_params()?;
        if !self.expect_peek(Token::LSquirly) {
            let err = format!("expected LSquirly, got {:#?}", self.peek);
            return Err(err.into());
        }
        let body = self.parse_block()?;

        let func = FunctionExpression { name, params, body };
        let res = Expression::Function(func);
        return Ok(res);
    }

    fn parse_call(&mut self, e: Expression) -> Result<Expression, ParserError> {
        let name = match e {
            Expression::Ident(name) => name,
            _ => {
                let err = format!("expected Ident, got {}", e.to_string());
                return Err(err.into());
            }
        };
        if self.cur != Token::LParen {
            let err = format!("expected LParen, got {:#?}", self.peek);
            return Err(err.into());
        }
        let args = self.parse_call_args()?;
        let call = Call { name, args };
        let res = Expression::Call(call);
        return Ok(res);
    }

    fn parse_function_params(&mut self) -> Result<Vec<String>, ParserError> {
        let mut res = Vec::new();
        if self.peek == Token::RParen {
            self.next_token();
            return Ok(res);
        }
        self.next_token();
        let cur = match std::mem::take(&mut self.cur) {
            Token::Ident(val) => val,
            _ => {
                let err = format!("expected Ident, got {:#?}", self.cur);
                return Err(err.into());
            }
        };
        res.push(cur);
        while self.peek == Token::Comma {
            self.next_token();
            self.next_token();
            let next = match std::mem::take(&mut self.cur) {
                Token::Ident(val) => val,
                _ => {
                    let err = format!("expected Ident, got {:#?}", self.cur);
                    return Err(err.into());
                }
            };
            res.push(next);
        }

        if !self.expect_peek(Token::RParen) {
            let err = format!("expected RParen, got {:#?}", self.peek);
            return Err(err.into());
        }
        return Ok(res);
    }

    fn parse_call_args(&mut self) -> Result<Vec<Expression>, ParserError> {
        let mut res = Vec::new();
        if self.peek == Token::RParen {
            self.next_token();
            return Ok(res);
        }
        self.next_token();
        let cur = self.parse_expression(Precedence::Lowest)?;
        res.push(cur);
        while self.peek == Token::Comma {
            self.next_token();
            self.next_token();
            let next = self.parse_expression(Precedence::Lowest)?;
            res.push(next);
        }
        if !self.expect_peek(Token::RParen) {
            let err = format!("expected RParen, got {:#?}", self.peek);
            return Err(err.into());
        }
        return Ok(res);
    }

    fn parse_block(&mut self) -> Result<Block, ParserError> {
        self.next_token();
        let mut stmts = Vec::new();

        while self.cur != Token::RSquirly && self.cur != Token::Eof {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
            self.next_token();
        }
        let res = Block { block: stmts };
        return Ok(res);
    }

    fn cur_precedence(&self) -> Precedence {
        return Precedence::new(&self.cur);
    }

    fn peek_precedence(&self) -> Precedence {
        return Precedence::new(&self.peek);
    }

    fn next_token(&mut self) {
        std::mem::swap(&mut self.cur, &mut self.peek);
        self.peek = self.l.next_token();
    }

    fn expect_peek(&mut self, tok: Token) -> bool {
        if self.peek != tok {
            return false;
        }
        self.next_token();
        return true;
    }
}

#[cfg(test)]
mod test {

    use crate::{
        ast::{
            Expression, FunctionExpression, InfixOperator, LetStatement, PrefixOperator, Statement,
        },
        lexer::Lexer,
    };

    use super::Parser;

    struct BoolTest {
        input: &'static str,
        exp: bool,
    }

    struct PrefixTest<T> {
        input: &'static str,
        oper: PrefixOperator,
        exp: T,
    }

    struct InfixTest<T> {
        input: &'static str,
        oper: InfixOperator,
        left: T,
        right: T,
    }

    struct LetTest {
        name: &'static str,
        value: Expression,
    }

    struct PrecedenceTest {
        input: &'static str,
        exp: &'static str,
    }

    fn check_errors(p: &Parser) {
        let errs = p.errors();
        for e in errs {
            println!("{}", e)
        }
        assert_eq!(errs.len(), 0);
    }

    fn assert_expression(stmt: &Statement) -> &Expression {
        assert!(matches!(stmt, Statement::ExpressionStatement(_)));
        let e = match &stmt {
            Statement::ExpressionStatement(e) => e,
            _ => unreachable!(),
        };
        return e;
    }

    fn assert_let(stmt: &Statement) -> &LetStatement {
        assert!(matches!(stmt, Statement::LetStatement(_)));
        let ls = match &stmt {
            Statement::LetStatement(ls) => ls,
            _ => unreachable!(),
        };
        return ls;
    }

    fn assert_ident(e: &Expression, exp: &str) {
        assert!(matches!(e, Expression::Ident(_)));
        let ident = match e {
            Expression::Ident(ident) => ident,
            _ => unreachable!(),
        };
        assert_eq!(ident, exp);
    }

    fn assert_int(e: &Expression, exp: i64) {
        assert!(matches!(e, Expression::Int(_)));
        let int = match e {
            Expression::Int(i) => i,
            _ => unreachable!(),
        };
        assert_eq!(*int, exp);
    }

    fn assert_bool(e: &Expression, exp: bool) {
        assert!(matches!(e, Expression::Boolean(_)));
        let b = match e {
            Expression::Boolean(i) => i,
            _ => unreachable!(),
        };
        assert_eq!(*b, exp);
    }

    fn assert_function(e: &Expression) -> &FunctionExpression {
        assert!(matches!(e, Expression::Function(_)));
        let f = match e {
            Expression::Function(f) => f,
            _ => unreachable!(),
        };
        return f;
    }

    fn assert_int_prefix(e: &Expression, oper: PrefixOperator, exp: i64) {
        assert!(matches!(e, Expression::Prefix(_)));
        let prefix = match e {
            Expression::Prefix(p) => p,
            _ => unreachable!(),
        };
        assert_eq!(prefix.oper, oper);
        assert!(matches!(*prefix.right, Expression::Int(_)));
        let right = match *prefix.right {
            Expression::Int(i) => i,
            _ => unreachable!(),
        };
        assert_eq!(right, exp);
    }

    fn assert_int_infix(e: &Expression, oper: InfixOperator, left: i64, right: i64) {
        assert!(matches!(e, Expression::Infix(_)));
        let infix = match e {
            Expression::Infix(inf) => inf,
            _ => unreachable!(),
        };
        assert_eq!(infix.oper, oper);
        assert_int(&infix.left, left);
        assert_int(&infix.right, right);
    }

    fn assert_ident_infix(e: &Expression, oper: InfixOperator, left: &str, right: &str) {
        assert!(matches!(e, Expression::Infix(_)));
        let infix = match e {
            Expression::Infix(inf) => inf,
            _ => unreachable!(),
        };
        assert_eq!(infix.oper, oper);
        assert_ident(&infix.left, left);
        assert_ident(&infix.right, right);
    }

    #[test]
    fn test_ident() {
        let input = "foobar;";
        let l = Lexer::new(input.as_bytes());
        let mut p = Parser::new(l);
        let res = p.parse();
        check_errors(&p);
        assert_eq!(res.statements.len(), 1);
        let stmt = &res.statements[0];
        let e = assert_expression(stmt);
        assert_ident(e, "foobar");
    }

    #[test]
    fn test_boolean() {
        let tests = [
            BoolTest {
                input: "true;",
                exp: true,
            },
            BoolTest {
                input: "false;",
                exp: false,
            },
        ];

        for test in tests {
            let l = Lexer::new(test.input.as_bytes());
            let mut p = Parser::new(l);
            let res = p.parse();
            check_errors(&p);
            assert_eq!(res.statements.len(), 1);
            let stmt = &res.statements[0];
            let e = assert_expression(&stmt);
            assert_bool(e, test.exp);
        }
    }

    #[test]
    fn test_ints() {
        let inputs = ["5", "10"];

        let exps = [5, 10];

        for (i, inp) in inputs.iter().enumerate() {
            let l = Lexer::new(inp.as_bytes());
            let mut p = Parser::new(l);
            let res = p.parse();
            check_errors(&p);
            assert_eq!(res.statements.len(), 1);
            let stmt = &res.statements[0];
            let e = assert_expression(&stmt);
            let exp = exps[i];
            assert_int(&e, exp);
        }
    }

    #[test]
    fn test_strings() {
        let input = "\"hello world\"";
        let l = Lexer::new(input.as_bytes());
        let mut p = Parser::new(l);
        let res = p.parse();
        assert_eq!(res.statements.len(), 1);
        let stmt = &res.statements[0];
        let exp = assert_expression(&stmt);
        assert!(matches!(exp, Expression::String(_)));
        let s = match exp {
            Expression::String(s) => s,
            _ => unreachable!(),
        };

        assert_eq!(s, "hello world");
    }

    #[test]
    fn test_prefix() {
        let tests = [
            PrefixTest {
                input: "-5",
                oper: PrefixOperator::Minus,
                exp: 5,
            },
            PrefixTest {
                input: "!15",
                oper: PrefixOperator::Bang,
                exp: 15,
            },
        ];

        for test in tests {
            let l = Lexer::new(test.input.as_bytes());
            let mut p = Parser::new(l);
            let res = p.parse();
            check_errors(&p);
            assert_eq!(res.statements.len(), 1);
            let stmt = &res.statements[0];
            let e = assert_expression(stmt);
            assert_int_prefix(&e, test.oper, test.exp);
        }
    }

    #[test]
    fn test_infix() {
        let tests = [
            InfixTest {
                input: "5 + 5;",
                oper: InfixOperator::Plus,
                left: 5,
                right: 5,
            },
            InfixTest {
                input: "5 - 5;",
                oper: InfixOperator::Minus,
                left: 5,
                right: 5,
            },
            InfixTest {
                input: "5 * 5",
                oper: InfixOperator::Asterisk,
                left: 5,
                right: 5,
            },
            InfixTest {
                input: "5 / 5;",
                oper: InfixOperator::Slash,
                left: 5,
                right: 5,
            },
            InfixTest {
                input: "5 < 5;",
                oper: InfixOperator::Lt,
                left: 5,
                right: 5,
            },
            InfixTest {
                input: "5 > 5;",
                oper: InfixOperator::Gt,
                left: 5,
                right: 5,
            },
            InfixTest {
                input: "5 == 5;",
                oper: InfixOperator::Eq,
                left: 5,
                right: 5,
            },
            InfixTest {
                input: "5 != 5;",
                oper: InfixOperator::NotEq,
                left: 5,
                right: 5,
            },
        ];

        for test in tests {
            let l = Lexer::new(test.input.as_bytes());
            let mut p = Parser::new(l);
            let res = p.parse();
            check_errors(&p);
            assert_eq!(res.statements.len(), 1);
            let stmt = &res.statements[0];
            let e = assert_expression(&stmt);
            assert_int_infix(&e, test.oper, test.left, test.right);
        }
    }

    #[test]
    fn test_let_statement() {
        let input = "
        let foo = 5;
        let bar = 10;
        let foobar = foo;
        ";
        let exps = [
            LetTest {
                name: "foo",
                value: Expression::Int(5),
            },
            LetTest {
                name: "bar",
                value: Expression::Int(10),
            },
            LetTest {
                name: "foobar",
                value: Expression::Ident("foo".into()),
            },
        ];

        let l = Lexer::new(input.as_bytes());
        let mut p = Parser::new(l);
        let res = p.parse();
        check_errors(&p);
        assert_eq!(res.statements.len(), 3);
        for (i, exp) in exps.iter().enumerate() {
            let cur = &res.statements[i];
            let ls = assert_let(&cur);
            assert_eq!(ls.name, exp.name);
            match &exp.value {
                Expression::Int(i) => assert_int(&ls.value, *i),
                Expression::Ident(i) => assert_ident(&ls.value, &i),
                _ => unreachable!(),
            };
        }
    }

    #[test]
    fn test_function() {
        let input = "
        fn add(x, y) {
            return x + y;
        }
        ";
        let l = Lexer::new(input.as_bytes());
        let mut p = Parser::new(l);
        let res = p.parse();
        check_errors(&p);
        assert_eq!(res.statements.len(), 1);
        let stmt = &res.statements[0];
        let exp = assert_expression(&stmt);
        let func = assert_function(&exp);
        assert_eq!(func.name, "add");
        assert_eq!(func.params.len(), 2);
        assert_eq!(func.params[0], "x");
        assert_eq!(func.params[1], "y");
        assert_eq!(func.body.block.len(), 1);
        let body_stmt = &func.body.block[0];
        assert!(matches!(body_stmt, Statement::ReturnStatement(_)));
        let ret = match body_stmt {
            Statement::ReturnStatement(rs) => rs,
            _ => unreachable!(),
        };
        assert_ident_infix(&ret, InfixOperator::Plus, "x", "y");
    }

    #[test]
    fn test_if_else() {
        let input = "
            if x < y {
                return x;
            } else {
                return y;
            }
        ";
        let l = Lexer::new(input.as_bytes());
        let mut p = Parser::new(l);
        let res = p.parse();
        assert_eq!(res.statements.len(), 1);
        let stmt = &res.statements[0];
        let exp = assert_expression(&stmt);
        assert!(matches!(exp, Expression::IfExpression(_)));
        let if_exp = match exp {
            Expression::IfExpression(e) => e,
            _ => unreachable!(),
        };
        assert_ident_infix(&if_exp.cond, InfixOperator::Lt, "x", "y");
        assert_eq!(if_exp.consequence.block.len(), 1);
        let conseq_stmt = &if_exp.consequence.block[0];
        assert!(matches!(conseq_stmt, Statement::ReturnStatement(_)));
        let conseq_ret = match &conseq_stmt {
            Statement::ReturnStatement(rs) => rs,
            _ => unreachable!(),
        };
        assert_ident(&conseq_ret, "x");
        assert!(matches!(if_exp.alternative, Some(_)));
        let alt = if_exp.alternative.as_ref().unwrap();
        assert_eq!(alt.block.len(), 1);
        let alt_stmt = &alt.block[0];
        assert!(matches!(alt_stmt, Statement::ReturnStatement(_)));
        let alt_ret = match &alt_stmt {
            Statement::ReturnStatement(rs) => rs,
            _ => unreachable!(),
        };
        assert_ident(&alt_ret, "y");
    }

    #[test]
    fn test_call() {
        let input = "add(y, 5 + 10);";
        let l = Lexer::new(input.as_bytes());
        let mut p = Parser::new(l);
        let res = p.parse();
        check_errors(&p);
        assert_eq!(res.statements.len(), 1);
        let stmt = &res.statements[0];
        let exp = assert_expression(&stmt);
        assert!(matches!(exp, Expression::Call(_)));
        let call = match exp {
            Expression::Call(call) => call,
            _ => unreachable!(),
        };
        assert_eq!(call.name, "add");
        assert_eq!(call.args.len(), 2);
        assert_ident(&call.args[0], "y");
        assert_int_infix(&call.args[1], InfixOperator::Plus, 5, 10);
    }

    #[test]
    fn operator_precedence() {
        let tests = [
            PrecedenceTest {
                input: "-a * b",
                exp: "((-a) * b)",
            },
            PrecedenceTest {
                input: "!-a",
                exp: "(!(-a))",
            },
            PrecedenceTest {
                input: "a + b + c",
                exp: "((a + b) + c)",
            },
            PrecedenceTest {
                input: "a + b - c",
                exp: "((a + b) - c)",
            },
            PrecedenceTest {
                input: "a * b * c",
                exp: "((a * b) * c)",
            },
            PrecedenceTest {
                input: "a * b / c",
                exp: "((a * b) / c)",
            },
            PrecedenceTest {
                input: "a + b / c",
                exp: "(a + (b / c))",
            },
            PrecedenceTest {
                input: "a + b * c + d / e - f",
                exp: "(((a + (b * c)) + (d / e)) - f)",
            },
            PrecedenceTest {
                input: "3 + 4; -5 * 5",
                exp: "(3 + 4)((-5) * 5)",
            },
            PrecedenceTest {
                input: "5 > 4 == 3 < 4",
                exp: "((5 > 4) == (3 < 4))",
            },
            PrecedenceTest {
                input: "5 < 4 != 3 > 4",
                exp: "((5 < 4) != (3 > 4))",
            },
            PrecedenceTest {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                exp: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            },
            PrecedenceTest {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                exp: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            },
            PrecedenceTest {
                input: "true",
                exp: "true",
            },
            PrecedenceTest {
                input: "false",
                exp: "false",
            },
            PrecedenceTest {
                input: "3 > 5 == false",
                exp: "((3 > 5) == false)",
            },
            PrecedenceTest {
                input: "3 < 5 == true",
                exp: "((3 < 5) == true)",
            },
            PrecedenceTest {
                input: "1 + (2 + 3) + 4",
                exp: "((1 + (2 + 3)) + 4)",
            },
            PrecedenceTest {
                input: "(5 + 5) * 2",
                exp: "((5 + 5) * 2)",
            },
            PrecedenceTest {
                input: "2 / (5 + 5)",
                exp: "(2 / (5 + 5))",
            },
            PrecedenceTest {
                input: "-(5 + 5)",
                exp: "(-(5 + 5))",
            },
            PrecedenceTest {
                input: "!(true == true)",
                exp: "(!(true == true))",
            },
        ];
        for test in tests {
            let l = Lexer::new(test.input.as_bytes());
            let mut p = Parser::new(l);
            let res = p.parse();
            check_errors(&p);
            let s = res.to_string();
            assert_eq!(s, test.exp);
        }
    }
}

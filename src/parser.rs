use std::error::Error;

use crate::{
    ast::{Expression, Program, Statement},
    lexer::Lexer,
    token::Token,
};

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

    pub fn parse(&mut self) -> Program {
        let mut stmts = Vec::new();

        while self.cur != Token::Eof {
            let stmt = self.parse_statement();
            match stmt {
                Ok(s) => {
                    stmts.push(s);
                    self.next_token();
                }
                Err(e) => {
                    let s = e.to_string();
                    self.errors.push(s);
                }
            }
        }

        return Program { statements: stmts };
    }

    pub fn errors(&self) -> &Vec<String> {
        return &self.errors;
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.cur {
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let exp = self.parse_expression()?;
        if self.peek == Token::Semicolon {
            self.next_token();
        }
        return Ok(Statement::ExpressionStatement(exp));
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        let cur = std::mem::take(&mut self.cur);
        let exp: Expression;
        match cur {
            Token::Ident(ident) => exp = Expression::Ident(ident),
            Token::Int(int) => {
                let val: i64 = int.parse()?;
                exp = Expression::Int(val);
            }
            _ => todo!(),
        };
        return Ok(exp);
    }

    fn next_token(&mut self) {
        std::mem::swap(&mut self.cur, &mut self.peek);
        self.peek = self.l.next_token();
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{Expression, LetStatement, Statement},
        lexer::Lexer,
    };

    use super::Parser;

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
        assert!(matches!(stmt, Statement::ExpressionStatement(_)));
        let ls = match &stmt {
            Statement::LetStatement(l) => l,
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
}

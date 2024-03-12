use crate::{
    ast::{Ast, Expression, Statement},
    object::Object,
};

pub fn eval(ast: Ast) -> Object {
    eval_statements(ast.statements)
}

fn eval_statements(stmts: Vec<Statement>) -> Object {
    let mut res = Object::default();
    for stmt in stmts {
        res = eval_statement(stmt);
    }
    return res;
}

fn eval_statement(stmt: Statement) -> Object {
    match stmt {
        Statement::ExpressionStatement(e) => eval_expression(e),
        _ => todo!(),
    }
}

fn eval_expression(e: Expression) -> Object {
    match e {
        Expression::Int(val) => Object::Int(val),
        _ => todo!(),
    }
}

#[cfg(test)]
mod test {
    use crate::{lexer::Lexer, object::Object, parser::Parser};

    use super::eval;

    struct IntTest {
        input: &'static str,
        expected: i64,
    }

    struct BoolTest {
        input: &'static str,
        expected: bool,
    }

    fn check_errors(p: &Parser) {
        let errs = p.errors();
        for e in errs {
            println!("{}", e)
        }
        assert_eq!(errs.len(), 0);
    }

    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input.as_bytes());
        let mut p = Parser::new(l);
        let ast = p.parse();
        check_errors(&p);
        return eval(ast);
    }

    fn test_int(got: &Object, exp: i64) {
        assert!(matches!(got, Object::Int(_)));
        let val = match got {
            Object::Int(i) => i,
            _ => unreachable!(),
        };
        assert_eq!(*val, exp);
    }

    fn test_bool(got: &Object, exp: bool) {
        assert!(matches!(got, Object::Bool(_)));
        let val = match got {
            Object::Bool(b) => b,
            _ => unreachable!(),
        };
        assert_eq!(*val, exp);
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = [
            IntTest {
                input: "5;",
                expected: 5,
            },
            IntTest {
                input: "10;",
                expected: 10,
            },
        ];

        for test in tests {
            let res = test_eval(test.input);
            test_int(&res, test.expected);
        }
    }

    #[test]
    fn test_eval_bool() {
        let tests = [
            BoolTest {
                input: "true;",
                expected: true,
            },
            BoolTest {
                input: "false;",
                expected: false,
            },
        ];

        for test in tests {
            let res = test_eval(test.input);
            test_bool(&res, test.expected);
        }
    }
}

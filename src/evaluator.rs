use crate::{
    ast::{Ast, Expression, InfixOperator, PrefixOperator, Statement},
    object::{Object, ObjectType},
};

const TRUE: Object = Object::Bool(true);
const FALSE: Object = Object::Bool(false);
const NULL: Object = Object::Null;

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
        Statement::ExpressionStatement(e) => eval_expression(&e),
        _ => todo!(),
    }
}

fn eval_expression(e: &Expression) -> Object {
    match e {
        Expression::Int(val) => Object::Int(*val),
        Expression::Boolean(val) => native_bool_to_boolean_object(*val),
        Expression::Prefix(prefix) => {
            let right = eval_expression(&prefix.right);
            return eval_prefix(&prefix.oper, right);
        }
        Expression::Infix(infix) => {
            let left = eval_expression(&infix.left);
            let right = eval_expression(&infix.right);
            return eval_infix(&infix.oper, left, right);
        }
        _ => todo!(),
    }
}

fn eval_prefix(oper: &PrefixOperator, right: Object) -> Object {
    match oper {
        PrefixOperator::Bang => eval_bang(right),
        PrefixOperator::Minus => eval_minus_prefix(right),
    }
}

fn eval_infix(oper: &InfixOperator, left: Object, right: Object) -> Object {
    if left.get_type() == ObjectType::Int && right.get_type() == ObjectType::Int {
        let lval = match left {
            Object::Int(i) => i,
            _ => unreachable!(),
        };
        let rval = match right {
            Object::Int(i) => i,
            _ => unreachable!(),
        };
        return eval_int_infix(oper, lval, rval);
    }
    if *oper == InfixOperator::Eq {
        return native_bool_to_boolean_object(left == right);
    }
    if *oper == InfixOperator::NotEq {
        return native_bool_to_boolean_object(left != right);
    }
    return NULL;
}

fn eval_int_infix(oper: &InfixOperator, left: i64, right: i64) -> Object {
    match oper {
        InfixOperator::Plus => Object::Int(left + right),
        InfixOperator::Minus => Object::Int(left - right),
        InfixOperator::Asterisk => Object::Int(left * right),
        InfixOperator::Slash => Object::Int(left / right),
        InfixOperator::Lt => native_bool_to_boolean_object(left < right),
        InfixOperator::Gt => native_bool_to_boolean_object(left > right),
        InfixOperator::Eq => native_bool_to_boolean_object(left == right),
        InfixOperator::NotEq => native_bool_to_boolean_object(left != right),
    }
}

fn eval_bang(right: Object) -> Object {
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_prefix(right: Object) -> Object {
    let right_val = match right {
        Object::Int(v) => v,
        _ => return NULL,
    };
    return Object::Int(-right_val);
}

fn native_bool_to_boolean_object(input: bool) -> Object {
    if input {
        return TRUE;
    }
    return FALSE;
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
            IntTest {
                input: "-5",
                expected: -5,
            },
            IntTest {
                input: "-10",
                expected: -10,
            },
            IntTest {
                input: "5 + 5 + 5 + 5 - 10",
                expected: 10,
            },
            IntTest {
                input: "2 * 2 * 2 * 2 * 2",
                expected: 32,
            },
            IntTest {
                input: "-50 + 100 + -50",
                expected: 0,
            },
            IntTest {
                input: "5 * 2 + 10",
                expected: 20,
            },
            IntTest {
                input: "5 + 2 * 10",
                expected: 25,
            },
            IntTest {
                input: "20 + 2 * -10",
                expected: 0,
            },
            IntTest {
                input: "50 / 2 * 2 + 10",
                expected: 60,
            },
            IntTest {
                input: "2 * (5 + 10)",
                expected: 30,
            },
            IntTest {
                input: "3 * 3 * 3 + 10",
                expected: 37,
            },
            IntTest {
                input: "3 * (3 * 3) + 10",
                expected: 37,
            },
            IntTest {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
                expected: 50,
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
            BoolTest {
                input: "!true",
                expected: false,
            },
            BoolTest {
                input: "!false",
                expected: true,
            },
            BoolTest {
                input: "!5",
                expected: false,
            },
            BoolTest {
                input: "!!true",
                expected: true,
            },
            BoolTest {
                input: "!!false",
                expected: false,
            },
            BoolTest {
                input: "!!5",
                expected: true,
            },
            BoolTest {
                input: "false",
                expected: false,
            },
            BoolTest {
                input: "1 < 2",
                expected: true,
            },
            BoolTest {
                input: "1 > 2",
                expected: false,
            },
            BoolTest {
                input: "1 < 1",
                expected: false,
            },
            BoolTest {
                input: "1 > 1",
                expected: false,
            },
            BoolTest {
                input: "1 == 1",
                expected: true,
            },
            BoolTest {
                input: "1 != 1",
                expected: false,
            },
            BoolTest {
                input: "1 == 2",
                expected: false,
            },
            BoolTest {
                input: "1 != 2",
                expected: true,
            },
            BoolTest {
                input: "true == true",
                expected: true,
            },
            BoolTest {
                input: "false == false",
                expected: true,
            },
            BoolTest {
                input: "true == false",
                expected: false,
            },
            BoolTest {
                input: "true != false",
                expected: true,
            },
            BoolTest {
                input: "false != true",
                expected: true,
            },
            BoolTest {
                input: "(1 < 2) == true",
                expected: true,
            },
            BoolTest {
                input: "(1 < 2) == false",
                expected: false,
            },
            BoolTest {
                input: "(1 > 2) == true",
                expected: false,
            },
            BoolTest {
                input: "(1 > 2) == false",
                expected: true,
            },
        ];

        for test in tests {
            let res = test_eval(test.input);
            test_bool(&res, test.expected);
        }
    }
}

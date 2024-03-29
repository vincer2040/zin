use std::ops::Deref;

use crate::{
    ast::{Ast, Block, Expression, IfExpression, InfixOperator, PrefixOperator, Statement},
    builtins::len,
    environment::Environment,
    object::{Builtin, Function, Object, ObjectType},
};

const TRUE: Object = Object::Bool(true);
const FALSE: Object = Object::Bool(false);
const NULL: Object = Object::Null;

pub fn eval(ast: Ast, env: &mut Environment) -> Object {
    eval_statements(&ast.statements, env)
}

fn eval_statements(stmts: &Vec<Statement>, env: &mut Environment) -> Object {
    let mut res = Object::default();
    for stmt in stmts {
        res = eval_statement(&stmt, env);
        if res.is_error() {
            return res;
        }
        if let Object::Return(r) = &res {
            return r.deref().to_owned();
        }
    }
    return res;
}

fn eval_statement(stmt: &Statement, env: &mut Environment) -> Object {
    match stmt {
        Statement::LetStatement(ls) => {
            let res = eval_expression(&ls.value, env);
            if res.is_error() {
                return res;
            }
            env.set(ls.name.to_owned(), res);
            return NULL;
        }
        Statement::ReturnStatement(e) => {
            let res = eval_expression(&e, env);
            if res.is_error() {
                return res;
            }
            Object::Return(Box::new(res))
        }
        Statement::ExpressionStatement(e) => eval_expression(&e, env),
    }
}

fn eval_expression(e: &Expression, env: &mut Environment) -> Object {
    match e {
        Expression::Int(val) => Object::Int(*val),
        Expression::Boolean(val) => native_bool_to_boolean_object(*val),
        Expression::String(s) => Object::String(s.to_string()),
        Expression::Ident(val) => match env.get(val) {
            Some(res) => res.to_owned(),
            None => {
                let err = format!("identifier not found: {}", val);
                Object::Error(err)
            }
        },
        Expression::Prefix(prefix) => {
            let right = eval_expression(&prefix.right, env);
            if right.is_error() {
                return right;
            }
            return eval_prefix(&prefix.oper, right);
        }
        Expression::Infix(infix) => {
            let left = eval_expression(&infix.left, env);
            if left.is_error() {
                return left;
            }
            let right = eval_expression(&infix.right, env);
            if right.is_error() {
                return right;
            }
            return eval_infix(&infix.oper, left, right);
        }
        Expression::IfExpression(if_exp) => eval_if_expression(if_exp, env),
        Expression::Function(func) => {
            let name = &func.name;
            let body = &func.body;
            let params = &func.params;
            let function = Function {
                name: name.to_owned(),
                params: params.to_owned(),
                body: body.to_owned(),
                env: env.clone(),
            };
            let obj = Object::Function(function);
            env.set(name.to_owned(), obj);
            return NULL;
        }
        Expression::Call(call) => {
            let func_obj = match env.get(&call.name) {
                Some(o) => o.clone(),
                None => match call.name.as_str() {
                    "len" => Object::Builtin(Builtin::Len),
                    _ => {
                        let err = format!("identifier not found: {}", call.name);
                        return Object::Error(err);
                    }
                },
            };
            let args = eval_expressions(&call.args, env);
            if args.len() == 1 && args[0].is_error() {
                return (&args[0]).to_owned();
            }
            return apply_function(func_obj, args);
        }
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
    if left.get_type() == ObjectType::String && right.get_type() == ObjectType::String {
        let lval = match left {
            Object::String(s) => s,
            _ => unreachable!(),
        };
        let rval = match right {
            Object::String(s) => s,
            _ => unreachable!(),
        };
        return eval_string_infix(oper, &lval, &rval);
    }
    if left.get_type() != right.get_type() {
        let err = format!(
            "type mismatch: {} {} {}",
            left.get_type().to_string(),
            oper.to_string(),
            right.get_type().to_string()
        );
        return Object::Error(err);
    }
    if *oper == InfixOperator::Eq {
        return native_bool_to_boolean_object(left == right);
    }
    if *oper == InfixOperator::NotEq {
        return native_bool_to_boolean_object(left != right);
    }
    let err = format!(
        "unknown operator: {} {} {}",
        left.get_type().to_string(),
        oper.to_string(),
        right.get_type().to_string()
    );
    return Object::Error(err);
}

fn eval_if_expression(if_exp: &IfExpression, env: &mut Environment) -> Object {
    let cond = eval_expression(&if_exp.cond, env);
    if cond.is_error() {
        return cond;
    }
    if is_truthy(&cond) {
        return eval_block(&if_exp.consequence, env);
    }
    if let Some(alt) = &if_exp.alternative {
        return eval_block(&alt, env);
    }
    return NULL;
}

fn eval_block(block: &Block, env: &mut Environment) -> Object {
    let mut res = Object::default();
    for stmt in &block.block {
        res = eval_statement(&stmt, env);
        if res.is_error() {
            return res;
        }
        if let Object::Return(_) = &res {
            return res;
        }
    }
    return res;
}

fn apply_function(func_obj: Object, args: Vec<Object>) -> Object {
    match func_obj {
        Object::Function(func) => {
            let mut extended = extend_function_env(&func, args);
            let res = eval_block(&func.body, &mut extended);
            return unwrap_return_value(res);
        }
        Object::Builtin(b) => match b {
            Builtin::Len => {
                return len(args);
            }
        },
        _ => {
            let err = format!("not a function: {}", func_obj.get_type().to_string());
            return Object::Error(err);
        }
    }
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::Return(r) = obj {
        return r.deref().to_owned();
    }
    return obj;
}

fn extend_function_env(func: &Function, args: Vec<Object>) -> Environment {
    let mut env = Environment::new_enclosed((&func.env).to_owned());
    for (i, param) in func.params.iter().enumerate() {
        env.set(param.to_string(), (&args[i]).to_owned());
    }
    return env;
}

fn eval_expressions(es: &Vec<Expression>, env: &mut Environment) -> Vec<Object> {
    let mut res = Vec::new();
    for e in es {
        let o = eval_expression(e, env);
        if o.is_error() {
            res.clear();
            res.push(o);
            return res;
        }
        res.push(o);
    }
    return res;
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Bool(val) => *val,
        Object::Int(_) => true,
        Object::Null => false,
        _ => true,
    }
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

fn eval_string_infix(oper: &InfixOperator, left: &str, right: &str) -> Object {
    if *oper != InfixOperator::Plus {
        let err = format!(
            "unknown operator: {} {} {}",
            "STRING",
            oper.to_string(),
            "STRING"
        );
        return Object::Error(err);
    }
    let s = String::new() + left + right;
    return Object::String(s);
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
        _ => {
            let err = format!("unknown operator: -{}", right.get_type().to_string());
            return Object::Error(err);
        }
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
    use crate::{environment::Environment, lexer::Lexer, object::Object, parser::Parser};

    use super::eval;

    struct Test<T> {
        input: &'static str,
        expected: T,
    }

    enum LenTestExpected {
        String(&'static str),
        Int(i64),
    }

    fn check_errors(p: &Parser) {
        let errs = p.errors();
        for e in errs {
            println!("{}", e)
        }
        assert_eq!(errs.len(), 0);
    }

    fn test_eval(input: &str) -> Object {
        let mut env = Environment::new();
        let l = Lexer::new(input.as_bytes());
        let mut p = Parser::new(l);
        let ast = p.parse();
        check_errors(&p);
        return eval(ast, &mut env);
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

    fn test_null(got: &Object) {
        assert!(matches!(got, Object::Null));
    }

    fn test_error(got: &Object, exp: &str) {
        assert!(matches!(got, Object::Error(_)));
        let e = match got {
            Object::Error(e) => e,
            _ => unreachable!(),
        };
        assert_eq!(e, exp);
    }

    fn test_string(got: &Object, exp: &str) {
        assert!(matches!(got, Object::String(_)));
        let s = match got {
            Object::String(s) => s,
            _ => unreachable!(),
        };
        assert_eq!(s, exp);
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = [
            Test {
                input: "5;",
                expected: 5,
            },
            Test {
                input: "10;",
                expected: 10,
            },
            Test {
                input: "-5",
                expected: -5,
            },
            Test {
                input: "-10",
                expected: -10,
            },
            Test {
                input: "5 + 5 + 5 + 5 - 10",
                expected: 10,
            },
            Test {
                input: "2 * 2 * 2 * 2 * 2",
                expected: 32,
            },
            Test {
                input: "-50 + 100 + -50",
                expected: 0,
            },
            Test {
                input: "5 * 2 + 10",
                expected: 20,
            },
            Test {
                input: "5 + 2 * 10",
                expected: 25,
            },
            Test {
                input: "20 + 2 * -10",
                expected: 0,
            },
            Test {
                input: "50 / 2 * 2 + 10",
                expected: 60,
            },
            Test {
                input: "2 * (5 + 10)",
                expected: 30,
            },
            Test {
                input: "3 * 3 * 3 + 10",
                expected: 37,
            },
            Test {
                input: "3 * (3 * 3) + 10",
                expected: 37,
            },
            Test {
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
            Test {
                input: "true;",
                expected: true,
            },
            Test {
                input: "false;",
                expected: false,
            },
            Test {
                input: "!true",
                expected: false,
            },
            Test {
                input: "!false",
                expected: true,
            },
            Test {
                input: "!5",
                expected: false,
            },
            Test {
                input: "!!true",
                expected: true,
            },
            Test {
                input: "!!false",
                expected: false,
            },
            Test {
                input: "!!5",
                expected: true,
            },
            Test {
                input: "false",
                expected: false,
            },
            Test {
                input: "1 < 2",
                expected: true,
            },
            Test {
                input: "1 > 2",
                expected: false,
            },
            Test {
                input: "1 < 1",
                expected: false,
            },
            Test {
                input: "1 > 1",
                expected: false,
            },
            Test {
                input: "1 == 1",
                expected: true,
            },
            Test {
                input: "1 != 1",
                expected: false,
            },
            Test {
                input: "1 == 2",
                expected: false,
            },
            Test {
                input: "1 != 2",
                expected: true,
            },
            Test {
                input: "true == true",
                expected: true,
            },
            Test {
                input: "false == false",
                expected: true,
            },
            Test {
                input: "true == false",
                expected: false,
            },
            Test {
                input: "true != false",
                expected: true,
            },
            Test {
                input: "false != true",
                expected: true,
            },
            Test {
                input: "(1 < 2) == true",
                expected: true,
            },
            Test {
                input: "(1 < 2) == false",
                expected: false,
            },
            Test {
                input: "(1 > 2) == true",
                expected: false,
            },
            Test {
                input: "(1 > 2) == false",
                expected: true,
            },
        ];

        for test in tests {
            let res = test_eval(test.input);
            test_bool(&res, test.expected);
        }
    }

    #[test]
    fn test_if_else() {
        let tests = [
            Test {
                input: "if (true) { 10 }",
                expected: Some(10),
            },
            Test {
                input: "if (false) { 10 }",
                expected: None,
            },
            Test {
                input: "if (1) { 10 }",
                expected: Some(10),
            },
            Test {
                input: "if (1 < 2) { 10 }",
                expected: Some(10),
            },
            Test {
                input: "if (1 > 2) { 10 }",
                expected: None,
            },
            Test {
                input: "if (1 > 2) { 10 } else { 20 }",
                expected: Some(20),
            },
            Test {
                input: "if (1 < 2) { 10 } else { 20 }",
                expected: Some(10),
            },
        ];

        for test in tests {
            let res = test_eval(test.input);
            match test.expected {
                Some(val) => test_int(&res, val),
                None => test_null(&res),
            }
        }
    }

    #[test]
    fn test_return_statement() {
        let tests = [
            Test {
                input: "return 10;",
                expected: 10,
            },
            Test {
                input: "return 10; 9;",
                expected: 10,
            },
            Test {
                input: "return 2 * 5; 9;",
                expected: 10,
            },
            Test {
                input: "9; return 2 * 5; 9;",
                expected: 10,
            },
            Test {
                input: "
                    if (10 > 1) {
                        if (10 > 1) {
                            return 10;
                        }
                        return 1
                    }
                    ",
                expected: 10,
            },
        ];
        for test in tests {
            let res = test_eval(test.input);
            test_int(&res, test.expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = [
            Test {
                input: "5 + true;",
                expected: "type mismatch: INTEGER + BOOLEAN",
            },
            Test {
                input: "5 + true; 5;",
                expected: "type mismatch: INTEGER + BOOLEAN",
            },
            Test {
                input: "-true",
                expected: "unknown operator: -BOOLEAN",
            },
            Test {
                input: "true + false;",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
            Test {
                input: "5; true + false; 5",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
            Test {
                input: "if (10 > 1) { true + false; }",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
            Test {
                input: "
                if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }
                    return 1;
                }
                ",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
            Test {
                input: "foobar",
                expected: "identifier not found: foobar",
            },
            Test {
                input: "\"Hello\" - \"World\"",
                expected: "unknown operator: STRING - STRING",
            },
        ];

        for test in tests {
            let res = test_eval(test.input);
            test_error(&res, test.expected);
        }
    }

    #[test]
    fn test_let_statement() {
        let tests = [
            Test {
                input: "let a = 5; a;",
                expected: 5,
            },
            Test {
                input: "let a = 5 * 5; a;",
                expected: 25,
            },
            Test {
                input: "let a = 5; let b = a; b;",
                expected: 5,
            },
            Test {
                input: "let a = 5; let b = a; let c = a + b + 5; c;",
                expected: 15,
            },
        ];

        for test in tests {
            let res = test_eval(test.input);
            test_int(&res, test.expected);
        }
    }

    #[test]
    fn test_function_application() {
        let tests = [
            Test {
                input: "fn identity(x) { x; }; identity(5);",
                expected: 5,
            },
            Test {
                input: "fn identity(x) { return x; }; identity(5);",
                expected: 5,
            },
            Test {
                input: "fn double(x) { x * 2; }; double(5);",
                expected: 10,
            },
            Test {
                input: "fn add(x, y) { x + y; }; add(5, 5);",
                expected: 10,
            },
            Test {
                input: "fn add(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                expected: 20,
            },
        ];

        for test in tests {
            let res = test_eval(test.input);
            test_int(&res, test.expected);
        }
    }

    #[test]
    fn test_closures() {
        let input = "
            fn newAdder(x) {
                fn adder(y) { x + y };
                return adder;
            };
            let addTwo = newAdder(2);
            addTwo(2);
            ";

        let res = test_eval(input);
        test_int(&res, 4);
    }

    #[test]
    fn test_strings() {
        let input = "\"hello world\"";
        let res = test_eval(input);
        test_string(&res, "hello world");
    }

    #[test]
    fn test_string_concatination() {
        let input = "\"hello\" + \" \" + \"world\"";
        let res = test_eval(input);
        test_string(&res, "hello world");
    }

    #[test]
    fn test_builtin_len() {
        let tests = [
            Test {
                input: "len(\"\")",
                expected: LenTestExpected::Int(0),
            },
            Test {
                input: "len(\"four\")",
                expected: LenTestExpected::Int(4),
            },
            Test {
                input: "len(1)",
                expected: LenTestExpected::String("argument to 'len' not supported, got INTEGER"),
            },
            Test {
                input: "len(\"one\", \"two\")",
                expected: LenTestExpected::String("wrong number of arguments. got=2, want=1"),
            },
        ];

        for (i, test) in tests.iter().enumerate() {
            println!("{}", i);
            let res = test_eval(&test.input);
            match test.expected {
                LenTestExpected::Int(i) => test_int(&res, i),
                LenTestExpected::String(s) => test_error(&res, s),
            }
        }
    }
}

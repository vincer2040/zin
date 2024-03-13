use std::io::{stdin, stdout, Write};

use environment::Environment;
use lexer::Lexer;

use crate::{evaluator::eval, parser::Parser};

mod ast;
mod builtins;
mod environment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod token;
mod util;

fn main() -> std::io::Result<()> {
    let mut env = Environment::new();
    loop {
        let mut line = String::new();
        print!(">>> ");
        stdout().flush()?;
        let _ = stdin().read_line(&mut line)?;
        if line == "exit\n" {
            break;
        }
        let l = Lexer::new(line.as_bytes());
        let mut p = Parser::new(l);
        let ast = p.parse();
        if !check_errors(&p) {
            continue;
        }
        let evaluated = eval(ast, &mut env);
        let obj_string = evaluated.inspect();
        println!("{}", obj_string);
    }
    Ok(())
}

fn check_errors(p: &Parser) -> bool {
    let errs = p.errors();
    for e in errs {
        println!("{}", e);
    }
    return errs.len() == 0;
}

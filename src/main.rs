use std::io::{stdin, stdout, Write};

use lexer::Lexer;

use crate::parser::Parser;

mod ast;
mod lexer;
mod parser;
mod token;
mod util;

fn main() -> std::io::Result<()> {
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
        let _ = p.parse();
        check_errors(&p);
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

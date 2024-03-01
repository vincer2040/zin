use std::io::{stdin, stdout, Write};

use lexer::Lexer;
use token::Token;

mod lexer;
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
        let mut l = Lexer::new(line.as_bytes());
        let mut tok = l.next_token();
        while tok != Token::Eof {
            println!("{:#?}", tok);
            tok = l.next_token();
        }
    }
    Ok(())
}

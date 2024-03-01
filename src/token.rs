
#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Eof,
    Illegal,
    Assign,
    Plus,
    Minus,
    Bang,
    Eq,
    NotEq,
    Lt,
    Gt,
    Semicolon,
    Comma,
    LParen,
    RParen,
    LSquirly,
    RSquirly,
    Let,
    Function,
    Return,
    Ident(String),
    Int(String),
}

impl Into<Token> for String {
    fn into(self) -> Token {
        match self.as_str() {
            "fn" => Token::Function,
            "let" => Token::Let,
            "return" => Token::Return,
            _ => Token::Ident(self)
        }
    }
}

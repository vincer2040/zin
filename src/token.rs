#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Eof,
    Illegal,
    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
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
    True,
    False,
    Ident(String),
    Int(String),
}

impl Into<Token> for String {
    fn into(self) -> Token {
        match self.as_str() {
            "fn" => Token::Function,
            "let" => Token::Let,
            "true" => Token::True,
            "false" => Token::False,
            "return" => Token::Return,
            _ => Token::Ident(self),
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Token::Illegal
    }
}

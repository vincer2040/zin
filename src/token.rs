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
    If,
    Else,
    Function,
    Return,
    True,
    False,
    String(String),
    Ident(String),
    Int(String),
}

impl Into<Token> for String {
    fn into(self) -> Token {
        match self.as_str() {
            "fn" => Token::Function,
            "if" => Token::If,
            "let" => Token::Let,
            "else" => Token::Else,
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

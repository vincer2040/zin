use crate::{
    token::Token,
    util::{is_digit, is_letter},
};

pub struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8]) -> Self {
        let mut res = Self {
            input,
            pos: 0,
            ch: 0,
        };
        res.read_btye();
        return res;
    }

    pub fn next_token(&mut self) -> Token {
        let tok: Token;
        self.skip_whitespace();
        match self.ch {
            b'=' => {
                if self.peek_byte() == b'=' {
                    self.read_btye();
                    tok = Token::Eq;
                } else {
                    tok = Token::Assign;
                }
            }
            b'!' => {
                if self.peek_byte() == b'=' {
                    self.read_btye();
                    tok = Token::NotEq;
                } else {
                    tok = Token::Bang;
                }
            }
            b'+' => tok = Token::Plus,
            b'-' => tok = Token::Minus,
            b'*' => tok = Token::Asterisk,
            b'/' => tok = Token::Slash,
            b'<' => tok = Token::Lt,
            b'>' => tok = Token::Gt,
            b';' => tok = Token::Semicolon,
            b',' => tok = Token::Comma,
            b'(' => tok = Token::LParen,
            b')' => tok = Token::RParen,
            b'{' => tok = Token::LSquirly,
            b'}' => tok = Token::RSquirly,
            0 => tok = Token::Eof,
            _ => {
                if is_letter(self.ch) {
                    let ident = self.read_ident();
                    return ident.into();
                } else if is_digit(self.ch) {
                    let int = self.read_int();
                    return Token::Int(int);
                } else {
                    tok = Token::Illegal;
                }
            }
        }
        self.read_btye();
        return tok;
    }

    fn peek_byte(&self) -> u8 {
        if self.pos >= self.input.len() {
            return 0;
        }
        return self.input[self.pos];
    }

    fn read_ident(&mut self) -> String {
        let mut res = String::new();
        while is_letter(self.ch) {
            res.push(self.ch as char);
            self.read_btye();
        }
        return res;
    }

    fn read_int(&mut self) -> String {
        let mut res = String::new();
        while is_digit(self.ch) {
            res.push(self.ch as char);
            self.read_btye();
        }
        return res;
    }

    fn read_btye(&mut self) {
        if self.pos >= self.input.len() {
            self.ch = 0;
            return;
        }
        self.ch = self.input[self.pos];
        self.pos += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_btye();
        }
    }
}

#[cfg(test)]
mod test {
    use crate::token::Token;

    use super::Lexer;

    #[test]
    fn basic_test() {
        let input = "=+(){}";
        let mut lexer = Lexer::new(input.as_bytes());
        let exps = [
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LSquirly,
            Token::RSquirly,
            Token::Eof,
        ];
        for exp in exps {
            let tok = lexer.next_token();
            assert_eq!(tok, exp);
        }
    }

    #[test]
    fn basic_program() {
        let input = "
        let five = 5;
        let ten = 10;
        fn add(a, b) {
            return a + b;
        }
        let res = add(five, ten);
            ";
        let mut l = Lexer::new(input.as_bytes());
        let exps = [
            Token::Let,
            Token::Ident("five".into()),
            Token::Assign,
            Token::Int("5".into()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".into()),
            Token::Assign,
            Token::Int("10".into()),
            Token::Semicolon,
            Token::Function,
            Token::Ident("add".into()),
            Token::LParen,
            Token::Ident("a".into()),
            Token::Comma,
            Token::Ident("b".into()),
            Token::RParen,
            Token::LSquirly,
            Token::Return,
            Token::Ident("a".into()),
            Token::Plus,
            Token::Ident("b".into()),
            Token::Semicolon,
            Token::RSquirly,
            Token::Let,
            Token::Ident("res".into()),
            Token::Assign,
            Token::Ident("add".into()),
            Token::LParen,
            Token::Ident("five".into()),
            Token::Comma,
            Token::Ident("ten".into()),
            Token::RParen,
            Token::Semicolon,
            Token::Eof,
        ];

        for exp in exps {
            let tok = l.next_token();
            assert_eq!(tok, exp);
        }
    }

    #[test]
    fn test_peek() {
        let input = "
            10 != 9;
            10 == 10;
        ";
        let mut l = Lexer::new(input.as_bytes());
        let exps = [
            Token::Int("10".into()),
            Token::NotEq,
            Token::Int("9".into()),
            Token::Semicolon,
            Token::Int("10".into()),
            Token::Eq,
            Token::Int("10".into()),
            Token::Semicolon,
            Token::Eof,
        ];

        for exp in exps {
            let tok = l.next_token();
            assert_eq!(tok, exp);
        }
    }
}

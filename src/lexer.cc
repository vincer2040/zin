#include "lexer.hh"
#include "token.hh"
#include <string>

static bool is_letter(char ch);
static bool is_valid_ident_char(char ch);

zinc::lexer::lexer(std::string input) : input(std::move(input)) {
    pos = 0;
    ch = 0;
    read_char();
}

zinc::token zinc::lexer::next_token() {
    zinc::token tok;
    skip_whitespace();
    switch (ch) {
    case '=':
        if (peek_char() == '=') {
            read_char();
            tok.type = zinc::tokent::Eq;
            break;
        }
        tok.type = zinc::tokent::Assign;
        break;
    case '+':
        tok.type = zinc::tokent::Plus;
        break;
    case '-':
        if (peek_char() == '>') {
            read_char();
            tok.type = zinc::tokent::Arrow;
            break;
        }
        tok.type = zinc::tokent::Minus;
        break;
    case '*':
        tok.type = zinc::tokent::Asterisk;
        break;
    case '/':
        tok.type = zinc::tokent::Slash;
        break;
    case ';':
        tok.type = zinc::tokent::Semicolon;
        break;
    case ':':
        tok.type = zinc::tokent::Colon;
        break;
    case ',':
        tok.type = zinc::tokent::Comma;
        break;
    case '(':
        tok.type = zinc::tokent::LParen;
        break;
    case ')':
        tok.type = zinc::tokent::RParen;
        break;
    case '{':
        tok.type = zinc::tokent::LSquirly;
        break;
    case '}':
        tok.type = zinc::tokent::RSquirly;
        break;
    case '!':
        if (peek_char() == '=') {
            read_char();
            tok.type = zinc::tokent::NotEq;
            break;
        }
        tok.type = zinc::tokent::Bang;
        break;
    case '<':
        tok.type = zinc::tokent::Lt;
        break;
    case '>':
        tok.type = zinc::tokent::Gt;
        break;
    case 0:
        tok.type = zinc::tokent::Eof;
        break;
    default:
        if (is_letter(ch)) {
            std::string ident = read_string();
            tok = lookup_ident(std::move(ident));
            return tok;
        } else if (isdigit(ch)) {
            std::string num = read_int();
            tok.type = zinc::tokent::Int;
            tok.literal = num;
            return tok;
        }
        break;
    }
    read_char();
    return tok;
}

void zinc::lexer::read_char() {
    if (pos >= input.length()) {
        ch = 0;
        return;
    }
    ch = input[pos];
    pos++;
}

std::string zinc::lexer::read_string() {
    std::string res;
    while (is_valid_ident_char(ch)) {
        res.push_back(ch);
        read_char();
    }
    return res;
}

std::string zinc::lexer::read_int() {
    std::string res;
    while (isdigit(ch)) {
        res.push_back(ch);
        read_char();
    }
    return res;
}

void zinc::lexer::skip_whitespace() {
    while (ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n') {
        read_char();
    }
}

char zinc::lexer::peek_char() {
    if (pos >= input.length()) {
        return 0;
    }
    return input[pos];
}

static bool is_letter(char ch) {
    return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_';
}

static bool is_valid_ident_char(char ch) {
    return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_' ||
           isdigit(ch);
}

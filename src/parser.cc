#include "parser.hh"
#include <variant>

zinc::parser::parser(lexer l) : l(std::move(l)) {
    next_token();
    next_token();
}

zinc::ast zinc::parser::parse() {
    std::vector<statement> statements;
    while (cur.type != zinc::tokent::Eof) {
        statement stmt = parse_statement();
        if (stmt.type != statement::type::Invalid) {
            statements.push_back(std::move(stmt));
        }
        next_token();
    }
    return {std::move(statements)};
}

std::vector<std::string>& zinc::parser::get_errors() { return errors; }

zinc::statement zinc::parser::parse_statement() {
    switch (cur.type) {
    case tokent::Let:
        return parse_let();
    default:
        break;
    }

    return {statement::type::Invalid, std::monostate()};
}

zinc::statement zinc::parser::parse_let() {
    if (!expect_peek(tokent::Ident)) {
        return {statement::type::Invalid, std::monostate()};
    }
    std::string ident_value = std::get<std::string>(cur.literal);
    while (!cur_token_is(tokent::Semicolon)) {
        next_token();
    }

    identifer ident = {std::move(ident_value), data_type::Infer};
    let_statement let = {
        std::move(ident),
        {expression::type::Invalid, std::monostate()},
    };
    statement stmt = {statement::type::Let, std::move(let)};
    return stmt;
}

void zinc::parser::next_token() {
    std::swap(cur, peek);
    peek = l.next_token();
}

bool inline zinc::parser::cur_token_is(tokent type) { return cur.type == type; }

bool inline zinc::parser::peek_token_is(tokent type) {
    return peek.type == type;
}

bool zinc::parser::expect_peek(tokent type) {
    if (peek_token_is(type)) {
        next_token();
        return true;
    }
    return false;
}

void zinc::parser::peek_error(tokent type) {
    std::string err = "expected next token to be " +
                      std::string((zinc::token_type_string(type))) + ", got " +
                      cur.type_to_string() + " instead";
    errors.push_back(std::move(err));
}

#include "parser.hh"
#include "token.hh"
#include <cstdint>
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
    case tokent::Return:
        return parse_return();
    default:
        return parse_expression_statement();
    }

    // todo: unreachable
    return {statement::type::Invalid, std::monostate()};
}

zinc::statement zinc::parser::parse_let() {
    if (!expect_peek(tokent::Ident)) {
        return {statement::type::Invalid, std::monostate()};
    }
    std::string ident_value = std::get<std::string>(cur.literal);

    // todo: parse type and expression
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

zinc::statement zinc::parser::parse_return() {
    // todo: expression
    while (!cur_token_is(tokent::Semicolon)) {
        next_token();
    }

    zinc::expression e = {expression::type::Invalid, std::monostate()};

    statement stmt = {
        statement::type::Return,
        std::move(e),
    };

    return stmt;
}

zinc::statement zinc::parser::parse_expression_statement() {
    expression e = parse_expression(zinc::precedence::Lowest);
    if (peek_token_is(tokent::Semicolon)) {
        next_token();
    }
    return {statement::type::Expression, std::move(e)};
}

zinc::expression zinc::parser::parse_expression(zinc::precedence prec) {
    UNUSED(prec);
    switch (cur.type) {
    case tokent::Ident:
        return parse_ident_expression();
    case tokent::Int:
        return parse_int_expression();
    default: {
        std::string err =
            "no prefix parse function for " + std::string(cur.type_to_string());
        errors.push_back(std::move(err));
        return {expression::type::Invalid, std::monostate()};
    }
    }
    return {expression::type::Invalid, std::monostate()};
}

zinc::expression zinc::parser::parse_ident_expression() {
    std::string name = std::get<std::string>(cur.literal);
    identifer ident = {std::move(name), data_type::Infer};
    expression e = {expression::type::Identifier, std::move(ident)};
    return e;
}

zinc::expression zinc::parser::parse_int_expression() {
    std::string num_literal = std::get<std::string>(cur.literal);
    uint64_t value = std::stoull(num_literal);
    expression e = {expression::type::Integer, value};
    return e;
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

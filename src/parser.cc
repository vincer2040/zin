#include "parser.hh"
#include "ast.hh"
#include "token.hh"
#include <cstdint>
#include <memory>
#include <variant>

namespace zinc {

parser::parser(lexer l) : l(std::move(l)) {
    next_token();
    next_token();
}

ast parser::parse() {
    std::vector<statement> statements;
    while (cur.type != tokent::Eof) {
        statement stmt = parse_statement();
        if (stmt.type != statement::type::Invalid) {
            statements.push_back(std::move(stmt));
        }
        next_token();
    }
    return {std::move(statements)};
}

std::vector<std::string>& parser::get_errors() { return errors; }

statement parser::parse_statement() {
    switch (cur.type) {
    case tokent::Let:
        return parse_let();
    case tokent::Return:
        return parse_return();
    default:
        return parse_expression_statement();
    }
}

statement parser::parse_let() {
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

statement parser::parse_return() {
    // todo: expression
    while (!cur_token_is(tokent::Semicolon)) {
        next_token();
    }

    expression e = {expression::type::Invalid, std::monostate()};

    statement stmt = {
        statement::type::Return,
        std::move(e),
    };

    return stmt;
}

statement parser::parse_expression_statement() {
    expression e = parse_expression(precedence::Lowest);
    if (peek_token_is(tokent::Semicolon)) {
        next_token();
    }
    return {statement::type::Expression, std::move(e)};
}

expression parser::parse_expression(precedence prec) {
    expression left;
    switch (cur.type) {
    case tokent::Ident:
        left = parse_ident_expression();
        break;
    case tokent::Int:
        left = parse_int_expression();
        break;
    case tokent::True:
        left = parse_bool_expression(true);
        break;
    case tokent::False:
        left = parse_bool_expression(false);
        break;
    case tokent::Minus:
        left = parse_prefix_expression(prefix_operator::Minus);
        break;
    case tokent::Bang:
        left = parse_prefix_expression(prefix_operator::Bang);
        break;
    default: {
        std::string err =
            "no prefix parse function for " + std::string(cur.type_to_string());
        errors.push_back(std::move(err));
        return {expression::type::Invalid, std::monostate()};
    }
    }

    while (!peek_token_is(tokent::Semicolon) && (prec < peek_precedence())) {
        switch (peek.type) {
        case tokent::Plus:
            next_token();
            left =
                parse_infix_expression(infix_operator::Plus, std::move(left));
            break;
        case tokent::Minus:
            next_token();
            left =
                parse_infix_expression(infix_operator::Minus, std::move(left));
            break;
        case tokent::Asterisk:
            next_token();
            left = parse_infix_expression(infix_operator::Asterisk,
                                          std::move(left));
            break;
        case tokent::Slash:
            next_token();
            left =
                parse_infix_expression(infix_operator::Slash, std::move(left));
            break;
        case tokent::Lt:
            next_token();
            left = parse_infix_expression(infix_operator::Lt, std::move(left));
            break;
        case tokent::Gt:
            next_token();
            left = parse_infix_expression(infix_operator::Gt, std::move(left));
            break;
        case tokent::Eq:
            next_token();
            left = parse_infix_expression(infix_operator::Eq, std::move(left));
            break;
        case tokent::NotEq:
            next_token();
            left =
                parse_infix_expression(infix_operator::NotEq, std::move(left));
            break;
        default:
            return left;
        }
    }
    return left;
}

expression parser::parse_ident_expression() {
    std::string name = std::get<std::string>(cur.literal);
    identifer ident = {std::move(name), data_type::Infer};
    expression e = {expression::type::Identifier, std::move(ident)};
    return e;
}

expression parser::parse_int_expression() {
    std::string num_literal = std::get<std::string>(cur.literal);
    uint64_t value = std::stoull(num_literal);
    expression e = {expression::type::Integer, value};
    return e;
}

expression parser::parse_bool_expression(bool value) {
    expression e = {expression::type::Boolean, value};
    return e;
}

expression parser::parse_prefix_expression(prefix_operator oper) {
    next_token();
    auto right = std::make_unique<expression>(
        std::move(parse_expression(precedence::Lowest)));
    prefix_expression pe = {oper, std::move(right)};
    expression e = {expression::type::Prefix, std::move(pe)};
    return e;
}

expression parser::parse_infix_expression(infix_operator oper,
                                          expression left) {
    auto leftu = std::make_unique<expression>(std::move(left));
    precedence prec = cur_precedence();
    next_token();
    auto right = std::make_unique<expression>(parse_expression(prec));
    infix_expression infix = {oper, std::move(leftu), std::move(right)};
    expression e = {expression::type::Infix, std::move(infix)};
    return e;
}

void parser::next_token() {
    std::swap(cur, peek);
    peek = l.next_token();
}

bool inline parser::cur_token_is(tokent type) { return cur.type == type; }

bool inline parser::peek_token_is(tokent type) { return peek.type == type; }

bool parser::expect_peek(tokent type) {
    if (peek_token_is(type)) {
        next_token();
        return true;
    }
    return false;
}

precedence parser::precedence_from_tokent(tokent type) {
    switch (type) {
    case tokent::Eq:
    case tokent::NotEq:
        return precedence::Equals;
    case tokent::Lt:
    case tokent::Gt:
        return precedence::LessGreater;
    case tokent::Plus:
    case tokent::Minus:
        return precedence::Sum;
    case tokent::Asterisk:
    case tokent::Slash:
        return precedence::Product;
    default:
        break;
    }

    return precedence::Lowest;
}

precedence parser::cur_precedence() { return precedence_from_tokent(cur.type); }

precedence parser::peek_precedence() {
    return precedence_from_tokent(peek.type);
}

void parser::peek_error(tokent type) {
    std::string err = "expected next token to be " +
                      std::string((token_type_string(type))) + ", got " +
                      cur.type_to_string() + " instead";
    errors.push_back(std::move(err));
}
} // namespace zinc

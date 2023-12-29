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

    identifier ident = {std::move(ident_value), data_type::Infer};
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
    case tokent::Function:
        left = parse_function();
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
        case tokent::LParen:
            next_token();
            left = parse_call(std::move(left));
            break;
        default:
            return left;
        }
    }
    return left;
}

expression parser::parse_ident_expression() {
    std::string name = std::get<std::string>(cur.literal);
    identifier ident = {std::move(name), data_type::Infer};
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

expression parser::parse_function() {
    next_token();
    data_type return_type = data_type::Unit;
    std::string name = std::move(std::get<std::string>(cur.literal));
    if (!expect_peek((tokent::LParen))) {
        return {expression::type::Invalid, std::monostate()};
    }
    std::vector<identifier> params = parse_function_params();
    if (peek_token_is(tokent::Arrow)) {
        next_token();
        next_token();
        return_type = parse_data_type();
        if (return_type == data_type::Unkown) {
            std::string error = "unkown function return type";
            errors.push_back(std::move(error));
            return {expression::type::Invalid, std::monostate()};
        }
    }
    identifier ident = {std::move(name), return_type};
    if (!expect_peek(tokent::LSquirly)) {
        return {expression::type::Invalid, std::monostate()};
    }
    next_token();
    block_statement bs = parse_block();
    function fn = {std::move(ident), std::move(params), std::move(bs)};
    return {expression::type::Function, std::move(fn)};
}

expression parser::parse_call(expression function) {
    auto functionu = std::make_unique<expression>(std::move(function));
    std::vector<expression> args = parse_function_args();
    call c = {std::move(functionu), std::move(args)};
    expression e = {expression::type::Call, std::move(c)};
    return e;
}

std::vector<identifier> parser::parse_function_params() {
    std::vector<identifier> params;
    next_token();
    if (cur_token_is(tokent::RParen)) {
        return params;
    }
    identifier param = parse_identifier();
    if (param.type == data_type::Unkown) {
        std::string error = "function params must have a type";
        errors.push_back(error);
        return params;
    }
    params.push_back(std::move(param));
    while (!peek_token_is(tokent::RParen)) {
        next_token();
        next_token();
        identifier p = parse_identifier();
        if (p.type == data_type::Unkown) {
            std::string error = "function params must have a type";
            errors.push_back(error);
            params.clear();
            return params;
        }

        params.push_back(std::move(p));
    }
    next_token();
    return params;
}

std::vector<expression> parser::parse_function_args() {
    std::vector<expression> args;
    next_token();
    if (cur_token_is(tokent::RParen)) {
        return args;
    }
    expression a = parse_expression(precedence::Lowest);
    if (a.type == expression::type::Invalid) {
        return args;
    }
    args.push_back(std::move(a));
    while (!peek_token_is(tokent::RParen)) {
        next_token();
        next_token();
        expression arg = parse_expression(precedence::Lowest);
        if (arg.type == expression::type::Invalid) {
            args.clear();
            return args;
        }
        args.push_back(std::move(arg));
    }
    next_token();
    return args;
}

block_statement parser::parse_block() {
    block_statement bs;
    while (!cur_token_is(tokent::RSquirly)) {
        statement stmt = parse_statement();
        bs.push_back(std::move(stmt));
        next_token();
    }
    return bs;
}

identifier parser::parse_identifier() {
    std::string name = std::move(std::get<std::string>(cur.literal));
    if (!peek_token_is(tokent::Colon)) {
        return {std::move(name), data_type::Infer};
    }
    next_token();
    next_token();
    data_type type = parse_data_type();
    return {std::move(name), type};
}

data_type parser::parse_data_type() {
    switch (cur.type) {
    case tokent::u8:
        return data_type::u8;
    case tokent::u16:
        return data_type::u16;
    case tokent::u32:
        return data_type::u32;
    case tokent::u64:
        return data_type::u64;
    case tokent::i8:
        return data_type::i8;
    case tokent::i16:
        return data_type::i16;
    case tokent::i32:
        return data_type::i32;
    case tokent::i64:
        return data_type::i64;
    case tokent::Bool:
        return data_type::Bool;
    default:
        break;
    }
    return data_type::Unkown;
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
    case tokent::LParen:
        return precedence::Call;
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

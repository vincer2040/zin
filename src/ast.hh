#pragma once

#include <cstdint>
#include <memory>
#include <string>
#include <variant>
#include <vector>

namespace zinc {
struct ast {
    std::vector<struct statement> statements;
};

enum class data_type {
    Infer,
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    Bool,
};

struct identifer {
    std::string name;
    data_type type;
};

enum class prefix_operator {
    Bang,
    Minus,
};

struct prefix_expression {
    prefix_operator oper;
    std::unique_ptr<struct expression> right;
};

enum class infix_operator {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,
};

struct infix_expression {
    infix_operator oper;
    std::unique_ptr<struct expression> left;
    std::unique_ptr<struct expression> right;
};

using expression_data = std::variant<std::monostate, identifer, uint64_t, bool,
                                     prefix_expression, infix_expression>;

struct expression {
    enum class type {
        Invalid,
        Identifier,
        Integer,
        Boolean,
        Prefix,
        Infix,
    } type;
    expression_data data;
    expression() : type(expression::type::Invalid), data(std::monostate()) {}
    expression(enum type type, expression_data data)
        : type(type), data(std::move(data)) {}
};

struct let_statement {
    identifer ident;
    expression e;
};

struct statement {
    enum class type {
        Invalid,
        Let,
        Return,
        Expression,
    } type;
    std::variant<std::monostate, let_statement, expression> data;
};
} // namespace zinc

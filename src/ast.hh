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

struct expression {
    enum class type {
        Invalid,
        Identifier,
        Integer,
        Boolean,
        Prefix,
    } type;
    std::variant<std::monostate, identifer, uint64_t, bool, prefix_expression>
        data;
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

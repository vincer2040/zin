#pragma once

#include <cstdint>
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

struct expression {
    enum class type {
        Invalid,
        Identifier,
        Integer,
        Boolean,
    } type;
    std::variant<std::monostate, identifer, uint64_t, bool> data;
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

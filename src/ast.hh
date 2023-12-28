#pragma once

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
    } type;
    std::variant<std::monostate, identifer> data;
};

struct let_statement {
    identifer ident;
    expression e;
};

struct statement {
    enum class type {
        Invalid,
        Let,
    } type;
    std::variant<std::monostate, let_statement> data;
};
} // namespace zinc

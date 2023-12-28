#pragma once

#include <string>
#include <variant>

namespace zinc {
    enum class tokent {
        Illegal,
        Eof,
        Assign,
        Plus,
        Minus,
        Asterisk,
        Slash,
        Semicolon,
        Comma,
        Colon,
        Arrow,
        LParen,
        RParen,
        LSquirly,
        RSquirly,
        Bang,
        Lt,
        Gt,
        Eq,
        NotEq,
        Let,
        Function,
        u8,
        u16,
        u32,
        u64,
        i8,
        i16,
        i32,
        i64,
        Bool,
        True,
        False,
        Return,
        Ident,
        Int,
    };

    struct token {
        tokent type;
        std::variant<std::monostate, std::string> literal;
        token();

        const char* type_to_string();
    };

    token lookup_ident(std::string ident);

    const char* token_type_string(tokent type);
}

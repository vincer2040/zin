#include "token.hh"

struct lookup {
    std::string ident;
    zinc::tokent type;
};

lookup lookups[] = {
    {"fn", zinc::tokent::Function}, {"u8", zinc::tokent::u8},
    {"i8", zinc::tokent::i8},       {"u16", zinc::tokent::u16},
    {"i16", zinc::tokent::i16},     {"u32", zinc::tokent::u32},
    {"i32", zinc::tokent::i32},     {"u64", zinc::tokent::u64},
    {"i64", zinc::tokent::i64},     {"let", zinc::tokent::Let},
    {"true", zinc::tokent::True},   {"false", zinc::tokent::False},
    {"match", zinc::tokent::Match}, {"return", zinc::tokent::Return},
};

size_t lookups_len = sizeof lookups / sizeof lookups[0];

zinc::token::token() : type(zinc::tokent::Illegal), literal(std::monostate()) {}

zinc::token zinc::lookup_ident(std::string ident) {
    zinc::token tok;
    size_t i;
    for (i = 0; i < lookups_len; ++i) {
        lookup l = lookups[i];
        if (l.ident == ident) {
            tok.type = l.type;
            return tok;
        }
    }
    tok.type = zinc::tokent::Ident;
    tok.literal = std::move(ident);
    return tok;
}

const char* zinc::token::type_to_string() {
    return zinc::token_type_string(type);
}

const char* zinc::token_type_string(tokent type) {
    switch (type) {
    case tokent::Illegal:
        return "Illegal";
    case tokent::Eof:
        return "Eof";
    case tokent::Assign:
        return "Assign";
    case tokent::Plus:
        return "Plus";
    case tokent::Minus:
        return "Minus";
    case tokent::Asterisk:
        return "Asterisk";
    case tokent::Slash:
        return "Slash";
    case tokent::Semicolon:
        return "Semicolon";
    case tokent::Comma:
        return "Comma";
    case tokent::Colon:
        return "Colon";
    case tokent::Arrow:
        return "Arrow";
    case tokent::LParen:
        return "LParen";
    case tokent::RParen:
        return "RParen";
    case tokent::LSquirly:
        return "LSquirly";
    case tokent::RSquirly:
        return "RSquirly";
    case tokent::Bang:
        return "Bang";
    case tokent::Lt:
        return "Lt";
    case tokent::Gt:
        return "Gt";
    case tokent::Eq:
        return "Eq";
    case tokent::NotEq:
        return "NotEq";
    case tokent::Let:
        return "Let";
    case tokent::Function:
        return "Function";
    case tokent::u8:
        return "u8";
    case tokent::u16:
        return "u16";
    case tokent::u32:
        return "u32";
    case tokent::u64:
        return "u64";
    case tokent::i8:
        return "i8";
    case tokent::i16:
        return "i16";
    case tokent::i32:
        return "i32";
    case tokent::i64:
        return "i64";
    case tokent::Bool:
        return "Bool";
    case tokent::Return:
        return "Return";
    case tokent::Ident:
        return "Ident";
    case tokent::Int:
        return "Int";
    case tokent::True:
        return "True";
    case tokent::False:
        return "False";
    case tokent::Match:
        return "Match";
    case tokent::Underscore:
        return "Underscore";
    }
    return nullptr;
}

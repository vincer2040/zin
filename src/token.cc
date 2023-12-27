#include "token.hh"

struct lookup {
    std::string ident;
    zinc::tokent type;
};

lookup lookups[] = {
    {"fn", zinc::tokent::Function},   {"u8", zinc::tokent::u8},
    {"i8", zinc::tokent::i8},         {"u16", zinc::tokent::u16},
    {"i16", zinc::tokent::i16},       {"u32", zinc::tokent::u32},
    {"i32", zinc::tokent::i32},       {"u64", zinc::tokent::u64},
    {"i64", zinc::tokent::i64},       {"let", zinc::tokent::Let},
    {"return", zinc::tokent::Return},
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

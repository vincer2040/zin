#include "../src/lexer.hh"
#include <gtest/gtest.h>

#define arr_size(arr) sizeof arr / sizeof arr[0]

struct lexer_test {
    zinc::tokent exp_type;
    const char* exp_literal;
};

TEST(Lexer, NextToken) {
    std::string input = "\
let five = 5;\n\
let ten = 10;\n\
fn add(x: i32, y: i32) -> i32 {\n\
    return x + y;\n\
}\n\
let result = add(five, ten);\n\
";
    zinc::lexer l(std::move(input));
    lexer_test tests[] = {
        {zinc::tokent::Let, nullptr},       {zinc::tokent::Ident, "five"},
        {zinc::tokent::Assign, nullptr},    {zinc::tokent::Int, "5"},
        {zinc::tokent::Semicolon, nullptr}, {zinc::tokent::Let, nullptr},
        {zinc::tokent::Ident, "ten"},       {zinc::tokent::Assign, nullptr},
        {zinc::tokent::Int, "10"},          {zinc::tokent::Semicolon, nullptr},
        {zinc::tokent::Function, nullptr},  {zinc::tokent::Ident, "add"},
        {zinc::tokent::LParen, nullptr},    {zinc::tokent::Ident, "x"},
        {zinc::tokent::Colon, nullptr},     {zinc::tokent::i32, nullptr},
        {zinc::tokent::Comma, nullptr},     {zinc::tokent::Ident, "y"},
        {zinc::tokent::Colon, nullptr},     {zinc::tokent::i32, nullptr},
        {zinc::tokent::RParen, nullptr},    {zinc::tokent::Arrow, nullptr},
        {zinc::tokent::i32, nullptr},       {zinc::tokent::LSquirly, nullptr},
        {zinc::tokent::Return, nullptr},    {zinc::tokent::Ident, "x"},
        {zinc::tokent::Plus, nullptr},      {zinc::tokent::Ident, "y"},
        {zinc::tokent::Semicolon, nullptr}, {zinc::tokent::RSquirly, nullptr},
        {zinc::tokent::Let, nullptr},       {zinc::tokent::Ident, "result"},
        {zinc::tokent::Assign, nullptr},    {zinc::tokent::Ident, "add"},
        {zinc::tokent::LParen, nullptr},    {zinc::tokent::Ident, "five"},
        {zinc::tokent::Comma, nullptr},     {zinc::tokent::Ident, "ten"},
        {zinc::tokent::RParen, nullptr},    {zinc::tokent::Semicolon, nullptr},
        {zinc::tokent::Eof, nullptr},
    };
    size_t i, len = arr_size(tests);
    for (i = 0; i < len; ++i) {
        lexer_test t = tests[i];
        zinc::token tok = l.next_token();
        EXPECT_EQ(t.exp_type, tok.type);
        if (t.exp_literal != nullptr) {
            auto literal = std::get<std::string>(tok.literal);
            EXPECT_STREQ(t.exp_literal, literal.c_str());
        }
    }
}

TEST(Lexer, OtherTokens) {
    std::string input = "\
!*/\n\
!=\n\
<>\n\
==\
";
    zinc::lexer l(input);
    lexer_test tests[] = {
        {zinc::tokent::Bang, nullptr},  {zinc::tokent::Asterisk, nullptr},
        {zinc::tokent::Slash, nullptr}, {zinc::tokent::NotEq, nullptr},
        {zinc::tokent::Lt, nullptr},    {zinc::tokent::Gt, nullptr},
        {zinc::tokent::Eq, nullptr},    {zinc::tokent::Eof, nullptr},
    };
    size_t i, len = arr_size(tests);
    for (i = 0; i < len; ++i) {
        zinc::token tok = l.next_token();
        lexer_test t = tests[i];
        EXPECT_EQ(t.exp_type, tok.type);
    }
}

TEST(Lexer, Match) {
    std::string input = "\
match foo {\
    1 -> true,\
    2 -> true,\
    3 -> true,\
    _ -> false,\
}\
";
    zinc::lexer l(input);
    lexer_test tests[] = {
        {zinc::tokent::Match, nullptr},    {zinc::tokent::Ident, "foo"},
        {zinc::tokent::LSquirly, nullptr}, {zinc::tokent::Int, "1"},
        {zinc::tokent::Arrow, nullptr},    {zinc::tokent::True, nullptr},
        {zinc::tokent::Comma, nullptr},    {zinc::tokent::Int, "2"},
        {zinc::tokent::Arrow, nullptr},    {zinc::tokent::True, nullptr},
        {zinc::tokent::Comma, nullptr},    {zinc::tokent::Int, "3"},
        {zinc::tokent::Arrow, nullptr},    {zinc::tokent::True, nullptr},
        {zinc::tokent::Comma, nullptr},    {zinc::tokent::Underscore, nullptr},
        {zinc::tokent::Arrow, nullptr},    {zinc::tokent::False, nullptr},
        {zinc::tokent::Comma, nullptr},    {zinc::tokent::RSquirly, nullptr},
        {zinc::tokent::Eof, nullptr},
    };
    size_t i, len = arr_size(tests);
    for (i = 0; i < len; ++i) {
        lexer_test t = tests[i];
        zinc::token tok = l.next_token();
        EXPECT_EQ(t.exp_type, tok.type);
        if (t.exp_literal != nullptr) {
            auto literal = std::get<std::string>(tok.literal);
            EXPECT_STREQ(t.exp_literal, literal.c_str());
        }
    }
}

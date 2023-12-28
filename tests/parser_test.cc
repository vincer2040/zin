#include "../src/parser.hh"
#include <cstdint>
#include <gtest/gtest.h>

#define arr_size(arr) sizeof arr / sizeof arr[0]

struct let_test {
    const char* exp_ident;
};

struct int_test {
    std::string input;
    uint64_t exp_value;
};

struct bool_test {
    std::string input;
    bool exp_value;
};

#define test_int(exp_num, exp)                                                 \
    do {                                                                       \
        EXPECT_EQ(exp.type, zinc::expression::type::Integer);                  \
        uint64_t val = std::get<uint64_t>(exp.data);                           \
        EXPECT_EQ(exp_num, val);                                               \
    } while (0)

#define test_bool(exp_value, exp)                                              \
    do {                                                                       \
        EXPECT_EQ(exp.type, zinc::expression::type::Boolean);                  \
        bool val = std::get<bool>(exp.data);                                   \
        EXPECT_EQ(exp_value, val);                                             \
    } while (0)

TEST(Parser, Let) {
    std::string input = "\
let foo = 5;\n\
let bar = 10;\n\
let foobar = 131313;\n\
";
    let_test tests[] = {
        {"foo"},
        {"bar"},
        {"foobar"},
    };
    size_t i, len = arr_size(tests);
    zinc::lexer l(std::move(input));
    zinc::parser p(std::move(l));
    zinc::ast ast = p.parse();
    EXPECT_EQ(ast.statements.size(), 3);
    for (i = 0; i < len; ++i) {
        let_test t = tests[i];
        auto& stmt = ast.statements[i];

        EXPECT_EQ(stmt.type, zinc::statement::type::Let);
        zinc::let_statement& let = std::get<zinc::let_statement>(stmt.data);
        EXPECT_STREQ(let.ident.name.c_str(), t.exp_ident);
    }
}

TEST(Parser, Return) {
    std::string input = "return foobar;";
    zinc::lexer l(std::move(input));
    zinc::parser p(std::move(l));
    zinc::ast ast = p.parse();
    EXPECT_EQ(ast.statements.size(), 1);
    auto& stmt = ast.statements[0];
    EXPECT_EQ(stmt.type, zinc::statement::type::Return);
}

TEST(Parser, IdentExpression) {
    std::string input = "foobar;";
    zinc::lexer l(std::move(input));
    zinc::parser p(std::move(l));
    zinc::ast ast = p.parse();
    EXPECT_EQ(ast.statements.size(), 1);
    auto& stmt = ast.statements[0];
    EXPECT_EQ(stmt.type, zinc::statement::type::Expression);
    auto& exp = std::get<zinc::expression>(stmt.data);
    EXPECT_EQ(exp.type, zinc::expression::type::Identifier);
    auto& ident = std::get<zinc::identifer>(exp.data);
    EXPECT_STREQ(ident.name.c_str(), "foobar");
}

TEST(Parser, Integers) {
    int_test tests[] = {
        {"5;", 5},
        {"10;", 10},
    };
    size_t i, len = arr_size(tests);
    for (i = 0; i < len; ++i) {
        int_test t = tests[i];
        zinc::lexer l(std::move(t.input));
        zinc::parser p(std::move(l));
        zinc::ast ast = p.parse();
        EXPECT_EQ(ast.statements.size(), 1);
        auto& stmt = ast.statements[0];
        EXPECT_EQ(stmt.type, zinc::statement::type::Expression);
        auto& exp = std::get<zinc::expression>(stmt.data);
        test_int(t.exp_value, exp);
    }
}

TEST(Parser, Boolean) {
    bool_test tests[] = {
        {"true;", true},
        {"false;", false},
    };
    size_t i, len = arr_size(tests);
    for (i = 0; i < len; ++i) {
        bool_test t = tests[i];
        zinc::lexer l(std::move(t.input));
        zinc::parser p(std::move(l));
        zinc::ast ast = p.parse();
        EXPECT_EQ(ast.statements.size(), 1);
        auto& stmt = ast.statements[0];
        EXPECT_EQ(stmt.type, zinc::statement::type::Expression);
        auto& exp = std::get<zinc::expression>(stmt.data);
        test_bool(t.exp_value, exp);
    }
}

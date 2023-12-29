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

struct prefix_test {
    std::string input;
    enum class type {
        Int,
        Bool,
    } type;
    zinc::prefix_operator oper;
    std::variant<uint64_t, bool> data;
};

struct infix_test {
    std::string input;
    enum class type {
        Int,
        Bool,
    } type;
    zinc::infix_operator oper;
    std::variant<uint64_t, bool> left;
    std::variant<uint64_t, bool> right;
};

#define check_errors(p)                                                        \
    do {                                                                       \
        std::vector<std::string>& errs = p.get_errors();                       \
        if (errs.size() > 0) {                                                 \
            for (auto& e : errs) {                                             \
                std::cout << e << '\n';                                        \
            }                                                                  \
        }                                                                      \
        EXPECT_EQ(errs.size(), 0);                                             \
    } while (0)

#define test_int(exp_num, e)                                                   \
    do {                                                                       \
        EXPECT_EQ(e.type, zinc::expression::type::Integer);                    \
        uint64_t val = std::get<uint64_t>(e.data);                             \
        EXPECT_EQ(exp_num, val);                                               \
    } while (0)

#define test_bool(exp_value, e)                                                \
    do {                                                                       \
        EXPECT_EQ(e.type, zinc::expression::type::Boolean);                    \
        bool val = std::get<bool>(e.data);                                     \
        EXPECT_EQ(exp_value, val);                                             \
    } while (0)

#define test_prefix(ptest, got)                                                \
    do {                                                                       \
        EXPECT_EQ(got.type, zinc::expression::type::Prefix);                   \
        auto& prefix = std::get<zinc::prefix_expression>(got.data);            \
        EXPECT_EQ(ptest.oper, prefix.oper);                                    \
        auto& right = *prefix.right;                                           \
        switch (ptest.type) {                                                  \
        case prefix_test::type::Int: {                                         \
            uint64_t exp_val = std::get<uint64_t>(ptest.data);                 \
            test_int(exp_val, right);                                          \
        } break;                                                               \
        case prefix_test::type::Bool: {                                        \
            bool exp_val = std::get<bool>(ptest.data);                         \
            test_bool(exp_val, right);                                         \
        } break;                                                               \
        }                                                                      \
    } while (0)

#define test_infix(ptest, got)                                                 \
    do {                                                                       \
        EXPECT_EQ(got.type, zinc::expression::type::Infix);                    \
        auto& infix = std::get<zinc::infix_expression>(got.data);              \
        EXPECT_EQ(ptest.oper, infix.oper);                                     \
        auto& left = *infix.left;                                              \
        auto& right = *infix.right;                                            \
        switch (ptest.type) {                                                  \
        case infix_test::type::Int: {                                          \
            uint64_t exp_left = std::get<uint64_t>(ptest.left);                \
            uint64_t exp_right = std::get<uint64_t>(ptest.right);              \
            test_int(exp_left, left);                                          \
            test_int(exp_right, right);                                        \
        } break;                                                               \
        case infix_test::type::Bool: {                                         \
            bool exp_left = std::get<bool>(ptest.left);                        \
            bool exp_right = std::get<bool>(ptest.right);                      \
            test_bool(exp_left, left);                                         \
            test_bool(exp_right, right);                                       \
        } break;                                                               \
        }                                                                      \
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
    check_errors(p);
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
    check_errors(p);
    EXPECT_EQ(ast.statements.size(), 1);
    auto& stmt = ast.statements[0];
    EXPECT_EQ(stmt.type, zinc::statement::type::Return);
}

TEST(Parser, IdentExpression) {
    std::string input = "foobar;";
    zinc::lexer l(std::move(input));
    zinc::parser p(std::move(l));
    zinc::ast ast = p.parse();
    check_errors(p);
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
        check_errors(p);
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
        check_errors(p);
        EXPECT_EQ(ast.statements.size(), 1);
        auto& stmt = ast.statements[0];
        EXPECT_EQ(stmt.type, zinc::statement::type::Expression);
        auto& exp = std::get<zinc::expression>(stmt.data);
        test_bool(t.exp_value, exp);
    }
}

TEST(Parser, Prefix) {
    prefix_test tests[] = {
        {
            "-5;",
            prefix_test::type::Int,
            zinc::prefix_operator::Minus,
            (uint64_t)5,
        },
        {
            "!true;",
            prefix_test::type::Bool,
            zinc::prefix_operator::Bang,
            true,
        },
    };
    size_t i, len = arr_size(tests);
    for (i = 0; i < len; ++i) {
        prefix_test t = tests[i];
        zinc::lexer l(std::move(t.input));
        zinc::parser p(std::move(l));
        zinc::ast ast = p.parse();
        check_errors(p);
        EXPECT_EQ(ast.statements.size(), 1);
        auto& stmt = ast.statements[0];
        EXPECT_EQ(stmt.type, zinc::statement::type::Expression);
        auto& exp = std::get<zinc::expression>(stmt.data);
        test_prefix(t, exp);
    }
}

TEST(Parser, Infix) {
    infix_test tests[] = {
        {
            "5 + 5;",
            infix_test::type::Int,
            zinc::infix_operator::Plus,
            (uint64_t)5,
            (uint64_t)5,
        },
        {
            "5 - 5;",
            infix_test::type::Int,
            zinc::infix_operator::Minus,
            (uint64_t)5,
            (uint64_t)5,
        },
        {
            "5 * 5;",
            infix_test::type::Int,
            zinc::infix_operator::Asterisk,
            (uint64_t)5,
            (uint64_t)5,
        },
        {
            "5 / 5;",
            infix_test::type::Int,
            zinc::infix_operator::Slash,
            (uint64_t)5,
            (uint64_t)5,
        },
        {
            "5 == 5;",
            infix_test::type::Int,
            zinc::infix_operator::Eq,
            (uint64_t)5,
            (uint64_t)5,
        },
        {
            "5 != 5;",
            infix_test::type::Int,
            zinc::infix_operator::NotEq,
            (uint64_t)5,
            (uint64_t)5,
        },
        {
            "5 < 5;",
            infix_test::type::Int,
            zinc::infix_operator::Lt,
            (uint64_t)5,
            (uint64_t)5,
        },
        {
            "5 > 5;",
            infix_test::type::Int,
            zinc::infix_operator::Gt,
            (uint64_t)5,
            (uint64_t)5,
        },
    };
    size_t i, len = arr_size(tests);
    for (i = 0; i < len; ++i) {
        infix_test t = tests[i];
        zinc::lexer l(std::move(t.input));
        zinc::parser p(std::move(l));
        zinc::ast ast = p.parse();
        EXPECT_EQ(ast.statements.size(), 1);
        auto& stmt = ast.statements[0];
        EXPECT_EQ(stmt.type, zinc::statement::type::Expression);
        auto& e = std::get<zinc::expression>(stmt.data);
        test_infix(t, e);
    }
}

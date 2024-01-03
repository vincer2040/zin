#include "../src/parser.hh"
#include <cstdint>
#include <gtest/gtest.h>

#define arr_size(arr) sizeof arr / sizeof arr[0]

struct let_test {
    const char* exp_ident;
    zinc::data_type exp_type;
    uint64_t exp_val;
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
        Ident,
    } type;
    zinc::infix_operator oper;
    std::variant<uint64_t, bool, const char*> left;
    std::variant<uint64_t, bool, const char*> right;
};

struct match_branch_test {
    zinc::match_branch_expr_type expr_type;
    zinc::match_branch_result_type result_type;
};

struct function_param_test {
    const char* name;
    zinc::data_type type;
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
        case infix_test::type::Ident: {                                        \
            const char* exp_left = std::get<const char*>(ptest.left);          \
            const char* exp_right = std::get<const char*>(ptest.right);        \
            EXPECT_EQ(left.type, zinc::expression::type::Identifier);          \
            EXPECT_EQ(right.type, zinc::expression::type::Identifier);         \
            auto& left_val = std::get<zinc::identifier>(left.data);            \
            auto& right_val = std::get<zinc::identifier>(right.data);          \
            EXPECT_STREQ(exp_left, left_val.name.c_str());                     \
            EXPECT_STREQ(exp_right, right_val.name.c_str());                   \
        } break;                                                               \
        }                                                                      \
    } while (0)

TEST(Parser, Let) {
    std::string input = "\
let foo = 5;\n\
let bar = 10;\n\
let foobar: i32 = 131313;\n\
";
    let_test tests[] = {
        {"foo", zinc::data_type::Infer, 5},
        {"bar", zinc::data_type::Infer, 10},
        {"foobar", zinc::data_type::i32, 131313},
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
        EXPECT_EQ(let.ident.type, t.exp_type);
        auto& e = let.e;
        test_int(t.exp_val, e);
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
    auto& e = std::get<zinc::expression>(stmt.data);
    EXPECT_EQ(e.type, zinc::expression::type::Identifier);
    auto& ident = std::get<zinc::identifier>(e.data);
    EXPECT_STREQ("foobar", ident.name.c_str());
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
    auto& ident = std::get<zinc::identifier>(exp.data);
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

TEST(Parser, GroupedExpression) {
    infix_test t = {
        "(5 + 5)",   infix_test::type::Int, zinc::infix_operator::Plus,
        (uint64_t)5, (uint64_t)5,
    };
    zinc::lexer l(std::move(t.input));
    zinc::parser p(std::move(l));
    zinc::ast ast = p.parse();
    check_errors(p);
    EXPECT_EQ(ast.statements.size(), 1);
    auto& stmt = ast.statements[0];
    EXPECT_EQ(stmt.type, zinc::statement::type::Expression);
    auto& e = std::get<zinc::expression>(stmt.data);
    test_infix(t, e);
}

TEST(Parser, Match) {
    std::string input = "\
match foo {\
    1 -> true,\
    2 -> true,\
    3 -> true,\
    _ -> {\
        foo == 4\
    }\
}";

    match_branch_test branch_tests[] = {
        {
            zinc::match_branch_expr_type::Expression,
            zinc::match_branch_result_type::Expression,
        },
        {
            zinc::match_branch_expr_type::Expression,
            zinc::match_branch_result_type::Expression,
        },
        {
            zinc::match_branch_expr_type::Expression,
            zinc::match_branch_result_type::Expression,
        },
        {
            zinc::match_branch_expr_type::Wildcard,
            zinc::match_branch_result_type::Block,
        },
    };

    size_t i, branch_tests_len = arr_size(branch_tests);

    zinc::lexer l(std::move(input));
    zinc::parser p(std::move(l));
    zinc::ast ast = p.parse();
    check_errors(p);
    EXPECT_EQ(ast.statements.size(), 1);
    auto& stmt = ast.statements[0];
    EXPECT_EQ(stmt.type, zinc::statement::type::Expression);
    auto& e = std::get<zinc::expression>(stmt.data);
    EXPECT_EQ(e.type, zinc::expression::type::Match);

    auto& match = std::get<zinc::match_expression>(e.data);
    EXPECT_EQ(match.expr->type, zinc::expression::type::Identifier);
    auto& expr_ident = std::get<zinc::identifier>(match.expr->data);
    EXPECT_STREQ("foo", expr_ident.name.c_str());

    EXPECT_EQ(match.branches.size(), 4);

    for (i = 0; i < branch_tests_len; ++i) {
        match_branch_test t = branch_tests[i];
        zinc::match_branch& branch = match.branches[i];
        EXPECT_EQ(t.expr_type, branch.expr_type);
        EXPECT_EQ(t.result_type, branch.result_type);
    }
}

TEST(Parser, Functions) {
    std::string input = "fn add(x: i32, y: i32) -> i32 { x + y }";
    function_param_test params_tests[] = {
        {"x", zinc::data_type::i32},
        {"y", zinc::data_type::i32},
    };
    infix_test body_test = {
        "", infix_test::type::Ident, zinc::infix_operator::Plus, "x", "y",
    };
    size_t i, params_tests_len = arr_size(params_tests);
    zinc::lexer l(std::move(input));
    zinc::parser p(std::move(l));
    zinc::ast ast = p.parse();
    check_errors(p);
    EXPECT_EQ(ast.statements.size(), 1);
    auto& stmt = ast.statements[0];
    EXPECT_EQ(stmt.type, zinc::statement::type::Expression);
    auto& e = std::get<zinc::expression>(stmt.data);
    EXPECT_EQ(e.type, zinc::expression::type::Function);
    auto& fn = std::get<zinc::function>(e.data);

    EXPECT_STREQ("add", fn.name.name.c_str());
    EXPECT_EQ(zinc::data_type::i32, fn.name.type);

    EXPECT_EQ(fn.params.size(), 2);
    for (i = 0; i < params_tests_len; ++i) {
        function_param_test t = params_tests[i];
        auto& param = fn.params[i];
        EXPECT_STREQ(t.name, param.name.c_str());
        EXPECT_EQ(t.type, param.type);
    }

    EXPECT_EQ(fn.body.size(), 1);
    auto& body_stmt = fn.body[0];
    EXPECT_EQ(body_stmt.type, zinc::statement::type::Expression);
    auto& body_e = std::get<zinc::expression>(body_stmt.data);
    test_infix(body_test, body_e);
}

TEST(Parser, FunctionNoArgs) {
    std::string input = "fn res() { x }";
    zinc::lexer l(std::move(input));
    zinc::parser p(std::move(l));
    zinc::ast ast = p.parse();
    check_errors(p);
    EXPECT_EQ(ast.statements.size(), 1);
    auto& stmt = ast.statements[0];
    EXPECT_EQ(stmt.type, zinc::statement::type::Expression);
    auto& e = std::get<zinc::expression>(stmt.data);
    EXPECT_EQ(e.type, zinc::expression::type::Function);
    auto& fn = std::get<zinc::function>(e.data);
    EXPECT_EQ(fn.params.size(), 0);
    EXPECT_EQ(fn.body.size(), 1);
}

TEST(Parser, Call) {
    std::string input = "add(1, x + 2);";
    zinc::lexer l(std::move(input));
    zinc::parser p(std::move(l));
    zinc::ast ast = p.parse();
    check_errors(p);
    EXPECT_EQ(ast.statements.size(), 1);
    auto& stmt = ast.statements[0];
    EXPECT_EQ(stmt.type, zinc::statement::type::Expression);
    auto& e = std::get<zinc::expression>(stmt.data);
    EXPECT_EQ(e.type, zinc::expression::type::Call);
    auto& call = std::get<zinc::call>(e.data);
    auto& fn = call.fn;

    EXPECT_EQ(fn->type, zinc::expression::type::Identifier);
    auto& ident = std::get<zinc::identifier>(fn->data);
    EXPECT_STREQ("add", ident.name.c_str());

    EXPECT_EQ(call.args.size(), 2);
}

TEST(Parser, CallNoArgs) {
    std::string input = "add();";
    zinc::lexer l(std::move(input));
    zinc::parser p(std::move(l));
    zinc::ast ast = p.parse();
    check_errors(p);
    EXPECT_EQ(ast.statements.size(), 1);
    auto& stmt = ast.statements[0];
    EXPECT_EQ(stmt.type, zinc::statement::type::Expression);
    auto& e = std::get<zinc::expression>(stmt.data);
    EXPECT_EQ(e.type, zinc::expression::type::Call);
    auto& call = std::get<zinc::call>(e.data);
    EXPECT_EQ(call.args.size(), 0);
}

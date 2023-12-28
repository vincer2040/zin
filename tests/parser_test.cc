#include "../src/parser.hh"
#include <gtest/gtest.h>

#define arr_size(arr) sizeof arr / sizeof arr[0]

struct let_test {
    const char* exp_ident;
};

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
    zinc::lexer l(input);
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
    zinc::lexer l(input);
    zinc::parser p(l);
    zinc::ast ast = p.parse();
    EXPECT_EQ(ast.statements.size(), 1);
    auto& stmt = ast.statements[0];
    EXPECT_EQ(stmt.type, zinc::statement::type::Return);
}

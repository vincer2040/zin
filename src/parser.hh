#pragma once

#include "ast.hh"
#include "lexer.hh"
#include <string>
#include <vector>

#define UNUSED(v) ((void)v)

namespace zinc {
enum class precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
};

class parser {
  public:
    parser(lexer l);
    ast parse();
    std::vector<std::string>& get_errors();

  private:
    lexer l;
    token cur;
    token peek;
    std::vector<std::string> errors;

    statement parse_statement();
    statement parse_let();
    statement parse_return();
    statement parse_expression_statement();

    expression parse_expression(precedence prec);
    expression parse_ident_expression();
    expression parse_int_expression();
    expression parse_bool_expression(bool value);
    expression parse_prefix_expression(prefix_operator oper);
    expression parse_infix_expression(infix_operator oper, expression left);
    expression parse_function();

    block_statement parse_block();
    std::vector<identifier> parse_function_params();
    identifier parse_identifier();
    data_type parse_data_type();

    void next_token();
    bool inline cur_token_is(tokent type);
    bool inline peek_token_is(tokent type);
    bool expect_peek(tokent type);

    precedence precedence_from_tokent(tokent type);
    precedence cur_precedence();
    precedence peek_precedence();

    void peek_error(tokent exp);
};
} // namespace zinc

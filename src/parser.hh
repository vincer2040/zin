#pragma once

#include "ast.hh"
#include "lexer.hh"
#include <string>
#include <vector>

namespace zinc {
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

    void next_token();
    bool inline cur_token_is(tokent type);
    bool inline peek_token_is(tokent type);
    bool expect_peek(tokent type);

    void peek_error(tokent exp);
};
} // namespace zinc

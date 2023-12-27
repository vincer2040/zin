#pragma once

#include "token.hh"
#include <cstddef>
#include <string>

namespace zinc {

class lexer {
  public:
    lexer(std::string input);
    token next_token();

  private:
    std::string input;
    size_t pos;
    char ch;

    void read_char();
    void skip_whitespace();
    std::string read_string();
    std::string read_int();
    char peek_char();
};

} // namespace zinc

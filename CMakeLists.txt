cmake_minimum_required(VERSION 3.11)

project(zin)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall -Werror -pedantic -fstack-clash-protection \
-fstack-protector-all -fstack-protector-strong -Werror=format-security -pipe -O2")

set(CMAKE_CXX_FLAGS_RELEASE "-Wall -Werror -pedantic -fstack-clash-protection -fstack-protector-all \
-fstack-protector-strong -Werror=format-security -pipe -O2 -s -DNDEBUG")

enable_testing()
add_subdirectory(tests)

add_library(
    token
    src/token.cc
)

add_library(
    lexer
    src/lexer.cc
)

add_executable(
    zinc
    src/main.cc
)

target_link_libraries(
    lexer
    token
)

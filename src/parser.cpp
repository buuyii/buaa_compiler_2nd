#include "parser.hpp"

#include <iostream>
namespace frontend::parser {
GrammarNode Parser::parse() {
    auto gen = grammar::generator<NodeType::COMP_UNIT>();
    auto res = gen(this->lexer);
    if (!res.has_value()) {
        // TODO ERROR
        throw std::runtime_error("parse failed");
    }
    if (!this->lexer->is_end()) {
        // TODO ERROR
        throw std::runtime_error("lexer not reach end");
    }
    return std::move(res.value());
}
}
#include <iostream>
#include <map>
#include <string>
#include <fstream>
#include <vector>
#include <cctype>
#include <sstream>
#include <string_view>
#include <set>

#include "lexer.hpp"
#include "token.hpp"
#include "parser.hpp"
#include "file.hpp"
#include "ast.hpp"
#include "visitor.hpp"
#include "ir/mod.hpp"

std::ifstream src_file;
std::ofstream out_file;
std::ofstream error_file;
std::set<ErrorInfo> error_set;

int main(int, char **)
{
    std::string filePath = "testfile.txt";
    std::string outPath = "symbol.txt";
    std::string errPath = "error.txt";

    src_file.open(filePath);
    out_file.open(outPath);
    error_file.open(errPath);
    if (!src_file.is_open())
    {
        std::cerr << "Failed to open src_file: " << filePath << std::endl;
        return 1;
    }
    std::stringstream buffer;
    buffer << src_file.rdbuf();
    std::string text_content = buffer.str();
    std::string_view text(text_content);

    // std::cout << "string_view :\n" << text << std::endl; //test

    using Lexer = frontend::lexer::Lexer;
    using Token = frontend::token::Token;
    using Parser = frontend::parser::Parser;
    Lexer lexer(text);
    Parser parser(&lexer);

    // ast

    const auto ast = parser.parse();

    // visit

    using Module = ir::Module;
    using Visitor = frontend::visitor::Visitor;

    auto module = std::make_shared<Module>();
    Visitor visitor(module.get());

    const auto &comp_unit = std::get<frontend::ast::ASTNodePtr>(ast);
    visitor.visit(comp_unit);

    output_errors();
    return 0;
}

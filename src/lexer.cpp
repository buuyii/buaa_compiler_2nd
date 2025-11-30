#include "lexer.hpp"

#include "file.hpp"

#include <algorithm>
#include <cctype>
#include <optional>
#include <string>
#include <utility>

namespace frontend::token_matcher
{
MatchResult Matcher::try_match_reserved(std::string_view text)
{
    auto keywords = token_type::RESERVED_KEYWORDS();
    for (const auto &[kw, type] : keywords)
    {
        auto len = kw.size();
        if (len <= text.size() && text.substr(0, len) == kw   
            && (text[len] != '_' && !std::isalnum(text[len]))) 
        {
            return std::make_pair(len, type);
        }
    }
    return std::nullopt;
}

MatchResult Matcher::try_match_identifier(std::string_view text){
    if (text.empty() || (!std::isalpha(text[0]) && text[0] != '_'))
    {
        return std::nullopt;
    }
    size_t res_index = 1;
    while (res_index <= text.size() && (std::isalnum(text[res_index]) || text[res_index] == '_'))
    {
        res_index++;
    }
    return std::make_pair(res_index, TokenType::IDENFR);
}

MatchResult Matcher::try_match_str_const(std::string_view text){
    if (text.empty() || text[0] != '\"')
    {
        return std::nullopt;
    }
    size_t res_index = 1;
    while (res_index <= text.size() && text[res_index] != '\"')
    {
        res_index++;
    }
    return std::make_pair(res_index + 1, TokenType::STRCON);
}

MatchResult Matcher::try_match_int_const(std::string_view text){
    if (text.empty() || !std::isdigit(text[0]))
    {
        return std::nullopt;
    }
    if (text[0] == '0' && text.size() > 1 && (text[1]=='x' || text[1]=='X')) 
    {
        size_t res_index = 2;
        while (res_index < text.size() && (std::isdigit(text[res_index]) || (text[res_index]>='a' && text[res_index]<='f') || (text[res_index]>='A' && text[res_index]<='F')))
        {
            res_index++;
        }
        return std::make_pair(res_index, TokenType::INTCON);
    }
    else 
    {
        size_t res_index = 1;
        while (res_index <= text.size() && std::isdigit(text[res_index]))
        {
            res_index++;
        }
        return std::make_pair(res_index, TokenType::INTCON);
    }
}

MatchResult Matcher::try_match_delimiter(std::string_view text){  
    auto delimiters = token_type::DELIMITER_KEYWORDS();
    for (const auto &[del, type] : delimiters)
    {
        auto len = del.size();
        if (len <= text.size() && text.substr(0, len) == del) 
        {
            return std::make_pair(len, type);
        }
    }
    return std::nullopt;
}

MatchResult Matcher::try_match_illegal(std::string_view text) {
    auto illegals = token_type::ILLEGAL_KEYWORDS();
    for (const auto &[ill, type] : illegals)
    {
        auto len = ill.size();
        if (len <= text.size() && text.substr(0, len) == ill)
        {
            return std::make_pair(len, type);
        }
    }
    return std::nullopt;
}

}

namespace frontend::lexer
{
    using namespace frontend::token_matcher;
    std::optional<Token> Lexer::next_token()
    {
        //skip
        while (skip_space() || skip_comment())
        {
        }
        if (cur_loc.empty())
        {
            return std::nullopt;
        }

        if (auto result = token_matcher::match_a_token(cur_loc); result.has_value())
        {
            auto [len, type] = result.value();
            std::string content(cur_loc.substr(0, len));
            cur_loc.remove_prefix(len);
            return Token(type, content, cur_line);
        }
        // TODO ERROR
        // illegal
        if (auto result = token_matcher::Matcher::try_match_illegal(cur_loc); result.has_value())
        {
            auto [len, type] = result.value();
            std::string content(cur_loc.substr(0, len));
            cur_loc.remove_prefix(len);
            return Token(type, content, cur_line);
        }
        return std::nullopt;
    }

    std::optional<Token> Lexer::advance() {
        buf_token = cur_token;
        cur_token = next_token();
        return cur_token;
    }

    bool Lexer::skip_space() {
        if (!cur_loc.empty() && (cur_loc[0] == ' ' || cur_loc[0] == '\n' || cur_loc[0] == '\r' || cur_loc[0] == '\t')) {
            if (cur_loc[0] == '\n')
            {
                cur_line++;
            }
            cur_loc.remove_prefix(1);
            return true;
        }
        return false;
    }

    bool Lexer::skip_comment() {
        if (cur_loc.empty() || cur_loc[0] != '/') {
            return false;
        }
        if (cur_loc.substr(0, 2) == "//"){
            auto const end_line_flag = cur_loc.find('\n');
            if (end_line_flag == std::string_view::npos) {
                cur_loc.remove_prefix(cur_loc.size());
            } else {
                cur_loc.remove_prefix(end_line_flag + 1);
                cur_line++;
            }
            return true;
        }
        else if (cur_loc.substr(0, 2) == "/*") {
            const auto end_index = cur_loc.find("*/", 2);
            if (end_index == std::string_view::npos) {
                //TODO ERROR
                return false;
            }
            auto const comment = cur_loc.substr(0, end_index);
            cur_line += std::count(comment.cbegin(), comment.cend(), '\n');
            cur_loc.remove_prefix(end_index + 2);
            return true;
        }
        return false;
    }

    void Lexer::branch() {
        branches.push({cur_loc, cur_line});
        err_branches.push({buf_token, cur_token});
    }

    void Lexer::rollback() {
        if (branches.empty()) {
            return;
        }
        auto &[loc, line] = branches.top();
        cur_loc = loc;
        cur_line = line;
        auto &[ token1, token2 ] = err_branches.top();
        buf_token = token1;
        cur_token = token2;
        branches.pop();
        err_branches.pop();
    }

    void Lexer::commit() {
        if (branches.empty()) {
            //TODO ERROR ,NO BRANCH TO COMMIT
            throw std::runtime_error("no branch to commit");
            return;
        }
        branches.pop();
        err_branches.pop();
    }

    bool Lexer::is_end() {
        while (skip_space() || skip_comment())
        {
        }
        return branches.empty() && std::all_of(cur_loc.begin(), cur_loc.end(), [](unsigned char c){ return std::isspace(c); });
    }
}

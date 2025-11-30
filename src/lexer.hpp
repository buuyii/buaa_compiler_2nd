#ifndef BYC_FRONTEND_LEXER_HPP
#define BYC_FRONTEND_LEXER_HPP

#include <cstddef>
#include <functional>
#include <istream>
#include <optional>
#include <stack>
#include <string_view>
#include <utility>

#include "token.hpp"

namespace frontend::token_matcher
{
    using TokenType = token_type::TokenType;
    using MatchResult = std::optional<std::pair<size_t, TokenType>>;

    class Matcher
    { 
    public:
        static MatchResult try_match_reserved(std::string_view text);
        static MatchResult try_match_identifier(std::string_view text);
        static MatchResult try_match_int_const(std::string_view text);
        static MatchResult try_match_str_const(std::string_view text);
        static MatchResult try_match_delimiter(std::string_view text);
        static MatchResult try_match_illegal(std::string_view text);
    };

    inline const std::function<MatchResult(std::string_view)> matchers[] = {
        Matcher::try_match_reserved,
        Matcher::try_match_identifier,
        Matcher::try_match_int_const,
        Matcher::try_match_str_const,
        Matcher::try_match_delimiter,
    };

    inline MatchResult match_a_token(std::string_view text)
    {
        for (const auto &matcher : matchers)
        {
            if (auto result = matcher(text); result.has_value())
            {
                return result;
            }
        }
        return std::nullopt;
    }
} // namespace frontend::token_matcher

namespace frontend::lexer
{
    using namespace frontend::token;

    class Lexer
    {
    public:
        explicit Lexer(std::string_view text) : text(text), cur_loc(text), cur_line(1) {
            buf_token = std::optional<Token>();
            cur_token = std::optional<Token>();
        }
        std::optional<Token> next_token();
        std::optional<Token> advance();

        std::string_view get_text() const { return text; }
        size_t get_line() const { return cur_line; }

        inline size_t get_error_line() {
            if (buf_token.has_value()) {
                return buf_token->get_line();
            }
            return cur_token->get_line();
        }

        //parsing
        void branch();

        void rollback();

        void commit();

        [[nodiscard]] bool is_end();

    private:
        bool skip_comment();
        bool skip_space();

    private:
        std::string_view text;
        std::string_view cur_loc;
        size_t cur_line;
        std::optional<Token> buf_token;
        std::optional<Token> cur_token;

        std::stack<std::pair<std::string_view, size_t>> branches;
        std::stack<std::pair<std::optional<Token>, std::optional<Token>>> err_branches;
        };

} // namespace frontend::lexer
#endif // BYC_FRONTEND_LEXER_HPP
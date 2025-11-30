#ifndef BYC_TOKEN_HPP
#define BYC_TOKEN_HPP

#include <string>
#include <algorithm>
#include <unordered_map>
#include <ostream>
#include <vector>

namespace frontend::token_type
{

    enum class TokenType
    {
        IDENFR,

        INTCON,
        STRCON,

        CONSTTK,
        INTTK,
        STATICTK,
        BREAKTK,
        CONTINUETK,
        IFTK,
        ELSETK,

        NOT,
        AND,
        OR,
        FORTK,
        RETURNTK,
        VOIDTK,
        PLUS,
        MINU,
        MULT,
        DIV,
        MOD,
        LSS,
        LEQ,
        GRE,
        GEQ,
        EQL,
        NEQ,
        MAINTK,
        PRINTFTK,
        SEMICN,
        COMMA,
        LPARENT,
        RPARENT,
        LBRACK,
        RBRACK,
        LBRACE,
        RBRACE,
        ASSIGN,


    };

    
    inline std::ostream &operator<<(std::ostream &os, const TokenType &type)
    {
        const static std::unordered_map<TokenType, std::string> tokenTypeNames = {
            {TokenType::IDENFR, "IDENFR"},

            {TokenType::INTCON, "INTCON"},
            {TokenType::STRCON, "STRCON"},

            {TokenType::CONSTTK, "CONSTTK"},
            {TokenType::INTTK, "INTTK"},
            {TokenType::STATICTK, "STATICTK"},
            {TokenType::BREAKTK, "BREAKTK"},
            {TokenType::CONTINUETK, "CONTINUETK"},
            {TokenType::IFTK, "IFTK"},
            {TokenType::ELSETK, "ELSETK"},

            {TokenType::NOT, "NOT"},
            {TokenType::AND, "AND"},
            {TokenType::OR, "OR"},
            {TokenType::FORTK, "FORTK"},
            {TokenType::RETURNTK, "RETURNTK"},
            {TokenType::VOIDTK, "VOIDTK"},
            {TokenType::PLUS, "PLUS"},
            {TokenType::MINU, "MINU"},
            {TokenType::MULT, "MULT"},
            {TokenType::DIV, "DIV"},
            {TokenType::MOD, "MOD"},
            {TokenType::LSS, "LSS"},
            {TokenType::LEQ, "LEQ"},
            {TokenType::GRE, "GRE"},
            {TokenType::GEQ, "GEQ"},
            {TokenType::EQL, "EQL"},
            {TokenType::NEQ, "NEQ"},
            {TokenType::MAINTK, "MAINTK"},
            {TokenType::PRINTFTK, "PRINTFTK"},
            {TokenType::SEMICN, "SEMICN"},
            {TokenType::COMMA, "COMMA"},
            {TokenType::LPARENT, "LPARENT"},
            {TokenType::RPARENT, "RPARENT"},
            {TokenType::LBRACK, "LBRACK"},
            {TokenType::RBRACK, "RBRACK"},
            {TokenType::LBRACE, "LBRACE"},
            {TokenType::RBRACE, "RBRACE"},
            {TokenType::ASSIGN, "ASSIGN"},
        };

        if (const auto it = tokenTypeNames.find(type); it != tokenTypeNames.end())
        {
            os << it->second;
            return os;
        }
        // TODO ERROR
        os << "UNKNOWN_TOKEN_TYPE";
        return os;
    }
    constexpr static auto RESERVED_KEYWORDS = []() 
    {
        auto keywords = std::vector<std::pair<std::string, TokenType>>{
            {"const", TokenType::CONSTTK},
            {"int", TokenType::INTTK},
            {"static", TokenType::STATICTK},
            {"break", TokenType::BREAKTK},
            {"continue", TokenType::CONTINUETK},
            {"if", TokenType::IFTK},
            {"else", TokenType::ELSETK},
            {"for", TokenType::FORTK},
            {"return", TokenType::RETURNTK},
            {"void", TokenType::VOIDTK},
            {"main", TokenType::MAINTK},
            {"printf", TokenType::PRINTFTK},
        };
        std::sort(keywords.begin(), keywords.end(), [](const auto &a, const auto &b)
                  { return a.first.size() > b.first.size(); });

        return keywords;
    };
    constexpr static auto DELIMITER_KEYWORDS = []() 
    {
        auto keywords = std::vector<std::pair<std::string, TokenType>>{
            {"!", TokenType::NOT},
            {"&&", TokenType::AND},
            {"||", TokenType::OR},
            {"+", TokenType::PLUS},
            {"-", TokenType::MINU},
            {"*", TokenType::MULT},
            {"/", TokenType::DIV},
            {"%", TokenType::MOD},
            {"<", TokenType::LSS},
            {"<=", TokenType::LEQ},
            {">", TokenType::GRE},
            {">=", TokenType::GEQ},
            {"==", TokenType::EQL},
            {"!=", TokenType::NEQ},
            {"=", TokenType::ASSIGN},
            {";", TokenType::SEMICN},
            {",", TokenType::COMMA},
            {"(", TokenType::LPARENT},
            {")", TokenType::RPARENT},
            {"[", TokenType::LBRACK},
            {"]", TokenType::RBRACK},
            {"{", TokenType::LBRACE},
            {"}", TokenType::RBRACE},
        };
        std::sort(keywords.begin(), keywords.end(), [](const auto &a, const auto &b)
                  { return a.first.size() > b.first.size(); });

        return keywords;
    };

    constexpr static auto ILLEGAL_KEYWORDS = []()
    {
        auto keywords = std::vector<std::pair<std::string, TokenType>>{
            {"&", TokenType::AND},
            {"|", TokenType::OR},
        };
        return keywords;
    };
} // namespace frontend::token_type

namespace frontend::token
{
    using TokenType = token_type::TokenType;

    class Token
    {
    public:
        explicit Token(TokenType type, std::string content, size_t line)
            : type(type), content(std::move(content)), line(line) {}

        [[nodiscard]] inline bool is_type(const TokenType &t) const { return type == t; }
        [[nodiscard]] inline TokenType get_type() const { return type; }
        [[nodiscard]] inline const std::string &get_content() const { return content; }
        [[nodiscard]] inline size_t get_line() const { return line; }

        friend std::ostream &operator<<(std::ostream &os, const Token &token);
        inline bool is_illegal(){
            if (content == "&" || content == "|") {
                return true;
            }
            return false;
        }
        
    private:
        TokenType type;
        std::string content;
        size_t line;
    };

    inline std::ostream &operator<<(std::ostream &os, const Token &token)
    {
        os << token.type << " " << token.content;
        return os;
    }
}

#endif
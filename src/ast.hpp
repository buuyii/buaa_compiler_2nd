#ifndef BYC_FRONTEND_AST_HPP
#define BYC_FRONTEND_AST_HPP

#include <cstddef>
#include <memory>
#include <cassert>
#include <functional>
#include <optional>
#include <unordered_map>
#include <vector>
#include <variant>
#include <iostream>
#include <set>

#include "lexer.hpp"
#include "token.hpp"
#include "file.hpp"

namespace frontend::ast_type
{
    // clang-format off
enum class NodeType {
    COMP_UNIT, DECL, CONST_DECL, B_TYPE, CONST_DEF, CONST_INIT_VAL,
    VAR_DECL, VAR_DEF, INIT_VAL, FUNC_DEF, FUNC_TYPE,
    MAIN_FUNC_DEF, FUNC_F_PARAM, FUNC_F_PARAMS,
    BLOCK, BLOCK_ITEM, STMT, FOR_STMT,
    EXP, COND, L_VAL, PRIMARY_EXP, NUMBER, UNARY_EXP,
    UNARY_OP, FUNC_R_PARAMS, MUL_EXP, ADD_EXP, REL_EXP,
    EQ_EXP, L_AND_EXP, L_OR_EXP, CONST_EXP,
    //support
    ASSIGN_STMT, IF_STMT, S_FOR_STMT, BREAK_STMT, CONTINUE_STMT, RETURN_STMT, EXP_STMT, PRINTF_STMT,
};
    // clang-format on
    inline std::ostream &operator<<(std::ostream &os, const NodeType &type)
    {
        const static std::unordered_map<NodeType, std::string> nodeTypeNames = {
            {NodeType::COMP_UNIT, "CompUnit"},
            {NodeType::DECL, "Decl"},
            {NodeType::CONST_DECL, "ConstDecl"},
            {NodeType::B_TYPE, "BType"},
            {NodeType::CONST_DEF, "ConstDef"},
            {NodeType::CONST_INIT_VAL, "ConstInitVal"},
            {NodeType::VAR_DECL, "VarDecl"},
            {NodeType::VAR_DEF, "VarDef"},
            {NodeType::INIT_VAL, "InitVal"},
            {NodeType::FUNC_DEF, "FuncDef"},
            {NodeType::FUNC_TYPE, "FuncType"},
            {NodeType::MAIN_FUNC_DEF, "MainFuncDef"},
            {NodeType::FUNC_F_PARAM, "FuncFParam"},
            {NodeType::FUNC_F_PARAMS, "FuncFParams"},
            {NodeType::BLOCK, "Block"},
            {NodeType::BLOCK_ITEM, "BlockItem"},
            {NodeType::STMT, "Stmt"},
            {NodeType::FOR_STMT, "ForStmt"},
            {NodeType::EXP, "Exp"},
            {NodeType::COND, "Cond"},
            {NodeType::L_VAL, "LVal"},
            {NodeType::PRIMARY_EXP, "PrimaryExp"},
            {NodeType::NUMBER, "Number"},
            {NodeType::UNARY_EXP, "UnaryExp"},
            {NodeType::UNARY_OP, "UnaryOp"},
            {NodeType::FUNC_R_PARAMS, "FuncRParams"},
            {NodeType::MUL_EXP, "MulExp"},
            {NodeType::ADD_EXP, "AddExp"},
            {NodeType::REL_EXP, "RelExp"},
            {NodeType::EQ_EXP, "EqExp"},
            {NodeType::L_AND_EXP, "LAndExp"},
            {NodeType::L_OR_EXP, "LOrExp"},
            {NodeType::CONST_EXP, "ConstExp"},
            {NodeType::ASSIGN_STMT, "AssignExp"},
            {NodeType::IF_STMT, "IfStmt"},
            {NodeType::S_FOR_STMT, "SForStmt"},
            {NodeType::BREAK_STMT, "BreakStmt"},
            {NodeType::CONTINUE_STMT, "ContinueStmt"},
            {NodeType::RETURN_STMT, "ReturnStmt"},
            {NodeType::EXP_STMT, "ExpStmt"},
            {NodeType::PRINTF_STMT, "PrintfStmt"},
        };
        if (const auto it = nodeTypeNames.find(type); it != nodeTypeNames.end())
        {
            os << it->second;
            return os;
        }
        // TODO ERROR
        return os;
    }

    inline const std::set<NodeType> noPrintTypes = {
        NodeType::BLOCK_ITEM, NodeType::DECL, NodeType::B_TYPE};

    inline const std::set<NodeType> leftRecursionExps = {
        NodeType::L_OR_EXP,
        NodeType::L_AND_EXP,
        NodeType::EQ_EXP,
        NodeType::REL_EXP,
        NodeType::ADD_EXP,
        NodeType::MUL_EXP,
    };

} // namespace frontend::ast_type

namespace frontend::ast
{
    class ASTNode;
    using NodeType = ast_type::NodeType;
    using Token = token::Token;
    using ASTNodePtr = std::unique_ptr<ASTNode>;
    using TokenPtr = std::unique_ptr<Token>;
    using GrammarNode = std::variant<ASTNodePtr, TokenPtr>;
    using GrammarNodeIterator = std::vector<GrammarNode>::const_iterator;

    class ASTNode
    {
    public:
        explicit ASTNode(NodeType type) : type(type) {}
        inline NodeType get_type() const { return type; }
        inline bool is_type(const NodeType &t) const { return type == t; }
        inline void set_children(std::vector<GrammarNode> &&children)
        {
            this->children = std::move(children);
        }

        template <typename Father>
        void for_each_child(Father &&father, size_t from = 0)
        {
            assert(from < children.size());
            for (size_t i = from; i < children.size(); ++i)
            {
                std::invoke(std::forward<Father>(father), children[i]);
            }
        }

        std::vector<GrammarNode> &get_children()
        {
            return children;
        }

        void print_tree();

    private:
        NodeType type;
        std::vector<GrammarNode> children;
    };
} // namespace frontend::ast

namespace frontend::grammar
{
    using namespace ast;
    using namespace lexer;
    using GrammarNodeCollector = std::function<std::optional<std::vector<GrammarNode>>(Lexer *)>;
    using GrammarNodeGenerator = std::function<std::optional<ASTNodePtr>(Lexer *)>;

    extern const std::unordered_map<NodeType, GrammarNodeCollector> Collectors;

    enum class CollectorOperator
    {
        SEVERAL,
        OPTION
    };

    GrammarNodeCollector operator+(const GrammarNodeCollector &lhs, const GrammarNodeCollector &rhs);
    GrammarNodeCollector operator|(const GrammarNodeCollector &lhs, const GrammarNodeCollector &rhs);
    GrammarNodeCollector operator*(const GrammarNodeCollector &gen, const CollectorOperator &op);

    template <NodeType type>
    GrammarNodeGenerator generator()
    {
        return [](Lexer *lexer) -> std::optional<ASTNodePtr>
        {
            auto it = Collectors.find(type);
            if (it == Collectors.end())
            {
                // TODO ERROR
                return std::nullopt;
            }
            auto col = it->second;
            auto children = col(lexer);
            if (!children.has_value())
            {
                return std::nullopt;
            }
            auto node = std::make_unique<ASTNode>(type);
            node->set_children(std::move(children.value()));
            return std::move(node);
        };
    }

    template <TokenType type>
    GrammarNodeCollector token_collector(NodeType father_type = NodeType::COMP_UNIT)
    {
        return [father_type](Lexer *lexer) -> std::optional<std::vector<GrammarNode>>
        {
            lexer->branch();
            auto token = lexer->advance(); //
            if (token.has_value() && token->is_illegal())
            {
                error_report(token->get_line(), 'a');
                if (token->get_content() == "|" && type == TokenType::OR) {
                    lexer->commit();
                    std::vector<GrammarNode> res;
                    res.emplace_back(std::make_unique<Token>(TokenType::OR, "||", token->get_line()));
                    return res;
                } else if (token->get_content() == "&" && type == TokenType::AND) {
                    lexer->commit();
                    std::vector<GrammarNode> res;
                    res.emplace_back(std::make_unique<Token>(TokenType::AND, "&&", token->get_line()));
                    return res;
                }
                lexer->rollback();
                return std::nullopt;
            }
            if (!token.has_value() || token->get_type() != type)
            {
                // ===== CHECK MISSING TOKEN =====
                char error_code = 0;
                switch (type)
                {
                case TokenType::SEMICN: // ';'
                    if (father_type == NodeType::STMT ||
                        father_type == NodeType::CONST_DECL ||
                        father_type == NodeType::VAR_DECL)
                    {
                        error_code = 'i';
                    }
                    break;
                case TokenType::RPARENT: // ')'
                    if (father_type == NodeType::UNARY_EXP ||
                        father_type == NodeType::FUNC_DEF ||
                        father_type == NodeType::MAIN_FUNC_DEF ||
                        father_type == NodeType::STMT ||
                        father_type == NodeType::PRIMARY_EXP)
                    {
                        error_code = 'j';
                    }
                    break;
                case TokenType::RBRACK: // ']'
                    if (father_type == NodeType::CONST_DEF ||
                        father_type == NodeType::VAR_DEF ||
                        father_type == NodeType::FUNC_F_PARAM ||
                        father_type == NodeType::L_VAL)
                    {
                        error_code = 'k';
                    }
                    break;
                default:
                    break;
                }

                if (error_code != 0)
                {
                    // TODO ERROR FIX
                    size_t err_line = lexer->get_error_line();
                    error_report(err_line, error_code);
                    std::vector<GrammarNode> res;
                    switch (error_code) 
                    {
                    case 'i':
                        res.emplace_back(std::make_unique<Token>(std::move(Token(TokenType::SEMICN, ";", lexer->get_line()))));
                        break;
                    case 'j':
                        res.emplace_back(std::make_unique<Token>(std::move(Token(TokenType::RPARENT, ")", lexer->get_line()))));
                        break;
                    case 'k':
                        res.emplace_back(std::make_unique<Token>(std::move(Token(TokenType::RBRACK, "]", lexer->get_line()))));
                        break;
                    }
                    lexer->rollback();
                    return res;
                }
                lexer->rollback();
                return std::nullopt;
            }
            lexer->commit();
            std::vector<GrammarNode> res;
            res.emplace_back(std::make_unique<Token>(std::move(token.value())));
            return res;
        };
    }

    template <NodeType type>
    GrammarNodeCollector node_collector()
    {
        auto gen = generator<type>();

        return [gen](Lexer *lexer) -> std::optional<std::vector<GrammarNode>>
        {
            auto node = gen(lexer);
            if (!node.has_value())
            {
                return std::nullopt;
            }
            std::vector<GrammarNode> res;
            res.emplace_back(std::move(node.value()));
            return res;
        };
    }
} // namespace frontend::grammar

#endif
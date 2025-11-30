#include "ast.hpp"
#include "token.hpp"
#include "file.hpp"

#include <unordered_map>

namespace frontend::grammar
{
    constexpr auto OPTION = CollectorOperator::OPTION;
    constexpr auto SEVERAL = CollectorOperator::SEVERAL;

#define NODE(type) node_collector<NodeType::type>()
#define TOKEN(token) token_collector<TokenType::token>()
#define TOKEN_WITH_FATHER(token, father_type) token_collector<TokenType::token>(NodeType::father_type)

    const std::unordered_map<NodeType, GrammarNodeCollector> Collectors = {
        // ComUnit -> {Decl} {FuncDef} MainFuncDef
        {NodeType::COMP_UNIT, ((NODE(FUNC_DEF) | NODE(DECL)) * SEVERAL) +
                                  NODE(MAIN_FUNC_DEF)},

        // Decl -> ConstDecl | VarDecl
        {NodeType::DECL, NODE(CONST_DECL) | NODE(VAR_DECL)},

        // ConstDecl -> 'const' BType ConstDef {',' ConstDef} ';'
        {NodeType::CONST_DECL, TOKEN(CONSTTK) +
                                   NODE(B_TYPE) +
                                   NODE(CONST_DEF) +
                                   (TOKEN(COMMA) + NODE(CONST_DEF)) * SEVERAL +
                                   TOKEN_WITH_FATHER(SEMICN, CONST_DECL)},
        // BType -> 'int'
        {NodeType::B_TYPE, TOKEN(INTTK)},
        // ConstDef -> Ident {'[' ConstExp ']'} '=' ConstInitVal
        {NodeType::CONST_DEF, TOKEN(IDENFR) +
                                  (TOKEN(LBRACK) + NODE(CONST_EXP) + TOKEN_WITH_FATHER(RBRACK, CONST_DEF)) * SEVERAL +
                                  TOKEN(ASSIGN) +
                                  NODE(CONST_INIT_VAL)},
        // ConstInitVal -> ConstExp | '{' [ConstExp {',' ConstExp}] '}'
        {NodeType::CONST_INIT_VAL, NODE(CONST_EXP) |
                                       (TOKEN(LBRACE) +
                                        (NODE(CONST_EXP) +
                                         (TOKEN(COMMA) + NODE(CONST_EXP)) * SEVERAL) *
                                            OPTION +
                                        TOKEN(RBRACE))},
        // VarDecl -> ['static'] BType VarDef {',' VarDef} ';'
        {NodeType::VAR_DECL, (TOKEN(STATICTK) * OPTION) +
                                 NODE(B_TYPE) +
                                 NODE(VAR_DEF) +
                                 (TOKEN(COMMA) + NODE(VAR_DEF)) * SEVERAL +
                                 TOKEN_WITH_FATHER(SEMICN, VAR_DECL)},

        // VarDef -> Ident ['[' ConstExp ']'] ['=' InitVal]
        {NodeType::VAR_DEF, TOKEN(IDENFR) + (TOKEN(LBRACK) + NODE(CONST_EXP) + TOKEN_WITH_FATHER(RBRACK, VAR_DEF)) * OPTION +
                                (TOKEN(ASSIGN) + NODE(INIT_VAL)) * OPTION},
        // InitVal -> Exp | '{' [Exp { ',' Exp} ] '}'
        {NodeType::INIT_VAL, NODE(EXP) |
                                 (TOKEN(LBRACE) +
                                  (NODE(EXP) + (TOKEN(COMMA) + NODE(EXP)) * SEVERAL) * OPTION +
                                  TOKEN(RBRACE))},
        // FuncDef -> FuncType Ident '(' [FuncFParams] ')' Block
        {NodeType::FUNC_DEF, NODE(FUNC_TYPE) +
                                 TOKEN(IDENFR) +
                                 TOKEN(LPARENT) +
                                 (NODE(FUNC_F_PARAMS) * OPTION) +
                                 TOKEN_WITH_FATHER(RPARENT, FUNC_DEF) +
                                 NODE(BLOCK)},
        // MainFuncDef -> 'int' 'main' '(' ')' Block
        {NodeType::MAIN_FUNC_DEF, TOKEN(INTTK) +
                                      TOKEN(MAINTK) +
                                      TOKEN(LPARENT) +
                                      TOKEN_WITH_FATHER(RPARENT, MAIN_FUNC_DEF) +
                                      NODE(BLOCK)},
        // FuncType -> 'void' | 'int'
        {NodeType::FUNC_TYPE, TOKEN(VOIDTK) | TOKEN(INTTK)},
        // FuncFParams -> FuncFParam { ',' FuncFParam }
        {NodeType::FUNC_F_PARAMS, NODE(FUNC_F_PARAM) +
                                      (TOKEN(COMMA) + NODE(FUNC_F_PARAM)) * SEVERAL},
        // FuncFParam -> BType Ident [ '[' ']' ]
        {NodeType::FUNC_F_PARAM, NODE(B_TYPE) +
                                     TOKEN(IDENFR) +
                                     (TOKEN(LBRACK) + TOKEN_WITH_FATHER(RBRACK, FUNC_F_PARAM)) * OPTION},
        // Block -> '{' {BlockItem} '}'
        {NodeType::BLOCK, TOKEN(LBRACE) +
                              (NODE(BLOCK_ITEM) * SEVERAL) +
                              TOKEN(RBRACE)},
        // BlockItem -> Decl | Stmt
        {NodeType::BLOCK_ITEM, NODE(DECL) | NODE(STMT)},
        // Stmt -> LVal '=' Exp ';' | [Exp] ';' | Block | 'if' '(' Cond ')' Stmt ['else' Stmt] | 'for' '(' [ForStmt] ';' [Cond] ';' [ForStmt] ')' Stmt | 'break' ';' | 'continue' ';' | 'return' [Exp] ';' | 'printf' '(' StringConst {',' Exp} ')' ';'
        {NodeType::STMT, (NODE(L_VAL) + TOKEN(ASSIGN) + NODE(EXP) + TOKEN_WITH_FATHER(SEMICN, STMT)) | // ASSIGN_STMT
                             NODE(BLOCK) | //BLOCK
                             (TOKEN(IFTK) + TOKEN(LPARENT) + NODE(COND) + TOKEN_WITH_FATHER(RPARENT, STMT) + NODE(STMT) +
                              (TOKEN(ELSETK) + NODE(STMT)) * OPTION) | // IF_STMT
                             (TOKEN(FORTK) + TOKEN(LPARENT) +
                              (NODE(FOR_STMT) * OPTION) + TOKEN_WITH_FATHER(SEMICN, STMT) +
                              (NODE(COND) * OPTION) + TOKEN_WITH_FATHER(SEMICN, STMT) +
                              (NODE(FOR_STMT) * OPTION) + TOKEN_WITH_FATHER(RPARENT, STMT) +
                              NODE(STMT)) |                                                               // FOR_STMT
                             (TOKEN(BREAKTK) + TOKEN_WITH_FATHER(SEMICN, STMT)) |                         // BREAK_STMT
                             (TOKEN(CONTINUETK) + TOKEN_WITH_FATHER(SEMICN, STMT)) |                      // CONTINUE_STMT
                             (TOKEN(RETURNTK) + (NODE(EXP) * OPTION) + TOKEN_WITH_FATHER(SEMICN, STMT)) | // RETURN_STMT
                             (TOKEN(PRINTFTK) + TOKEN(LPARENT) +
                              TOKEN(STRCON) +
                              (TOKEN(COMMA) + NODE(EXP)) * SEVERAL +
                              TOKEN_WITH_FATHER(RPARENT, STMT) +
                              TOKEN_WITH_FATHER(SEMICN, STMT)) |                 //PRINTF_STMT
                             (NODE(EXP) + TOKEN_WITH_FATHER(SEMICN, STMT)) |
                             TOKEN(SEMICN)}, // 2 EXP_STMT
        // ForStmt -> LVal '=' Exp { ',' LVal '=' Exp }
        {NodeType::FOR_STMT, NODE(L_VAL) + TOKEN(ASSIGN) + NODE(EXP) +
                                 (TOKEN(COMMA) + NODE(L_VAL) + TOKEN(ASSIGN) + NODE(EXP)) * SEVERAL},
        // Exp -> AddExp
        {NodeType::EXP, NODE(ADD_EXP)},
        // Cond -> LOrExp
        {NodeType::COND, NODE(L_OR_EXP)},
        // LVal -> Ident ['[' Exp ']']
        {NodeType::L_VAL, TOKEN(IDENFR) +
                              (TOKEN(LBRACK) + NODE(EXP) + TOKEN_WITH_FATHER(RBRACK, L_VAL)) * OPTION},
        // PrimaryExp -> '(' Exp ')' | LVal | Number
        {NodeType::PRIMARY_EXP, (TOKEN(LPARENT) + NODE(EXP) + TOKEN_WITH_FATHER(RPARENT, PRIMARY_EXP)) |
                                    NODE(L_VAL) |
                                    NODE(NUMBER)},
        // Number -> IntConst
        {NodeType::NUMBER, TOKEN(INTCON)},
        // UnaryExp -> PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
        {NodeType::UNARY_EXP, (TOKEN(IDENFR) + TOKEN(LPARENT) +
                               (NODE(FUNC_R_PARAMS) * OPTION) +
                               TOKEN_WITH_FATHER(RPARENT, UNARY_EXP)) |
                                  NODE(PRIMARY_EXP) |
                                  (NODE(UNARY_OP) + NODE(UNARY_EXP))},
        // UnaryOp -> '+' | '-' | '!'
        {NodeType::UNARY_OP, TOKEN(PLUS) | TOKEN(MINU) | TOKEN(NOT)},
        // FuncRParams -> Exp {',' Exp}
        {NodeType::FUNC_R_PARAMS, NODE(EXP) +
                                      (TOKEN(COMMA) + NODE(EXP)) * SEVERAL},
        // MulExp -> UnaryExp {('*' | '/' | '%') UnaryExp}
        {NodeType::MUL_EXP, NODE(UNARY_EXP) +
                                ((TOKEN(MULT) | TOKEN(DIV) | TOKEN(MOD)) + NODE(UNARY_EXP)) * SEVERAL},
        // AddExp -> MulExp {('+' | '-') MulExp}
        {NodeType::ADD_EXP, NODE(MUL_EXP) +
                                ((TOKEN(PLUS) | TOKEN(MINU)) + NODE(MUL_EXP)) * SEVERAL},
        // RelExp -> AddExp {('<' | '>' | '<=' | '>=') AddExp}
        {NodeType::REL_EXP, NODE(ADD_EXP) +
                                ((TOKEN(LSS) | TOKEN(GRE) | TOKEN(LEQ) | TOKEN(GEQ)) + NODE(ADD_EXP)) * SEVERAL},
        // EqExp -> RelExp {('==' | '!=') RelExp}
        {NodeType::EQ_EXP, NODE(REL_EXP) +
                               ((TOKEN(EQL) | TOKEN(NEQ)) + NODE(REL_EXP)) * SEVERAL},
        // LAndExp -> EqExp {'&&' EqExp}
        {NodeType::L_AND_EXP, NODE(EQ_EXP) +
                                  (TOKEN(AND) + NODE(EQ_EXP)) * SEVERAL},
        // LOrExp -> LAndExp {'||' LAndExp}
        {NodeType::L_OR_EXP, NODE(L_AND_EXP) +
                                 (TOKEN(OR) + NODE(L_AND_EXP)) * SEVERAL},
        // ConstExp -> AddExp
        {NodeType::CONST_EXP, NODE(ADD_EXP)},
    };

    GrammarNodeCollector operator+(const GrammarNodeCollector &lhs, const GrammarNodeCollector &rhs)
    {
        return [lhs, rhs](Lexer *lexer) -> std::optional<std::vector<GrammarNode>>
        {
            auto left_res = lhs(lexer);
            if (!left_res.has_value())
            {
                return std::nullopt;
            }
            auto right_res = rhs(lexer);
            if (!right_res.has_value())
            {
                return std::nullopt;
            }

            left_res->insert(left_res->end(),
                             std::make_move_iterator(right_res->begin()),
                             std::make_move_iterator(right_res->end()));
            return left_res;
        };
    }

    GrammarNodeCollector operator|(const GrammarNodeCollector &lhs, const GrammarNodeCollector &rhs)
    {
        return [lhs, rhs](Lexer *lexer) -> std::optional<std::vector<GrammarNode>>
        {
            lexer->branch();
            if (auto res = lhs(lexer))
            {
                lexer->commit();
                return std::move(res.value());
            }
            else
            {
                lexer->rollback();
                lexer->branch();
                if (auto res = rhs(lexer))
                {
                    lexer->commit();
                    return std::move(res.value());
                }
                else
                {
                    lexer->rollback();
                    // TODO ERROR ,no match
                    return std::nullopt;
                }
            }
        };
    }

    GrammarNodeCollector operator*(const GrammarNodeCollector &gen, const CollectorOperator &op)
    {
        return [gen, op](Lexer *lexer) -> std::optional<std::vector<GrammarNode>>
        {
            switch (op)
            {
            case CollectorOperator::OPTION:
            {
                lexer->branch();
                if (auto gen_res = gen(lexer))
                {
                    lexer->commit();
                    return std::move(gen_res.value());
                }
                lexer->rollback();
                return std::vector<GrammarNode>();
            }
            case CollectorOperator::SEVERAL:
            {
                std::vector<GrammarNode> res;
                while (true)
                {
                    lexer->branch();
                    auto gen_res = gen(lexer);
                    if (!gen_res.has_value())
                    {
                        lexer->rollback();
                        break;
                    }
                    lexer->commit();
                    res.insert(res.end(),
                               std::make_move_iterator(gen_res->begin()),
                               std::make_move_iterator(gen_res->end()));
                }
                return std::move(res);
            }
            default:
                // TODO ERROR
                return std::nullopt;
            }
        };
    }
}

namespace frontend::ast
{
    void ASTNode::print_tree()
    {
        using frontend::ast_type::leftRecursionExps;
        using frontend::ast_type::noPrintTypes;
        using std::get_if;
        std::function<void(const ASTNode &)> dfs;
        std::function<void(const frontend::ast::GrammarNode &)> print_gram_node;

        print_gram_node = [&](const frontend::ast::GrammarNode &g)
        {
            if (auto child_node = get_if<ASTNodePtr>(&g))
            {
                if (*child_node)
                    dfs(**child_node);
            }
            else if (auto token_ptr = get_if<TokenPtr>(&g))
            {
                if (*token_ptr)
                {
                    if (!(*token_ptr)->is_illegal())  //TODO ERROR 
                    {
                        out_file << **token_ptr << std::endl;
                    }
                }
            }
        };

        dfs = [&](const ASTNode &node)
        {
            if (leftRecursionExps.find(node.get_type()) != leftRecursionExps.end())
            {
                const auto &ch = node.children;
                if (!ch.empty())
                {
                    print_gram_node(ch[0]);
                    out_file << "<" << node.type << ">" << std::endl;
                    for (size_t i = 1; i < ch.size();)
                    {
                        print_gram_node(ch[i++]);
                        if (i < ch.size())
                            print_gram_node(ch[i++]);
                        out_file << "<" << node.type << ">" << std::endl;
                    }
                }
                return;
            }

            for (const auto &child : node.children)
            {
                print_gram_node(child);
            }
            if (noPrintTypes.find(node.type) == noPrintTypes.end())
            {
                out_file << "<" << node.type << ">" << std::endl;
            }
        };
        dfs(*this);
    }
}
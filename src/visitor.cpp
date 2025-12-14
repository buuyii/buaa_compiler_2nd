#include "visitor.hpp"

#include <cstddef>
#include <functional>
#include <list>
#include <memory>
#include <optional>
#include <string>


#include "token.hpp"
#include "ast.hpp"
#include "ir/mod.hpp"
#include "util.hpp"
#include "file.hpp"

namespace frontend::symbol_table {
using namespace ir;
//tool 
static bool is_array_value(const std::shared_ptr<Value> &val) {
    auto ty = val->get_type();
    return ty->is_array_ty() ||
           (ty->is_pointer_ty() &&
            std::dynamic_pointer_cast<PointerType>(ty)->get_reference_type()->is_array_ty());
}

static bool is_int_scalar_value(const std::shared_ptr<Value> &val) {
    auto ty = val->get_type();
    return ty->is_integer_ty();   
}

static std::string calc_kind(const std::shared_ptr<Value> &val, SymbolType symbol_type, int scope_id) {
    //special case
    if (symbol_type == SymbolType::ARG)
    {
        auto ty = val->get_type();
        if (ty->is_pointer_ty() &&
            !std::dynamic_pointer_cast<PointerType>(ty)->get_reference_type()->is_integer_ty())
        {
            // treat param pointer-to-int (i.e. int a[]) as IntArray
            return "IntArray";
        }
        // otherwise fall through to normal handling (or return Int)
    }
    if (symbol_type == SymbolType::FUNC) {
        auto func = std::dynamic_pointer_cast<Function>(val);
        auto fty = std::dynamic_pointer_cast<FunctionType>(func->get_type());
        auto ret = fty->get_return_type();
        if (ret->is_void_ty()) {
            return "VoidFunc";
        }
        if (ret->is_integer_ty()) {
            return "IntFunc";
        }
        return "OtherFunc";
    }
    bool is_arr = is_array_value(val);

    //const
    if (symbol_type == SymbolType::CON) {
        return is_arr ? "ConstIntArray" : "ConstInt";
    }

    auto gv = std::dynamic_pointer_cast<GlobalVariable>(val);
    bool is_static = (gv != nullptr) && (scope_id > 1) || val->is_sp_static;
    if (is_static) {
        return is_arr ? "StaticIntArray" : "StaticInt";
    } else {
        return is_arr ? "IntArray" : "Int";
    }
}

void SymbolTable::enter_scope() {
    logical_scope_id = logical_scope_id + 1;
    logical_scope_stack.push_back(logical_scope_id);
    scopes.emplace_back(std::make_unique<std::unordered_map<std::string, SymbolContent>>());
}

void SymbolTable::exit_scope() {
    if (scopes.size() <= 1) {
        //TODO ERROR
        throw std::runtime_error("[SymbolTable::exit_scope] can not exit global");
    }
    logical_scope_stack.pop_back();
    scopes.pop_back();
}

std::optional<SymbolTable::SymbolContent> SymbolTable::lookup(std::string name) {
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        if (auto found = (*it)->find(name); found != (*it)->end()) {
            //find
            return found->second;
        }
    }
    //not find
    return std::nullopt;
}

void SymbolTable::insert(std::string name, SymbolType type, const std::shared_ptr<ir::Value> &val, size_t line) const {
    if (is_defined_in_cur_scope(name)) {
        error_report(line, 'b');
        return;
    }
    auto p = std::make_pair(val, type);
    scopes.back()->emplace(name, p);
    // append
    auto *self = const_cast<SymbolTable *>(this);
    int sid = self->current_logical_scope();
    std::string kind = calc_kind(val, type, sid);
    self->records.push_back(SymbolRecord{sid, name, kind});
}

void SymbolTable::remove(const std::string &name) const {
    scopes.back()->erase(name);
}
}

namespace frontend::visitor {
const std::string LOCAL_VAR_PREFIX = "%var_";
const std::string BASIC_BLOCK_PREFIX = "block_";
const std::string FUNCTION_PREFIX = "@";
const std::string GLOBAL_VAR_PREFIX = "@global_var_";

}

namespace frontend::visitor {

using namespace ir;


using TokenType = token_type::TokenType;

std::function<std::shared_ptr<Instruction>(
    const std::shared_ptr<BasicBlock> &, const std::shared_ptr<Value> &, const std::shared_ptr<Value> &, std::string)>
get_binary_create_func(TokenType op, const std::shared_ptr<Type> &type) {
    if (type == IntegerType::get(32)) {
        switch (op) {
            case TokenType::PLUS:
                return Add::create;
            case TokenType::MINU:
                return Sub::create;
            case TokenType::MULT:
                return Mul::create;
            case TokenType::DIV:
                return SDiv::create;
            case TokenType::MOD:
                return SRem::create;
            default:
                throw std::runtime_error("unsupported binary op");
        }
    }
    throw std::runtime_error("unsupported binary op type");
}
template <>
VisitExpsResult Visitor::visit_exps<NodeType::L_VAL>(ASTNodePtr &lval) {
    // LVal -> Ident ['[' Exp ']']     single
    auto look_res = symbol_table.lookup(
    std::get<TokenPtr>(lval->get_children().front())->get_content());
    bool in_left_context = !is_left.empty() && is_left.top();

    if (!look_res.has_value()) {
        error_report(std::get<TokenPtr>(lval->get_children().front())->get_line(), 'c');
        if (in_left_context) {
            return {nullptr, {}};
        }
        auto zero = std::make_shared<ConstantInt>(IntegerType::get(32), 0);
        return {zero, {}};
    }
    auto [symbol, symbol_type] = look_res.value();
    // error check
    if (is_left.top() && symbol_type == SymbolType::CON)
    {
        error_report(std::get<TokenPtr>(lval->get_children().front())->get_line(), 'h');
        return {nullptr, {}};
    }
    auto type = std::dynamic_pointer_cast<PointerType>(symbol->get_type());
    //determine if lval is an array
    if ((type != nullptr && type->get_reference_type()->is_array_ty()) ||
        (lval->get_children().size() > 1)) {
        std::list<std::shared_ptr<Instruction>> ins_list;
        std::vector<std::shared_ptr<Value>> indexes;
        if (type->get_reference_type()->is_pointer_ty())
        {
            // TODO inner type is pointer, load first
            auto load_ptr = Load::create(cur_basic_block, symbol, gen_local_var_name());
            ins_list.push_back(load_ptr);
            symbol = load_ptr;
        }
        else
        {
            // otherwise
            indexes.push_back(
                std::make_shared<ConstantInt>(IntegerType::get(32), 0));
        }

        lval->for_each_child([this, &ins_list, &indexes](auto &&child) {
            if (std::holds_alternative<ASTNodePtr>(child)) {
                auto [index, new_ins] = visit_exps<NodeType::EXP>(std::get<ASTNodePtr>(child));
                ins_list.splice(ins_list.end(), new_ins);
                indexes.push_back(index);
            }
        });
        if (in_array_arg) {
            indexes.push_back(std::make_shared<ConstantInt>(IntegerType::get(32), 0));
        }
        //TODO gep
        auto gep = Getelementptr::create(cur_basic_block, symbol, indexes, gen_local_var_name());
        ins_list.push_back(gep);
        if ((!is_left.empty() && is_left.top()) || in_array_arg)
        {
            return {gep, ins_list};
        }

        auto load = Load::create(cur_basic_block, gep, gen_local_var_name());
        ins_list.push_back(load);
        return {load, ins_list};
    } 
    // visit non array LVal
    if ((!is_left.empty() && is_left.top()) || symbol_type == SymbolType::CON) {
        return {symbol, {}};
    }
    // TODO if is neccessary to judge `Argument` specifically

    auto load = Load::create(cur_basic_block, symbol, gen_local_var_name());
    return {load, {load}};
}

template <>
VisitExpsResult Visitor::visit_exps<NodeType::EXP>(ASTNodePtr &exp);

template <>
VisitExpsResult Visitor::visit_exps<NodeType::ADD_EXP>(ASTNodePtr &exp);

template <>
VisitExpsResult Visitor::visit_exps<NodeType::REL_EXP>(ASTNodePtr &rel_exp) {
    // RelExp -> AddExp {('<' | '>' | '<=' | '>=') AddExp}
    auto result = visit_exps<NodeType::ADD_EXP>(std::get<ASTNodePtr>(rel_exp->get_children().front()));
    auto ret_val = result.first;
    auto ret_ins_list = result.second;
    if (rel_exp->get_children().size() == 1)
        return {ret_val, ret_ins_list};
    auto op = std::get<TokenPtr>(rel_exp->get_children()[1])->get_type();
    // clang-format off
        rel_exp->for_each_child([this, &ret_val, &ret_ins_list, &op](auto &&child) {
            std::visit(overloaded{
                    [&op](const TokenPtr &token) {
                        op = token->get_type();
                    },
                    [this, &ret_val, &ret_ins_list, &op](ASTNodePtr &node) {
                        auto [rhs_val, rhs_ins_list] = visit_exps<NodeType::ADD_EXP>(node);
                        ret_ins_list.splice(ret_ins_list.end(), rhs_ins_list);
                        auto [left, right, ins] = make_binary_type_conversion(ret_val, rhs_val);
                        ret_ins_list.splice(ret_ins_list.end(), ins);
                        ret_val = get_icmp_create_func(op, left, right);
                        ret_ins_list.push_back(std::dynamic_pointer_cast<Instruction>(ret_val));
                    }
            }, child);
        }, 2);
    // clang-format on
    return {ret_val, ret_ins_list};
}

template <>
VisitExpsResult Visitor::visit_exps<NodeType::EQ_EXP>(ASTNodePtr &eq_exp) {
    // EqExp -> RelExp {('==' | '!=') RelExp}
    auto result = visit_exps<NodeType::REL_EXP>(std::get<ASTNodePtr>(eq_exp->get_children().front()));
    auto ret_val = result.first;
    auto ret_ins_list = result.second;
    if (eq_exp->get_children().size() == 1)
        return {ret_val, ret_ins_list};
    auto op = std::get<TokenPtr>(eq_exp->get_children()[1])->get_type();
    // clang-format off
        eq_exp->for_each_child([this, &ret_val, &ret_ins_list, &op](auto &&child) {
            std::visit(overloaded{
                    [&op](const TokenPtr &token) {
                        op = token->get_type();
                    },
                    [this, &ret_val, &ret_ins_list, &op](ASTNodePtr &node) {
                        auto [rhs_val, rhs_ins_list] = visit_exps<NodeType::REL_EXP>(node);
                        ret_ins_list.splice(ret_ins_list.end(), rhs_ins_list);
                        auto [left, right, ins] = make_binary_type_conversion(ret_val, rhs_val);
                        ret_ins_list.splice(ret_ins_list.end(), ins);
                        ret_val = get_icmp_create_func(op, left, right);
                        ret_ins_list.push_back(std::dynamic_pointer_cast<Instruction>(ret_val));
                    }
            }, child);
        }, 2);
    // clang-format on
    return {ret_val, ret_ins_list};
}

template <>
VisitExpsResult Visitor::visit_exps<NodeType::PRIMARY_EXP>(ASTNodePtr &primary_exp)
{
    // PrimaryExp -> '(' Exp ')' | LVal | Number
    if (std::holds_alternative<TokenPtr>(primary_exp->get_children().front()))
    {
        // '(' Exp ')'
        return visit_exps<NodeType::EXP>(std::get<ASTNodePtr>(primary_exp->get_children()[1]));
    }
    auto &child = std::get<ASTNodePtr>(primary_exp->get_children().front());
    if (child->get_type() == NodeType::NUMBER) {
        // Number
        auto &token = std::get<TokenPtr>(child->get_children().front());
        if (token->is_type(token_type::TokenType::INTCON)) {
            return {std::make_shared<ConstantInt>(IntegerType::get(32), std::stoi(token->get_content(), nullptr, 0)),
                    {}};
        } else {
            //TODO FURTHER
            return {std::make_shared<ConstantInt>(IntegerType::get(32), std::stoi(token->get_content(), nullptr, 0)),
                    {}};
        }
    }
    //L_VAL
    is_left.push(false);
    auto res = visit_exps<NodeType::L_VAL>(child);
    is_left.pop();
    return res;
}

template <>
VisitExpsResult Visitor::visit_exps<NodeType::UNARY_EXP>(ASTNodePtr &unary_exp) {
    // UnaryExp -> PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
    auto *first_node = std::get_if<ASTNodePtr>(&unary_exp->get_children().front());
    if (first_node != nullptr && (*first_node)->get_type() == NodeType::UNARY_OP) {
        auto op = std::get<TokenPtr>((*first_node)->get_children().front())->get_type();
        auto [val, ins_list] = visit_exps<NodeType::UNARY_EXP>(std::get<ASTNodePtr>(unary_exp->get_children()[1]));
        if (op == token_type::TokenType::PLUS) {
            return {val, ins_list};
        } else if (op == token_type::TokenType::MINU) {
            // minu by zero
            if (val->get_type()->id == Type::TypeID::INTEGER_ID) {
                // i1 --> i32
                auto [converted_val, convert_inst] = make_collect_type_conversion(val, IntegerType::get(32));
                ins_list.splice(ins_list.end(), convert_inst);

                // 0 - converted_val
                auto left_zero = std::make_shared<ConstantInt>(IntegerType::get(32), 0);
                auto new_val = Sub::create(cur_basic_block, left_zero, converted_val, gen_local_var_name());
                ins_list.push_back(new_val);
                return {new_val, ins_list};
            } else {
                throw std::runtime_error("[Visitor::visit_exps<NodeType::UNARY_EXP>] Unexpected type for MINU operation");
            }
        } else if (op == token_type::TokenType::NOT) {
            // compare with zero
            if (val->get_type()->id == Type::TypeID::INTEGER_ID) {
                // i1 --> i32
                auto [converted_val, convert_inst] = make_collect_type_conversion(val, IntegerType::get(32));
                ins_list.splice(ins_list.end(), convert_inst);

                // 0 == converted_val
                auto left_zero = std::make_shared<ConstantInt>(IntegerType::get(32), 0);
                auto new_val =
                    ICmp::create(cur_basic_block, ICmp::ICmpType::EQ, left_zero, converted_val, gen_local_var_name());
                ins_list.push_back(new_val);
                return {new_val, ins_list};
            } else {
                throw std::runtime_error("[Visitor::visit_exps<NodeType::UNARY_EXP>] Unexpected type for NOT operation");
            }
        } else {
            throw std::runtime_error("[Visitor::visit_exps<NodeType::UNARY_EXP>] Unexpected operator type");
        }
    }
    if (std::holds_alternative<ASTNodePtr>(unary_exp->get_children().front())) {
        // PrimaryExp
        return visit_exps<NodeType::PRIMARY_EXP>(std::get<ASTNodePtr>(unary_exp->get_children().front()));
    }
    // call function
    auto &ident = std::get<TokenPtr>(unary_exp->get_children().front());
    auto ident_name = ident->get_content();
    //error check
    if (!symbol_table.lookup(ident_name).has_value()) {
        error_report(ident->get_line(), 'c');
        auto zero = std::make_shared<ConstantInt>(IntegerType::get(32), 0);
        return {zero, {}};
    }
    auto call_func = std::dynamic_pointer_cast<Function>(symbol_table.lookup(ident_name)->first);
    auto func_type = std::dynamic_pointer_cast<FunctionType>(call_func->get_type());
    auto param_types = func_type->get_param_types();
    size_t formal_cnt = param_types.size();
    std::vector<std::shared_ptr<Value>> args;
    std::list<std::shared_ptr<Instruction>> ins_list;
    size_t actual_cnt = 0;
    if (unary_exp->get_children().size() > 3)
    {
        auto &func_r_param = std::get<ASTNodePtr>(unary_exp->get_children()[2]);
        // clang-format off
            func_r_param->for_each_child([this, &param_types, formal_cnt,
                                      &args, &ins_list, &actual_cnt, &ident](auto &&child) {
                if (std::holds_alternative<ASTNodePtr>(child)) {
                    auto &exp_node = std::get<ASTNodePtr>(child);
                    ++actual_cnt;
                    // make inner error fixable
                    if (actual_cnt > formal_cnt) {
                        auto [tmp, extra_ins] = visit_exps<NodeType::EXP>(exp_node);
                        ins_list.splice(ins_list.end(), extra_ins);
                        return;
                    }

                    auto expected_ty = param_types[actual_cnt - 1];
                    bool expect_ptr = expected_ty->id == Type::TypeID::POINTER_ID;
                    in_array_arg = expect_ptr;
                    auto [arg, new_ins] = visit_exps<NodeType::EXP>(exp_node);

                    in_array_arg = false;
                    ins_list.splice(ins_list.end(), new_ins);

                    if (expect_ptr) {
                        if (arg->get_type()->id != Type::TypeID::POINTER_ID &&
                            arg->get_type()->id != Type::TypeID::ARRAY_ID) {
                            error_report(ident->get_line(), 'e');
                        }
                        args.push_back(arg);
                    } else {
                        //
                        if (arg->get_type()->id == Type::TypeID::POINTER_ID ||
                            arg->get_type()->id == Type::TypeID::ARRAY_ID) {
                            error_report(ident->get_line(), 'e');
                        }
                        auto [converrted_arg, conv_ins] = 
                            make_collect_type_conversion(arg, expected_ty);
                        ins_list.splice(ins_list.end(), conv_ins);
                        args.push_back(converrted_arg);
                    }
                }
            });
        // clang-format on
    }
    //error check
    if (actual_cnt != formal_cnt) {
        error_report(ident->get_line(), 'd');
        auto zero = std::make_shared<ConstantInt>(IntegerType::get(32), 0);
        return {zero, ins_list};
    }
    auto call = Call::create(cur_basic_block, call_func, args, gen_local_var_name());
    ins_list.push_back(call);
    return {call, ins_list};
}

template <>
VisitExpsResult Visitor::visit_exps<NodeType::MUL_EXP>(ASTNodePtr &mul_exp) {
    // MulExp -> UnaryExp {('*' | '/' | '%') UnaryExp}
    auto result = visit_exps<NodeType::UNARY_EXP>(std::get<ASTNodePtr>(mul_exp->get_children().front()));
    auto ret_val = result.first;
    auto ret_ins_list = result.second;
    if (mul_exp->get_children().size() == 1) return {ret_val, ret_ins_list};
    auto op = std::get<TokenPtr>(mul_exp->get_children()[1])->get_type();
    // clang-format off
        mul_exp->for_each_child([this, &ret_val, &ret_ins_list, &op](auto &&child) {
            std::visit(overloaded{
                    [&op](const TokenPtr &token) {
                        op = token->get_type();
                    },
                    [this, &ret_val, &ret_ins_list, &op](ASTNodePtr &node) {
                        auto [rhs_val, rhs_ins_list] = visit_exps<NodeType::UNARY_EXP>(node);
                        ret_ins_list.splice(ret_ins_list.end(), rhs_ins_list);
                        auto [left, right, ins] = make_binary_type_conversion(ret_val, rhs_val);
                        ret_ins_list.splice(ret_ins_list.end(), ins);
                        ret_val = get_binary_create_func(op, left->get_type())(cur_basic_block, left, right,
                                                                               gen_local_var_name());
                        ret_ins_list.push_back(std::dynamic_pointer_cast<Instruction>(ret_val));
                    }
            }, child);
        }, 2);
    // clang-format on
    return {ret_val, ret_ins_list};
}

template <>
VisitExpsResult Visitor::visit_exps<NodeType::ADD_EXP>(ASTNodePtr &add_exp) {
    // AddExp -> MulExp {('+' | '-') MulExp}
    auto result = visit_exps<NodeType::MUL_EXP>(std::get<ASTNodePtr>(add_exp->get_children().front()));
    auto ret_val = result.first;
    auto ret_ins_list = result.second;
    if (add_exp->get_children().size() == 1)
        return {ret_val, ret_ins_list};
    auto op = std::get<TokenPtr>(add_exp->get_children()[1])->get_type();
    // clang-format off
        add_exp->for_each_child([this, &ret_val, &ret_ins_list, &op](auto &&child) {
            std::visit(overloaded{
                    [&op](const TokenPtr &token) {
                        op = token->get_type();
                    },
                    [this, &ret_val, &ret_ins_list, &op](ASTNodePtr &node) {
                        auto [rhs_val, rhs_ins_list] = visit_exps<NodeType::MUL_EXP>(node);
                        ret_ins_list.splice(ret_ins_list.end(), rhs_ins_list);
                        auto [left, right, ins] = make_binary_type_conversion(ret_val, rhs_val);
                        ret_ins_list.splice(ret_ins_list.end(), ins);
                        ret_val = get_binary_create_func(op, left->get_type())(cur_basic_block, left, right,
                                                                               gen_local_var_name());
                        ret_ins_list.push_back(std::dynamic_pointer_cast<Instruction>(ret_val));
                    }
            }, child);
        }, 2);
    // clang-format on
    return {ret_val, ret_ins_list};
}

template <>
VisitExpsResult Visitor::visit_exps<NodeType::CONST_EXP>(ASTNodePtr &const_exp) {
    // ConstExp -> AddExp
    return visit_exps<NodeType::ADD_EXP>(std::get<ASTNodePtr>(const_exp->get_children().front()));
}

template <>
VisitExpsResult Visitor::visit_exps<NodeType::EXP>(ASTNodePtr &exp) {
    // Exp -> AddExp
    return visit_exps<NodeType::ADD_EXP>(std::get<ASTNodePtr>(exp->get_children().front()));
}
} // namespace frontend::visitor

namespace frontend::visitor {
    constexpr const Visitor::VisitPattern LIST = Visitor::VisitPattern::LIST;
    constexpr const Visitor::VisitPattern SINGLE = Visitor::VisitPattern::SINGLE;
    using TokenType = token_type::TokenType;

    template <>
    std::list<std::shared_ptr<Instruction>> Visitor::visit<NodeType::STMT, Instruction, LIST>(ASTNodePtr &stmt);

    template <>
    std::list<std::shared_ptr<BasicBlock>> Visitor::visit<NodeType::STMT, BasicBlock, LIST>(ASTNodePtr &stmt);

    std::shared_ptr<Constant> cal_binary_exp(
        const std::shared_ptr<Constant> &lhs,
        const std::shared_ptr<Constant> &rhs,
        TokenType op) {
            switch (op) {
                case TokenType::PLUS:
                    return *lhs + *rhs;
                case TokenType::MINU:
                    return *lhs - *rhs;
                case TokenType::MULT:
                    return *lhs * *rhs;
                case TokenType::DIV:
                    return *lhs / *rhs;
                case TokenType::MOD:
                    return *lhs % *rhs;
                default:
                    //TODO ERROR
                    throw std::runtime_error("unsupported binary op");
            }
    }
    template<>
    std::shared_ptr<Constant> Visitor::visit<NodeType::L_VAL, Constant, SINGLE>(ASTNodePtr &lval) {
        // LVal -> Ident ['[' Exp ']']
        auto look_res = symbol_table.lookup(std::get<TokenPtr>(lval->get_children().front())->get_content());
        bool in_left_context = !is_left.empty() && is_left.top();

        if (!look_res.has_value()) {
            error_report(std::get<TokenPtr>(lval->get_children().front())->get_line(), 'c');
            if (in_left_context) {
                return nullptr;
            }
            auto zero = std::make_shared<ConstantInt>(IntegerType::get(32), 0);
            return zero;
        }
        auto ident = symbol_table.lookup(std::get<TokenPtr>(lval->get_children().front())->get_content())->first;
        std::shared_ptr<Constant> arr_const = nullptr;

        if (auto glv = std::dynamic_pointer_cast<GlobalVariable>(ident)) {
            arr_const = glv->get_init_value();
        } else if (auto con = std::dynamic_pointer_cast<Constant>(ident)) {
            arr_const = con;
        } else {
            //TODO ERROR
            throw std::runtime_error("unexpected identifier type");
        }
        // check if has index
        if (lval->get_children().size() == 1)
        {
            return arr_const;
        } else {
            auto &exp = std::get<ASTNodePtr>(lval->get_children()[2]);
            auto [val, ins_list] = visit_exps<NodeType::EXP>(exp);
            int index = std::dynamic_pointer_cast<ConstantInt>(val)->get_val();
            auto array_const = std::dynamic_pointer_cast<ConstantArray>(arr_const);
            if (!array_const)
            {
                throw std::runtime_error("LVal symbol is not a constant array");
            }
            return array_const->get_vals()[index];
        }
    }

    // forward declaration
    template <>
    std::shared_ptr<Constant> Visitor::visit<NodeType::EXP, Constant, SINGLE>(ASTNodePtr &exp);

    template <>
    std::shared_ptr<Constant> Visitor::visit<NodeType::PRIMARY_EXP, Constant, SINGLE>(ASTNodePtr &primary_exp)
    {
        if (std::holds_alternative<TokenPtr>(primary_exp->get_children().front()))
        {
            // '(' EXP ')'
            return visit<NodeType::EXP, Constant, SINGLE>(std::get<ASTNodePtr>(primary_exp->get_children()[1]));
        }
        auto &child = std::get<ASTNodePtr>(primary_exp->get_children().front());
        if (child->get_type() == NodeType::NUMBER) {
            // Number
            auto &token = std::get<TokenPtr>(child->get_children().front());
            if (token->is_type(TokenType::INTCON)) {
                return std::make_shared<ConstantInt>(IntegerType::get(32), std::stoi(token->get_content(), nullptr, 0));
            } else {
                //TODO ERROR
                throw std::runtime_error("only int constant supported");
            }
        }
        // LVal
        return visit<NodeType::L_VAL, Constant, SINGLE>(child);
    }

    template <>
    std::shared_ptr<Constant> Visitor::visit<NodeType::UNARY_EXP, Constant, SINGLE>(ASTNodePtr &unary_exp) {
        // UnaryExp -> PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
        // here, function call can not be evaluated
        // PrimaryExp | UnaryOp UnaryExp
        auto *first_node = std::get_if<ASTNodePtr>(&unary_exp->get_children().front());
        if (first_node != nullptr && (*first_node)->get_type() == NodeType::UNARY_OP) {
            auto op = std::get<TokenPtr>((*first_node)->get_children().front())->get_type();
            auto rhs = visit<NodeType::UNARY_EXP, Constant, SINGLE>(std::get<ASTNodePtr>(unary_exp->get_children()[1]));
            if (op == TokenType::PLUS) {
                return rhs;
            } else if (op == TokenType::MINU) {
                return -*rhs;
            } else {
                //TODO ERROR
                throw std::runtime_error("unsupported unary op");
            }
        } else if (first_node == nullptr) {
            //Ident ..
            auto *fn = std::get_if<TokenPtr>(&unary_exp->get_children().front());
            if ((*fn)->get_type() == TokenType::IDENFR) {
                //fore-declare
                if ((*fn)->get_content() == "getint"){
                    //move intcon to here
                    return std::make_shared<ConstantInt>(IntegerType::get(32), 0);
                }
                //error_check
                auto ident_token = fn;
                const std::string ident_name = (*ident_token)->get_content();
                auto look_res = symbol_table.lookup(ident_name);
                if (!look_res.has_value()) {
                    error_report((*ident_token)->get_line(), 'c');
                }
                //TODO
            }
        }
        return visit<NodeType::PRIMARY_EXP, Constant, SINGLE>(std::get<ASTNodePtr>(unary_exp->get_children().front()));
    }

    template <>
    std::shared_ptr<Constant> Visitor::visit<NodeType::MUL_EXP, Constant, SINGLE>(ASTNodePtr &mul_exp) {
        // MulExp -> UnaryExp {('*' | '/' | '%') UnaryExp}
        auto res = visit<NodeType::UNARY_EXP, Constant, SINGLE>(std::get<ASTNodePtr>(mul_exp->get_children().front()));
        if (mul_exp->get_children().size() == 1)
            return res;
        auto op = std::get<TokenPtr>(mul_exp->get_children()[1])->get_type();
        mul_exp->for_each_child([this, &res, &op](auto &&child) {
            std::visit(overloaded{
                [&op](const TokenPtr &token) {
                    op = token->get_type();
                },
                [this, &res, &op](ASTNodePtr &node) {
                    auto rhs = visit<NodeType::UNARY_EXP, Constant, SINGLE>(node);
                    res = cal_binary_exp(res, rhs, op);
                }
            }, child);
        }, 2);
        return res;
    }

    template <>
    std::shared_ptr<Constant> Visitor::visit<NodeType::ADD_EXP, Constant, SINGLE>(ASTNodePtr &add_exp) {
        // AddExp -> MulExp {('+' | '-') MulExp}
        auto res = visit<NodeType::MUL_EXP, Constant, SINGLE>(std::get<ASTNodePtr>(add_exp->get_children().front()));
        if (add_exp->get_children().size() == 1)
            return res;
        auto op = std::get<TokenPtr>(add_exp->get_children()[1])->get_type();
        add_exp->for_each_child([this, &res, &op](auto &&child) {
            std::visit(overloaded{
                [&op](const TokenPtr &token) {
                    op = token->get_type();
                },
                [this, &res, &op](ASTNodePtr &node) {
                    auto rhs = visit<NodeType::MUL_EXP, Constant, SINGLE>(node);
                    res = cal_binary_exp(res, rhs, op);
                }
            }, child);
        }, 2);
        return res;
    }

    template <>
    std::shared_ptr<Constant> Visitor::visit<NodeType::EXP, Constant, SINGLE>(ASTNodePtr &exp) {
        // Exp -> AddExp
        if (cur_b_type == IntegerType::get(32)) {
            return visit<NodeType::ADD_EXP, Constant, SINGLE>(std::get<ASTNodePtr>(exp->get_children().front()))->cast_to_int();
        } else {
            //TODO ERROR
            throw std::runtime_error("only int type supported in constant expression");
        }
    }

    template <>
    std::shared_ptr<Constant> Visitor::visit<NodeType::CONST_EXP, Constant, SINGLE>(ASTNodePtr &const_exp) {
        // ConstExp -> AddExp
        return visit<NodeType::ADD_EXP, Constant, SINGLE>(std::get<ASTNodePtr>(const_exp->get_children().front()));
    }

    template <>
    std::list<std::shared_ptr<BasicBlock>> Visitor::visit<NodeType::BLOCK, BasicBlock, LIST>(ASTNodePtr &block) {
        // Block -> '{' {BlockItem} '}'
        symbol_table.enter_scope();



        // alloca for function args
        if (!func_arg_init) {
            for (auto &arg : cur_function->get_argument_ref()) {
                auto alloca = Alloca::create(cur_basic_block, arg->get_type(), gen_local_var_name());
                auto store = Store::create(cur_basic_block, arg, alloca);
                cur_basic_block->add_instructions({alloca, store});
                
                symbol_table.insert(arg->get_name(), SymbolType::ARG, alloca, func_init_line);
                //
                arg->set_name(gen_local_var_name());
            }
            func_arg_init = true;
            func_init_line = std::get<TokenPtr>(block->get_children().front())->get_line();
        }
        std::list<std::shared_ptr<BasicBlock>> basic_block_list;
        block->for_each_child([this, &basic_block_list](auto &&child)  {
            std::visit(overloaded{
                [this, &basic_block_list](ASTNodePtr &node) {
                    //BlockItem
                    auto &item = std::get<ASTNodePtr>(node->get_children().front());
                    //stmt
                    if (item->get_type() == NodeType::STMT) {
                        // TODO
                        auto &stmt_head = item->get_children().front();
                        // stmt_head is variant of ASTNodePtr or TokenPtr
                        bool is_token = std::holds_alternative<TokenPtr>(stmt_head);
                        bool is_astnode = std::holds_alternative<ASTNodePtr>(stmt_head);
                        if (is_astnode && std::get<ASTNodePtr>(stmt_head)->is_type(NodeType::BLOCK) || 
                            is_token && std::get<TokenPtr>(stmt_head)->is_type(TokenType::IFTK) ||
                            is_token && std::get<TokenPtr>(stmt_head)->is_type(TokenType::FORTK) ||
                            is_token && std::get<TokenPtr>(stmt_head)->is_type(TokenType::BREAKTK) ||
                            is_token && std::get<TokenPtr>(stmt_head)->is_type(TokenType::CONTINUETK) ) {
                            //block, if, for generate blocks
                            basic_block_list.splice(basic_block_list.end(), visit<NodeType::STMT, BasicBlock, LIST>(item));
                        } else {
                            cur_basic_block->add_instructions(visit<NodeType::STMT, Instruction, LIST>(item));
                        }
                    } else {
                        // Decl
                        cur_basic_block->add_instructions(visit<NodeType::DECL, Instruction, LIST>(item));
                    }
                },
                [](TokenPtr &token) {
                    if (token->get_type() != token_type::TokenType::LBRACE &&
                        token->get_type() != token_type::TokenType::RBRACE) {
                        //TODO ERROR
                    }
                }},
            child);
        });
        symbol_table.exit_scope();

        return basic_block_list;
    }

    template <>
    std::list<std::shared_ptr<Instruction>> Visitor::visit<NodeType::RETURN_STMT, Instruction, LIST>(
        ASTNodePtr &return_stmt) {
        // 'return' [Exp] ';'
        if (return_stmt->get_children().size() == 3) {
            auto [val, ins_list] = visit_exps<NodeType::EXP>(std::get<ASTNodePtr>(return_stmt->get_children()[1]));
            auto [ret_val, conv_ins] = make_collect_type_conversion(val, cur_function->get_return_type());
            ins_list.splice(ins_list.end(), conv_ins);
            auto ret = Ret::create(cur_basic_block, ret_val);
            ins_list.push_back(ret);
            return ins_list;
        }
        return {Ret::create(cur_basic_block)};
    }

    template <>
    std::list<std::shared_ptr<Instruction>> Visitor::visit<NodeType::EXP_STMT, Instruction, LIST>(ASTNodePtr &exp_stmt)
    {
        // ExpStmt -> [Exp] ';'
        if (exp_stmt->get_children().size() > 1) {
            auto [val, ins_list] = visit_exps<NodeType::EXP>(std::get<ASTNodePtr>(exp_stmt->get_children().front()));
            return ins_list;
        }
        return {};
    }

    template <>
    std::list<std::shared_ptr<Instruction>> Visitor::visit<NodeType::PRINTF_STMT, Instruction, LIST>(ASTNodePtr &printf_stmt) {
        // 'printf' '(' 'StringConst' {',' Exp} ')' ';'
        std::list<std::shared_ptr<Instruction>> ins_list;
        std::list<std::shared_ptr<Value>> val_list;
        auto &children = printf_stmt->get_children();
        // format string
        auto format_string = std::get<TokenPtr>(children[2])->get_content();
        int use = 0;
        for (int i = 4; i < children.size() - 2; i += 2)
        {
            auto [val, new_ins] = visit_exps<NodeType::EXP>(std::get<ASTNodePtr>(children[i]));
            ins_list.splice(ins_list.end(), new_ins);
            val_list.push_back(val);
            use++;
        }
        // count "%d"
        int count = 0;
        for (size_t i = 0; i < format_string.size() && count < use; i++) 
        {
            if (format_string.substr(i, 2) == "%d") {
                count++;
                i++;
            }
        }
        if (use != count) {
            error_report(std::get<TokenPtr>(children.front())->get_line(), 'l');
        }
        auto putint_rec = symbol_table.lookup("putint");
        auto putch_rec = symbol_table.lookup("putch");
        auto putint_func =
            std::dynamic_pointer_cast<Function>(putint_rec->first);
        auto putch_func =
            std::dynamic_pointer_cast<Function>(putch_rec->first);
        
        //reduce ""
        if (!format_string.empty() &&
            format_string.front() == '\"' &&
            format_string.back() == '\"')
        {
            format_string =
                format_string.substr(1, format_string.size() - 2);
        }

        auto val_it = val_list.begin();
        for (size_t i = 0; i < format_string.size(); ++i)
        {
            char c = format_string[i];

            if (c == '\\')
            {
                // deal "\"
                if (i + 1 >= format_string.size())
                    break;
                char esc = format_string[++i];
                char real_char;
                switch (esc)
                {
                case 'n':
                    real_char = '\n';
                    break;
                case 't':
                    real_char = '\t';
                    break;
                case '\\':
                    real_char = '\\';
                    break;
                case '\"':
                    real_char = '\"';
                    break;
                default:
                    // 
                    real_char = esc;
                    break;
                }

                std::vector<std::shared_ptr<Value>> args;
                args.push_back(std::make_shared<ConstantInt>(
                    IntegerType::get(32), static_cast<int>(real_char)));
                auto call_putch = Call::create(
                    cur_basic_block, putch_func, args, gen_local_var_name());
                ins_list.push_back(call_putch);
            }
            else if (c == '%' && i + 1 < format_string.size() && format_string[i + 1] == 'd')
            {
                // %d ->  int 
                i++; // skip 'd'

                if (val_it != val_list.end())
                {
                    auto val = *val_it;
                    ++val_it;

                    // convert to i32 then putint
                    auto [converted, conv_ins] =
                        make_collect_type_conversion(val,
                                                     IntegerType::get(32));
                    ins_list.splice(ins_list.end(), conv_ins);

                    std::vector<std::shared_ptr<Value>> args;
                    args.push_back(converted);
                    auto call_putint = Call::create(
                        cur_basic_block, putint_func, args, gen_local_var_name());
                    ins_list.push_back(call_putint);
                }
                else
                {
                    // 
                    continue;
                }
            }
            else
            {
                //  putch
                std::vector<std::shared_ptr<Value>> args;
                args.push_back(std::make_shared<ConstantInt>(
                    IntegerType::get(32), static_cast<int>(c)));
                auto call_putch = Call::create(
                    cur_basic_block, putch_func, args, gen_local_var_name());
                ins_list.push_back(call_putch);
            }
        }
        return ins_list;
    }

    template <>
    std::list<std::shared_ptr<BasicBlock>> Visitor::visit<NodeType::L_AND_EXP, BasicBlock, LIST>(ASTNodePtr &land_exp)
    {
        // LAndExp -> EqExp {'&&' EqExp}
        std::list<std::shared_ptr<BasicBlock>> basic_block_list;
        auto &children = land_exp->get_children();

        for (size_t i = 0; i < children.size(); i += 2)
        {
            std::shared_ptr<BasicBlock> next_basic_block;
            if (i == children.size() - 1) {
                // EqExp
                next_basic_block = true_basic_block_list.back();
            } else {
                // EqExp '&&' EqExp
                next_basic_block = std::make_shared<BasicBlock>(cur_function, gen_block_name());
                basic_block_list.push_back(next_basic_block);
            }

            // EqExp
            auto [value, ins_list] = visit_exps<NodeType::EQ_EXP>(std::get<ASTNodePtr>(children[i]));
            cur_basic_block->add_instructions(std::move(ins_list));
            auto [converted_value, convert_ins_list] = make_collect_type_conversion(value, IntegerType::get(1));
            cur_basic_block->add_instructions(std::move(convert_ins_list));

            // new Br
            cur_basic_block->add_instructions({Br::create(
                cur_basic_block, converted_value, next_basic_block, false_basic_block_list.back(), gen_local_var_name())});

            cur_basic_block = next_basic_block;
        }

        return basic_block_list;
    }

    template <>
    std::list<std::shared_ptr<BasicBlock>> Visitor::visit<NodeType::L_OR_EXP, BasicBlock, LIST>(ASTNodePtr &lor_exp)
    {
        // LOrExp -> LAndExp {'||' LAndExp}
        std::list<std::shared_ptr<BasicBlock>> basic_block_list;
        auto &children = lor_exp->get_children();

        for (size_t i = 0; i < children.size(); i += 2)
        {
            std::shared_ptr<BasicBlock> next_basic_block;
            if (i == children.size() - 1) {
                // ... LAndExp
                next_basic_block = false_basic_block_list.back();
            }
            else {
                // ... LAndExp ...
                next_basic_block = std::make_shared<BasicBlock>(cur_function, gen_block_name());
                basic_block_list.push_back(next_basic_block);
            }
            // LAndExp
            true_basic_block_list.push_back(true_basic_block_list.back());
            false_basic_block_list.push_back(next_basic_block);
            basic_block_list.splice(basic_block_list.end(),
                                    visit<NodeType::L_AND_EXP, BasicBlock, LIST>(std::get<ASTNodePtr>(children[i])));
            true_basic_block_list.pop_back();
            false_basic_block_list.pop_back();
            cur_basic_block = next_basic_block;
        }
        return basic_block_list;
    }

    template <>
    std::list<std::shared_ptr<BasicBlock>> Visitor::visit<NodeType::COND, BasicBlock, LIST>(ASTNodePtr &cond) {
        // Cond -> LOrExp
        return visit<NodeType::L_OR_EXP, BasicBlock, LIST>(std::get<ASTNodePtr>(cond->get_children().front()));
    }

    template <>
    std::list<std::shared_ptr<BasicBlock>> Visitor::visit<NodeType::IF_STMT, BasicBlock, LIST>(ASTNodePtr &if_stmt) {
        // 'if' '(' Cond ')' Stmt['else' Stmt]
        std::list<std::shared_ptr<BasicBlock>> basic_block_list;

        //

        // ir gep
        // if basic block
        std::shared_ptr<BasicBlock> if_basic_block = std::make_shared<BasicBlock>(cur_function, gen_block_name());
        basic_block_list.push_back(if_basic_block);

        // else basic block
        bool has_else = if_stmt->get_children().size() > 5;
        std::shared_ptr<BasicBlock> else_basic_block = nullptr;
        if (has_else) {
            else_basic_block = std::make_shared<BasicBlock>(cur_function, gen_block_name());
            basic_block_list.push_back(else_basic_block);
        }

        // finish basic block
        std::shared_ptr<BasicBlock> finish_basic_block = std::make_shared<BasicBlock>(cur_function, gen_block_name());
        basic_block_list.push_back(finish_basic_block);

        // Cond
        true_basic_block_list.push_back(if_basic_block);
        false_basic_block_list.push_back(has_else ? else_basic_block : finish_basic_block);
        basic_block_list.splice(basic_block_list.end(),
                                visit<NodeType::COND, BasicBlock, LIST>(std::get<ASTNodePtr>(if_stmt->get_children()[2])));
        true_basic_block_list.pop_back();
        false_basic_block_list.pop_back();
        
        // Stmt1
        cur_basic_block = if_basic_block;
        auto &stmt1 = std::get<ASTNodePtr>(if_stmt->get_children()[4]);
        auto &stmt1_head = stmt1->get_children().front();
        bool is_token = std::holds_alternative<TokenPtr>(stmt1_head);
        bool is_astnode = std::holds_alternative<ASTNodePtr>(stmt1_head);
        if (is_astnode && std::get<ASTNodePtr>(stmt1_head)->is_type(NodeType::BLOCK)  
            || is_token && std::get<TokenPtr>(stmt1_head)->is_type(TokenType::IFTK)
            || is_token && std::get<TokenPtr>(stmt1_head)->is_type(TokenType::FORTK)
            || is_token && std::get<TokenPtr>(stmt1_head)->is_type(TokenType::BREAKTK)
            || is_token && std::get<TokenPtr>(stmt1_head)->is_type(TokenType::CONTINUETK)) {
            // BLOCK, IF, FOR
            basic_block_list.splice(basic_block_list.end(), visit<NodeType::STMT, BasicBlock, LIST>(stmt1));  
        } else {
            // otherwise
            cur_basic_block->add_instructions(visit<NodeType::STMT, Instruction, LIST>(stmt1));
        }

        // add Br
        cur_basic_block->add_instructions({Br::create(cur_basic_block, finish_basic_block, gen_local_var_name())});

        if (has_else) {
            //Stmt2
            cur_basic_block = else_basic_block;
            auto &stmt2 = std::get<ASTNodePtr>(if_stmt->get_children()[6]);
            auto &stmt2_head = stmt2->get_children().front();
            bool is_token = std::holds_alternative<TokenPtr>(stmt2_head);
            bool is_astnode = std::holds_alternative<ASTNodePtr>(stmt2_head);
            if (is_astnode && std::get<ASTNodePtr>(stmt2_head)->is_type(NodeType::BLOCK)  
                || is_token && std::get<TokenPtr>(stmt2_head)->is_type(TokenType::IFTK)
                || is_token && std::get<TokenPtr>(stmt2_head)->is_type(TokenType::FORTK)
                || is_token && std::get<TokenPtr>(stmt2_head)->is_type(TokenType::BREAKTK)
                || is_token && std::get<TokenPtr>(stmt2_head)->is_type(TokenType::CONTINUETK)) {
                // BLOCK, IF, FOR
                basic_block_list.splice(basic_block_list.end(), visit<NodeType::STMT, BasicBlock, LIST>(stmt2));  
            } else {
                // otherwise
                cur_basic_block->add_instructions(visit<NodeType::STMT, Instruction, LIST>(stmt2));
            }

            // add Br
            cur_basic_block->add_instructions({Br::create(cur_basic_block, finish_basic_block, gen_local_var_name())});
        }


        //
        cur_basic_block = finish_basic_block;
        
        return basic_block_list;
    }
    template <>
    std::list<std::shared_ptr<Instruction>> Visitor::visit<NodeType::FOR_STMT, Instruction, LIST>(ASTNodePtr &for_stmt) {
        // ForStmt -> LVal '=' Exp { ',' LVal '=' Exp }
        std::list<std::shared_ptr<Instruction>> ins_list;
        auto &children = for_stmt->get_children();

        // LVAL
        for (size_t i = 0; i < children.size(); i += 4) {
            is_left.push(true);
            auto [left, left_ins] = visit_exps<NodeType::L_VAL>(std::get<ASTNodePtr>(children[i]));
            is_left.pop();
            auto [right, right_ins] = visit_exps<NodeType::EXP>(std::get<ASTNodePtr>(children[2 + i]));
            auto [actual_right, new_ins] = make_collect_type_conversion(
                right, std::dynamic_pointer_cast<PointerType>(left->get_type())->get_reference_type());
            auto store = Store::create(cur_basic_block, actual_right, left, gen_local_var_name());
            ins_list.splice(ins_list.end(), left_ins);
            ins_list.splice(ins_list.end(), right_ins);
            ins_list.splice(ins_list.end(), new_ins);
            ins_list.push_back(store);
        }
        return ins_list;
    }

    template <>
    std::list<std::shared_ptr<BasicBlock>> Visitor::visit<NodeType::S_FOR_STMT, BasicBlock, LIST>(ASTNodePtr &s_for_stmt) {
        // 'for' '(' [ForStmt] ';' [Cond] ';' [ForStmt] ')' Stmt
        std::list<std::shared_ptr<BasicBlock>> basic_block_list;

        // enter scope

        auto &children = s_for_stmt->get_children();

        auto cond_basic_block = std::make_shared<BasicBlock>(cur_function, gen_block_name());
        auto body_basic_block = std::make_shared<BasicBlock>(cur_function, gen_block_name());
        auto step_basic_block = std::make_shared<BasicBlock>(cur_function, gen_block_name());
        auto finish_basic_block = std::make_shared<BasicBlock>(cur_function, gen_block_name());

        basic_block_list.push_back(cond_basic_block);
        basic_block_list.push_back(body_basic_block);
        basic_block_list.push_back(step_basic_block);
        basic_block_list.push_back(finish_basic_block);

        // init ForStmt
        bool has_init = !std::holds_alternative<TokenPtr>(children[2]);
        if (has_init) {
            auto &for_init = std::get<ASTNodePtr>(children[2]);
            auto &specific_init = std::get<ASTNodePtr>(for_init->get_children().front());
            cur_basic_block->add_instructions(visit<NodeType::FOR_STMT, Instruction, LIST>(for_init));
        }

        cur_basic_block->add_instructions(
            {Br::create(cur_basic_block, cond_basic_block, gen_local_var_name())});

        //add info
        loop_entry_blocks.push_back(step_basic_block);
        loop_exit_blocks.push_back(finish_basic_block);

        // cond basic block
        cur_basic_block = cond_basic_block;
        bool has_cond = has_init ? std::holds_alternative<ASTNodePtr>(children[4]) && std::get<ASTNodePtr>(children[4])->is_type(NodeType::COND) : std::holds_alternative<ASTNodePtr>(children[3]) && std::get<ASTNodePtr>(children[3])->is_type(NodeType::COND);
        if (has_cond) {
            auto &cond_node = std::get<ASTNodePtr>(children[3 + has_init]);
            true_basic_block_list.push_back(body_basic_block);
            false_basic_block_list.push_back(finish_basic_block);
            basic_block_list.splice(
                basic_block_list.end(),
                visit<NodeType::COND, BasicBlock, LIST>(cond_node));
        } else {
            cur_basic_block->add_instructions(
                {Br::create(cur_basic_block, body_basic_block, gen_local_var_name())});
        }
        // body basic block
        cur_basic_block = body_basic_block;
        loop_depth++;
        auto &stmt_child = std::get<ASTNodePtr>(children.back())->get_children();
        auto &stmt_head = stmt_child.front();
        auto is_token = std::holds_alternative<TokenPtr>(stmt_head);
        auto is_astnode = std::holds_alternative<ASTNodePtr>(stmt_head);
        if (is_astnode && std::get<ASTNodePtr>(stmt_head)->is_type(NodeType::BLOCK) ||
            is_token && std::get<TokenPtr>(stmt_head)->is_type(TokenType::IFTK) ||
            is_token && std::get<TokenPtr>(stmt_head)->is_type(TokenType::FORTK) ||
            is_token && std::get<TokenPtr>(stmt_head)->is_type(TokenType::BREAKTK) ||
            is_token && std::get<TokenPtr>(stmt_head)->is_type(TokenType::CONTINUETK))
        {
            // block, if, for generate blocks
            basic_block_list.splice(basic_block_list.end(), visit<NodeType::STMT, BasicBlock, LIST>(std::get<ASTNodePtr>(children.back())));
        }
        else
        {
            cur_basic_block->add_instructions(visit<NodeType::STMT, Instruction, LIST>(std::get<ASTNodePtr>(children.back())));
        }

        cur_basic_block->add_instructions(
            {Br::create(cur_basic_block, step_basic_block, gen_local_var_name())});
        
        loop_depth--;

        // step
        cur_basic_block = step_basic_block;
        bool has_step;
        if (has_init && has_cond) {
            has_step = std::holds_alternative<ASTNodePtr>(children[6]) && std::get<ASTNodePtr>(children[6])->is_type(NodeType::FOR_STMT);
        } else if (has_init || has_cond) {
            has_step = std::holds_alternative<ASTNodePtr>(children[5]) && std::get<ASTNodePtr>(children[5])->is_type(NodeType::FOR_STMT);
        } else {
            has_step = std::holds_alternative<ASTNodePtr>(children[4]) && std::get<ASTNodePtr>(children[4])->is_type(NodeType::FOR_STMT);
        }
        if (has_step) {
            auto &for_step = std::get<ASTNodePtr>(children[4 + has_cond + has_init]);
            cur_basic_block->add_instructions(
                visit<NodeType::FOR_STMT, Instruction, LIST>(for_step));
        }
        cur_basic_block->add_instructions(
            {Br::create(cur_basic_block, cond_basic_block, gen_local_var_name())});

        //more ...
        loop_entry_blocks.pop_back();
        loop_exit_blocks.pop_back();

        cur_basic_block = finish_basic_block;

        return basic_block_list;
    }

    template <>
    std::list<std::shared_ptr<BasicBlock>> Visitor::visit<NodeType::BREAK_STMT, BasicBlock, LIST>(
        ASTNodePtr &break_stmt) {

        if (loop_depth <= 0) {
            auto &break_token = std::get<TokenPtr>(break_stmt->get_children().front());
            error_report(break_token->get_line(), 'm');
        }
        auto finish_basic_block = loop_exit_blocks.back();
        cur_basic_block->add_instructions({Br::create(cur_basic_block, finish_basic_block, gen_local_var_name())});
        return {};
    }

    template <>
    std::list<std::shared_ptr<BasicBlock>> Visitor::visit<NodeType::CONTINUE_STMT, BasicBlock, LIST>(
        ASTNodePtr &continue_stmt) {

        if (loop_depth <= 0) {
            auto &continue_token = std::get<TokenPtr>(continue_stmt->get_children().front());
            error_report(continue_token->get_line(), 'm');
        }
        auto while_basic_block = loop_entry_blocks.back();
        cur_basic_block->add_instructions({Br::create(cur_basic_block, while_basic_block, gen_local_var_name())});
        return {};
    }

    template <>
    std::list<std::shared_ptr<Instruction>> Visitor::visit<NodeType::ASSIGN_STMT, Instruction, LIST>(
        ASTNodePtr &assign_stmt) {
        // LVal '=' Exp ';'
        std::list<std::shared_ptr<Instruction>> ins_list;
        is_left.push(true);
        auto [left, left_ins] = visit_exps<NodeType::L_VAL>(std::get<ASTNodePtr>(assign_stmt->get_children().front()));
        if (left == nullptr) {
            return ins_list;
        }
        is_left.pop();
        auto [right, right_ins] = visit_exps<NodeType::EXP>(std::get<ASTNodePtr>(assign_stmt->get_children()[2]));
        //if args too many  
        auto [actual_right, new_ins] = make_collect_type_conversion(
            right, std::dynamic_pointer_cast<PointerType>(left->get_type())->get_reference_type());
        auto store = Store::create(cur_basic_block, actual_right, left, gen_local_var_name());
        ins_list.splice(ins_list.end(), left_ins);
        ins_list.splice(ins_list.end(), right_ins);
        ins_list.splice(ins_list.end(), new_ins);
        ins_list.push_back(store);
        return ins_list;
    }

    template <>
    std::list<std::shared_ptr<Instruction>> Visitor::visit<NodeType::STMT, Instruction, LIST>(ASTNodePtr &stmt) {
        auto &stmt_head = stmt->get_children().front();
        bool is_token = std::holds_alternative<TokenPtr>(stmt_head);
        bool is_astnode = std::holds_alternative<ASTNodePtr>(stmt_head);
        if (is_astnode && std::get<ASTNodePtr>(stmt_head)->is_type(NodeType::L_VAL)) {
            return visit<NodeType::ASSIGN_STMT, Instruction, LIST>(stmt);
        } else if (is_token && std::get<TokenPtr>(stmt_head)->is_type(TokenType::RETURNTK)) {
            return visit<NodeType::RETURN_STMT, Instruction, LIST>(stmt);
        } else if (is_token && std::get<TokenPtr>(stmt_head)->is_type(TokenType::SEMICN) ||
                   is_astnode && std::get<ASTNodePtr>(stmt_head)->is_type(NodeType::EXP)) {
            return visit<NodeType::EXP_STMT, Instruction, LIST>(stmt);
        } else if (is_token &&std::get<TokenPtr>(stmt_head)->is_type(TokenType::PRINTFTK)) {
            return visit<NodeType::PRINTF_STMT, Instruction, LIST>(stmt);
        } else {
            throw std::runtime_error("[Visitor::visit<STMT, Instruction, LIST>] Unexpected stmt type");
        }
    }

    template <>
    std::list<std::shared_ptr<BasicBlock>> Visitor::visit<NodeType::STMT, BasicBlock, LIST>(ASTNodePtr &stmt)
    {
        auto &stmt_head = stmt->get_children().front();
        bool is_token = std::holds_alternative<TokenPtr>(stmt_head);
        bool is_astnode = std::holds_alternative<ASTNodePtr>(stmt_head);
        if (is_astnode && std::get<ASTNodePtr>(stmt_head)->is_type(NodeType::BLOCK)){
            return visit<NodeType::BLOCK, BasicBlock, LIST>(std::get<ASTNodePtr>(stmt_head));
        } else if (is_token && std::get<TokenPtr>(stmt_head)->is_type(TokenType::IFTK)) {
            return visit<NodeType::IF_STMT, BasicBlock, LIST>(stmt);
        } else if (is_token && std::get<TokenPtr>(stmt_head)->is_type(TokenType::FORTK)) {
            return visit<NodeType::S_FOR_STMT, BasicBlock, LIST>(stmt);
        } else if (is_token && std::get<TokenPtr>(stmt_head)->is_type(TokenType::BREAKTK)) {
            return visit<NodeType::BREAK_STMT, BasicBlock, LIST>(stmt);
        } else if (is_token && std::get<TokenPtr>(stmt_head)->is_type(TokenType::CONTINUETK)) {
            return visit<NodeType::CONTINUE_STMT, BasicBlock, LIST>(stmt);
        } else {
            //TODO ERROR
            throw std::runtime_error("[Visitor::visit<STMT, BasicBlock, LIST>] Unexpected stmt type");
        }
    }

    template <>
    std::shared_ptr<Argument> Visitor::visit<NodeType::FUNC_F_PARAM, Argument, SINGLE>(ASTNodePtr &func_f_param) {
        // FuncFParam -> BType Ident [ '[' ']' ]
        cur_b_type = parse_type(std::get<ASTNodePtr>(func_f_param->get_children().front()));
        std::string arg_name = std::get<TokenPtr>(func_f_param->get_children()[1])->get_content();
        auto arg_type = get_var_type(func_f_param->get_children().begin() + 2, func_f_param->get_children().end());

        //error check
        auto &arg_token = std::get<TokenPtr>(func_f_param->get_children()[1]);

        // NOTE here, tne name of the argument is set to be orginal token name,
        // it would be rewritten when first entrying visit<BLOCK, BasicBlock, LIST>
        return std::make_shared<Argument>(arg_type, cur_function, arg_name);
    }

    template <>
    std::list<std::shared_ptr<Argument>> Visitor::visit<NodeType::FUNC_F_PARAMS, Argument, LIST>(
        ASTNodePtr &func_f_params) {
        // FuncFParams -> FuncFParam { ',' FuncFParam }
        std::list<std::shared_ptr<Argument>> result_list;

        // clang-format off
        func_f_params->for_each_child([this, &result_list](auto &&child) {
            std::visit(overloaded{
                    [this, &result_list](ASTNodePtr &node) {
                        result_list.push_back(visit<NodeType::FUNC_F_PARAM, Argument, SINGLE>(node));
                    },
                    [](TokenPtr &token) {
                        // TODO NOTHING
                    }
            }, child);
        });
        // clang-format on

        return result_list;
    }

    template <>
    std::shared_ptr<Function> Visitor::visit<NodeType::FUNC_DEF, Function, SINGLE>(ASTNodePtr &func_def) {
        // FuncDef -> FuncType Ident '(' [FuncFParams] ')' Block
        auto &children = func_def->get_children();
        
        // make a fake function type to initialize the function and insert to symbol table
        // fill the func type after construct the arguments
        const auto func_name = std::get<TokenPtr>(children[1])->get_content();
        //error check
        auto &ident_token = std::get<TokenPtr>(func_def->get_children()[1]);

        cur_function = std::make_shared<Function>(PlaceholderType::get(), FUNCTION_PREFIX + func_name);

        //visit arguments
        auto arguments = children.size() == 6
                             ? visit<NodeType::FUNC_F_PARAMS, Argument, LIST>(std::get<ASTNodePtr>(children[3]))
                             : std::list<std::shared_ptr<Argument>>();
        // get param types
        std::vector<std::shared_ptr<Type>> param_types;
        param_types.reserve(arguments.size());
        std::transform(
            arguments.begin(), arguments.end(), std::back_inserter(param_types), [](auto &arg) { return arg->get_type(); });
        const auto return_type = parse_type(std::get<ASTNodePtr>(children[0]));
        const auto func_type = FunctionType::get(return_type, param_types);
        cur_function->set_type(func_type);
        cur_function->set_arguments(std::vector<std::shared_ptr<Argument>>(arguments.begin(), arguments.end()));


        symbol_table.insert(func_name, SymbolType::FUNC, cur_function, ident_token->get_line());

        // ERROR_CHECK 'return'
        bool has_return = false;
        size_t return_line = 0;
        size_t rb_line = 0;
        auto &block = std::get<ASTNodePtr>(children.back());
        rb_line = std::get<TokenPtr>(block->get_children().back())->get_line();
        bool h_s = std::holds_alternative<ASTNodePtr>(block->get_children()[(block->get_children()).size() - 2]);
        if (h_s) {
            auto &last_item = std::get<ASTNodePtr>(block->get_children()[(block->get_children()).size() - 2]);
            auto &last_stmt = std::get<ASTNodePtr>(last_item->get_children().front());
            auto &s_head = last_stmt->get_children().front();
            if (std::holds_alternative<TokenPtr>(s_head) && std::get<TokenPtr>(s_head)->is_type(TokenType::RETURNTK) && last_stmt->get_children().size() > 2) {
                has_return = true;
                return_line = std::get<TokenPtr>(s_head)->get_line();
            }
        }
        if (cur_function->get_return_type()->is_void_ty() && has_return) {
            error_report(return_line, 'f');
        } else if (cur_function->get_return_type()->is_integer_ty() && !has_return) {
            error_report(rb_line, 'g');
        }
        

        // new scope



        cur_basic_block = std::make_shared<BasicBlock>(cur_function, gen_block_name());
        cur_function->add_basic_block(cur_function->get_basic_blocks_ref().end(), cur_basic_block);
        func_arg_init = false;
        func_init_line = std::get<TokenPtr>(children[1])->get_line() ; //not so right

        // Block
        auto basic_block_list = visit<NodeType::BLOCK, BasicBlock, LIST>(std::get<ASTNodePtr>(children.back()));
        cur_function->add_basic_blocks(std::move(basic_block_list));

        // if the function has no return statement, add a return instruction
        if (cur_basic_block->get_instructions_ref().empty() ||
            !std::dynamic_pointer_cast<Ret>(cur_basic_block->get_instructions_ref().back()))
        {
            if (cur_function->get_return_type()->id == Type::TypeID::VOID_ID)
            {
                cur_basic_block->add_instructions({Ret::create(cur_basic_block)});
            }
            else
            {
                if (cur_function->get_return_type()->id == Type::TypeID::INTEGER_ID) {
                    cur_basic_block->add_instructions(
                        {Ret::create(cur_basic_block, std::make_shared<ConstantInt>(IntegerType::get(32), 0))});
                } else {
                    // TODO ERROR
                }
            }
        }




        return cur_function;
    }

    template <>
    std::shared_ptr<Function> Visitor::visit<NodeType::MAIN_FUNC_DEF, Function, SINGLE>(ASTNodePtr &func_def) {
        // MainFuncDef -> 'int' 'main' '(' ')' Block
        auto &children = func_def->get_children();
        auto &main_ident = std::get<TokenPtr>(children[1]);

        // make a fake function type to initialize the function and insert to symbol table
        // fill the func type after construct the arguments
        const auto func_name = "main";
        cur_function = std::make_shared<Function>(PlaceholderType::get(), FUNCTION_PREFIX + func_name);

        //visit arguments
        auto arguments = std::list<std::shared_ptr<Argument>>();

        // get param types
        std::vector<std::shared_ptr<Type>> param_types;
        param_types.reserve(arguments.size());
        std::transform(
            arguments.begin(), arguments.end(), std::back_inserter(param_types), [](auto &arg) { return arg->get_type(); });
        const auto return_type = IntegerType::get(32);
        const auto func_type = FunctionType::get(return_type, param_types);
        cur_function->set_type(func_type);
        cur_function->set_arguments(std::vector<std::shared_ptr<Argument>>(arguments.begin(), arguments.end()));


        symbol_table.insert(func_name, SymbolType::FUNC, cur_function, main_ident->get_line());

        // ERROR_CHECK 'return'
        bool has_return = false;
        size_t return_line = 0;
        size_t rb_line = 0;
        auto &block = std::get<ASTNodePtr>(children.back());
        rb_line = std::get<TokenPtr>(block->get_children().back())->get_line();
        bool h_s = std::holds_alternative<ASTNodePtr>(block->get_children()[(block->get_children()).size() - 2]);
        if (h_s)
        {
            auto &last_item = std::get<ASTNodePtr>(block->get_children()[(block->get_children()).size() - 2]);
            auto &last_stmt = std::get<ASTNodePtr>(last_item->get_children().front());
            auto &s_head = last_stmt->get_children().front();
            if (std::holds_alternative<TokenPtr>(s_head) && std::get<TokenPtr>(s_head)->is_type(TokenType::RETURNTK) && last_stmt->get_children().size() > 2)
            {
                has_return = true;
                return_line = std::get<TokenPtr>(s_head)->get_line();
            }
        }
        if (cur_function->get_return_type()->is_void_ty() && has_return)
        {
            error_report(return_line, 'f');
        }
        else if (cur_function->get_return_type()->is_integer_ty() && !has_return)
        {
            error_report(rb_line, 'g');
        }

        // new scope
        cur_basic_block = std::make_shared<BasicBlock>(cur_function, gen_block_name());
        cur_function->add_basic_block(cur_function->get_basic_blocks_ref().end(), cur_basic_block);
        func_arg_init = false;

        //Block
        auto basic_block_list = visit<NodeType::BLOCK, BasicBlock, LIST>(std::get<ASTNodePtr>(children.back()));
        cur_function->add_basic_blocks(std::move(basic_block_list));

        // if the function has no return statement, add a return instruction
        if (cur_basic_block->get_instructions_ref().empty() ||
            !std::dynamic_pointer_cast<Ret>(cur_basic_block->get_instructions_ref().back()))
        {
            if (cur_function->get_return_type()->id == Type::TypeID::VOID_ID)
            {
                cur_basic_block->add_instructions({Ret::create(cur_basic_block)});
            }
            else
            {
                if (cur_function->get_return_type()->id == Type::TypeID::INTEGER_ID) {
                    cur_basic_block->add_instructions(
                        {Ret::create(cur_basic_block, std::make_shared<ConstantInt>(IntegerType::get(32), 0))});
                } else {
                    // TODO ERROR
                }
            }
        }


        return cur_function;
    }

    template <>
    std::shared_ptr<ArrayValueWrapper<Constant>> Visitor::visit<NodeType::INIT_VAL, ArrayValueWrapper<Constant>, SINGLE>(
        ASTNodePtr &init_val) {
        // InitVal -> Exp | '{' [Exp { ',' Exp} ] '}'
        if (init_val->get_children().size() == 2) {
            return std::make_shared<ArrayValueWrapper<Constant>>(ArrayValueWrapper<Constant>::WrapperVec{});
        }

        if (init_val->get_children().size() > 2) {
            ArrayValueWrapper<Constant>::WrapperVec initializers;
            init_val->for_each_child([this, &initializers](auto &&child)
                                     {
            if (std::holds_alternative<ASTNodePtr>(child)) {
                auto &node = std::get<ASTNodePtr>(child);
                auto value = visit<NodeType::EXP, Constant, SINGLE>(node);
                initializers.push_back(
                    std::make_shared<ArrayValueWrapper<Constant>>(value));
            } });
            return std::make_shared<ArrayValueWrapper<Constant>>(std::move(initializers));
        }
        return std::make_shared<ArrayValueWrapper<Constant>>(
            visit<NodeType::EXP, Constant, SINGLE>(std::get<ASTNodePtr>(init_val->get_children()[0])));
    }

    template <>
    std::shared_ptr<ArrayValueWrapper<Constant>>
    Visitor::visit<NodeType::CONST_INIT_VAL, ArrayValueWrapper<Constant>, SINGLE>(ASTNodePtr &const_init_val) {
        // ConstInitVal -> ConstExp | '{' [ConstExp {',' ConstExp}] '}'

        if (const_init_val->get_children().size() == 2) {
            return std::make_shared<ArrayValueWrapper<Constant>>(ArrayValueWrapper<Constant>::WrapperVec{});
        }

        if (const_init_val->get_children().size() > 2) {
            ArrayValueWrapper<Constant>::WrapperVec initializers;
            const_init_val->for_each_child([this, &initializers](auto &&child)
                                           {
            if (std::holds_alternative<ASTNodePtr>(child)) {
                auto &node = std::get<ASTNodePtr>(child);
                auto value = visit<NodeType::CONST_EXP, Constant, SINGLE>(node);
                initializers.push_back(
                    std::make_shared<ArrayValueWrapper<Constant>>(value));
            } });
            return std::make_shared<ArrayValueWrapper<Constant>>(std::move(initializers));
        }
        return std::make_shared<ArrayValueWrapper<Constant>>(
            visit<NodeType::CONST_EXP, Constant, SINGLE>(std::get<ASTNodePtr>(const_init_val->get_children()[0])));
    }

    template <>
    std::shared_ptr<GlobalVariable> Visitor::visit<NodeType::VAR_DEF, GlobalVariable, SINGLE>(ASTNodePtr &var_def) {
        // VarDef -> Ident ['[' ConstExp ']'] ['=' InitVal]

        // Ident
        auto ident_name = std::get<TokenPtr>(var_def->get_children()[0])->get_content();
        auto &ident_token = std::get<TokenPtr>(var_def->get_children()[0]);

        // type, wrapped by pointer
        int offset = std::holds_alternative<TokenPtr>(var_def->get_children().back()) ? 0 : -2;
        auto base_type = get_var_type(var_def->get_children().begin() + 1, var_def->get_children().end() + offset);
        auto type = PointerType::get(base_type);

        // InitVal
        bool has_init = var_def->get_children().size() > 1 &&
                        std::holds_alternative<TokenPtr>(var_def->get_children()[var_def->get_children().size() - 2]) &&
                        std::get<TokenPtr>(var_def->get_children()[var_def->get_children().size() - 2])->get_type() ==
                            token_type::TokenType::ASSIGN;
        auto init_val = has_init ? wrap_cast_to_const(visit<NodeType::INIT_VAL, ArrayValueWrapper<Constant>, SINGLE>(
                                                          std::get<ASTNodePtr>(var_def->get_children().back()))
                                                          ->format_as(base_type),
                                                      base_type)
                                 : get_zero(base_type);

        // add to symbol table
        auto var_value = std::make_shared<GlobalVariable>(type, init_val, false, gen_global_var_name());
        symbol_table.insert(ident_name, SymbolType::VAR, var_value,ident_token->get_line());

        return var_value;
    }

    template <>
    std::shared_ptr<GlobalVariable> Visitor::visit<NodeType::CONST_DEF, GlobalVariable, SINGLE>(ASTNodePtr &const_def) {
        // ConstDef -> Ident {'[' ConstExp ']'} '=' ConstInitVal

        // Ident
        auto ident_name = std::get<TokenPtr>(const_def->get_children()[0])->get_content();
        auto &ident_token = std::get<TokenPtr>(const_def->get_children()[0]);

        // type
        auto base_type = get_var_type(const_def->get_children().begin() + 1, const_def->get_children().end() - 2);
        auto type = PointerType::get(base_type);

        // ConstInitVal
        auto const_init_val = wrap_cast_to_const(visit<NodeType::CONST_INIT_VAL, ArrayValueWrapper<Constant>, SINGLE>(
                                                     std::get<ASTNodePtr>(const_def->get_children().back()))
                                                     ->format_as(base_type),
                                                 base_type);
        auto global_variable = std::make_shared<GlobalVariable>(type, const_init_val, true, gen_global_var_name());

        if (base_type->id != Type::TypeID::ARRAY_ID) {
            // if it's not array, only insert init val to symbol tabel
            // is not necessary to collect const normal global var to module
            symbol_table.insert(ident_name, SymbolType::CON, const_init_val, ident_token->get_line());
        } else {
            symbol_table.insert(ident_name, SymbolType::CON, global_variable, ident_token->get_line());
        }

        return global_variable;
    }

    template <>
    std::list<std::shared_ptr<Instruction>> Visitor::visit<NodeType::CONST_DEF, Instruction, LIST>(ASTNodePtr &const_def){
        // ConstDef -> Ident {'[' ConstExp ']'} '=' ConstInitVal
        std::list<std::shared_ptr<Instruction>> ins_list;

        // Ident
        auto ident_name = std::get<TokenPtr>(const_def->get_children()[0])->get_content();
        auto &ident_token = std::get<TokenPtr>(const_def->get_children()[0]);

        // type
        int offset = std::holds_alternative<TokenPtr>(const_def->get_children().back()) ? 0 : -2;
        auto type = get_var_type(const_def->get_children().begin() + 1, const_def->get_children().end() + offset);

        // If is array, consider it as a normal variable, alloca mem for it.
        // Because when retrieving values from an array, index may not be constant.
        if (type->id == Type::TypeID::ARRAY_ID)
        {
            // alloca memory
            auto alloca = Alloca::create(cur_basic_block, type, gen_local_var_name());
            ins_list.push_back(alloca);
            // insert to symbol table
            // TODO(Xingkun): whether to insert as var or con
            symbol_table.insert(ident_name, SymbolType::CON, alloca, ident_token->get_line());

            // collect init array val
            auto [ori_arr_wrap, new_ins] = visit_array_init_val(std::get<ASTNodePtr>(const_def->get_children().back()));
            ins_list.splice(ins_list.end(), new_ins);
            auto init_val_wrap = ori_arr_wrap->format_as(type);
            // store for alloca
            std::vector<std::shared_ptr<Value>> indexes = {std::make_shared<ConstantInt>(IntegerType::get(32), 0)};
            ins_list.splice(ins_list.end(), store_init_val_to_array(init_val_wrap, alloca, indexes));
            return ins_list;
        }

        // Otherwise, only set init value to symbol table
        auto const_init_val =
            std::get<std::shared_ptr<Constant>>(visit<NodeType::CONST_INIT_VAL, ArrayValueWrapper<Constant>, SINGLE>(
                                                    std::get<ASTNodePtr>(const_def->get_children().back()))
                                                    ->val);
        symbol_table.insert(ident_name, SymbolType::CON, const_init_val, ident_token->get_line());
        return {};
    }

    template <>
    std::list<std::shared_ptr<Instruction>> Visitor::visit<NodeType::VAR_DEF, Instruction, LIST>(ASTNodePtr &var_def) {
        // VarDef -> Ident ['[' ConstExp ']'] ['=' InitVal]
        std::list<std::shared_ptr<Instruction>> ins_list;

        // Ident
        auto ident_name = std::get<TokenPtr>(var_def->get_children()[0])->get_content();
        auto &ident_token = std::get<TokenPtr>(var_def->get_children()[0]);

        // type
        int offset = std::holds_alternative<TokenPtr>(var_def->get_children().back()) ? 0 : -2;
        auto type = get_var_type(var_def->get_children().begin() + 1, var_def->get_children().end() + offset);

        // alloca for var
        auto alloca = Alloca::create(cur_basic_block, type, gen_local_var_name());
        ins_list.push_back(alloca);
        // insert to symbol table
        symbol_table.insert(ident_name, SymbolType::VAR, alloca, ident_token->get_line());

        // further 
        // if no init val, return here
        if (var_def->get_children().size() <= 1 ||
            !std::holds_alternative<TokenPtr>(var_def->get_children()[var_def->get_children().size() - 2]) ||
            std::get<TokenPtr>(var_def->get_children()[var_def->get_children().size() - 2])->get_type() !=
                token_type::TokenType::ASSIGN)
        {
            return ins_list;
        }

        // InitVal
        if (type->id == Type::TypeID::ARRAY_ID)
        {
            // Array Init
            // collect init array val
            auto [ori_arr_wrap, new_ins] = visit_array_init_val(std::get<ASTNodePtr>(var_def->get_children().back()));
            ins_list.splice(ins_list.end(), new_ins);
            auto init_val_wrap = ori_arr_wrap->format_as(type);
            // store for alloca
            std::vector<std::shared_ptr<Value>> indexes = {std::make_shared<ConstantInt>(IntegerType::get(32), 0)};
            ins_list.splice(ins_list.end(), store_init_val_to_array(init_val_wrap, alloca, indexes));
            return ins_list;
        }

        // Normal init
        if (var_def->get_children().size() != 1)
        {
            auto [init_val, new_ins] = visit_exps<NodeType::EXP>(
                std::get<ASTNodePtr>(std::get<ASTNodePtr>(var_def->get_children().back())->get_children().front()));
            ins_list.splice(ins_list.end(), new_ins);
            // type conversion
            auto [actual_val, conv_ins] = make_collect_type_conversion(
                init_val, std::dynamic_pointer_cast<PointerType>(alloca->get_type())->get_reference_type());
            ins_list.splice(ins_list.end(), conv_ins);
            auto store = Store::create(cur_basic_block, actual_val, alloca, gen_local_var_name());
            ins_list.push_back(store);
        }

        return ins_list;
    }

    template <>
    std::list<std::shared_ptr<Instruction>> Visitor::visit<NodeType::VAR_DECL, Instruction, LIST>(ASTNodePtr &var_decl) {
        // VarDecl -> ['static'] BType VarDef {',' VarDef} ';'
        std::list<std::shared_ptr<Instruction>> result_list;
        auto &children = var_decl->get_children();

        // 'static'
        bool is_static = std::holds_alternative<TokenPtr>(children.front());

        // BType
        const ASTNodePtr &b_type_node = is_static ? std::get<ASTNodePtr>(children[1]) : std::get<ASTNodePtr>(children[0]);
        cur_b_type = parse_type(b_type_node);

        // VarDef
        if (is_static) {
            for (size_t i = 2; i< children.size(); i += 2) {
                auto &var_def = std::get<ASTNodePtr>(children[i]);
                auto var = visit<NodeType::VAR_DEF, GlobalVariable, SINGLE>(var_def);
                var->is_sp_static = true;
                this->module->add_global_variables({var});

                // TODO ERROR
            }
        } else {
            for (size_t i = 1; i < children.size(); i += 2) {
                result_list.splice(result_list.end(),
                                   visit<NodeType::VAR_DEF, Instruction, LIST>(std::get<ASTNodePtr>(children[i])));
            }
        }
        return result_list;
    }

    template <>
    std::list<std::shared_ptr<GlobalVariable>> Visitor::visit<NodeType::VAR_DECL, GlobalVariable, LIST>(
        ASTNodePtr &var_decl) {
        // VarDecl -> ['static'] BType VarDef {',' VarDef} ';'
        std::list<std::shared_ptr<GlobalVariable>> result_list;
        auto &children = var_decl->get_children();

        // 'static'
        bool is_static = std::holds_alternative<TokenPtr>(children.front());

        // BType
        const ASTNodePtr &b_type_node = is_static ? std::get<ASTNodePtr>(children[1]) : std::get<ASTNodePtr>(children[0]);
        cur_b_type = parse_type(b_type_node);

        // VarDef
        if (is_static) {
            for (size_t i = 2; i< children.size(); i += 2) {
                result_list.push_back(visit<NodeType::VAR_DEF, GlobalVariable, SINGLE>(std::get<ASTNodePtr>(children[i])));
            }
        } else {
            for (size_t i = 1; i < children.size(); i += 2) {
                result_list.push_back(visit<NodeType::VAR_DEF, GlobalVariable, SINGLE>(std::get<ASTNodePtr>(children[i])));
            }
        }
        return result_list;
    }

    template <>
    std::list<std::shared_ptr<Instruction>> Visitor::visit<NodeType::CONST_DECL, Instruction, LIST>(
        ASTNodePtr &const_decl) {
        // ConstDecl -> 'const' BType ConstDef {',' ConstDef} ';'
        std::list<std::shared_ptr<Instruction>> result_list;
        auto &children = const_decl->get_children();

        // BType
        cur_b_type = parse_type(std::get<ASTNodePtr>(children[1]));

        // ConstDef
        for (size_t i = 2; i < children.size(); i += 2) {
            result_list.splice(result_list.end(),
                               visit<NodeType::CONST_DEF, Instruction, LIST>(std::get<ASTNodePtr>(children[i])));
        }

        return result_list;
    }

    template <>
    std::list<std::shared_ptr<GlobalVariable>> Visitor::visit<NodeType::CONST_DECL, GlobalVariable, LIST>(
        ASTNodePtr &const_decl) {
        // ConstDecl -> 'const' BType ConstDef {',' ConstDef} ';'
        std::list<std::shared_ptr<GlobalVariable>> result_list;
        auto &children = const_decl->get_children();

        // BType
        cur_b_type = parse_type(std::get<ASTNodePtr>(children[1]));

        // ConstDef
        for (size_t i = 2; i < children.size(); i += 2) {
            result_list.push_back(visit<NodeType::CONST_DEF, GlobalVariable, SINGLE>(std::get<ASTNodePtr>(children[i])));
        }

        return result_list;
    }

    template <>
    std::list<std::shared_ptr<Instruction>> Visitor::visit<NodeType::DECL, Instruction, LIST>(ASTNodePtr &decl) {
        // Decl -> ConstDecl | VarDecl
        std::list<std::shared_ptr<Instruction>> result_list;
        // clang-format off
        decl->for_each_child([this, &result_list](auto &&child) {
            std::visit(
                overloaded{
                    [this, &result_list](ASTNodePtr &node) {
                        if (node->get_type() == NodeType::CONST_DECL) {
                            result_list.splice(result_list.end(), visit<NodeType::CONST_DECL, Instruction, LIST>(node));
                        } else if (node->get_type() == NodeType::VAR_DECL) {
                            result_list.splice(result_list.end(), visit<NodeType::VAR_DECL, Instruction, LIST>(node));
                        } else {
                            throw std::runtime_error("[Visitor::visit] unexpected node type in DECL");
                        }
                    },
                    [](const TokenPtr &token) {
                        throw std::runtime_error("[Visitor::visit] unexpected token type in DECL");
                    }},
                child);
        });
        //clang-format on

        return result_list;
    }

    template <>
    std::list<std::shared_ptr<GlobalVariable>> Visitor::visit<NodeType::DECL, GlobalVariable, LIST>(ASTNodePtr &decl) {
        // Decl -> ConstDecl | VarDecl
        // clang-format off
        std::list<std::shared_ptr<GlobalVariable>> result_list;
        decl->for_each_child([this, &result_list](auto &&child) {
            std::visit(overloaded{
                    [this, &result_list](ASTNodePtr &node) {
                        if (node->get_type() == NodeType::CONST_DECL) {
                            auto const_vars = visit<NodeType::CONST_DECL, GlobalVariable, LIST>(node);
                            result_list.splice(result_list.end(), const_vars);
                        } else if (node->get_type() == NodeType::VAR_DECL) {
                            auto var_vars = visit<NodeType::VAR_DECL, GlobalVariable, LIST>(node);
                            result_list.splice(result_list.end(), var_vars);
                        } else {
                            throw std::runtime_error("[Visitor::visit] unexpected node type in DECL");
                        }
                    },
                    [](const TokenPtr &token) {
                        throw std::runtime_error("[Visitor::visit] unexpected token type in DECL");
                    }
            }, child);
        });
        //clang-format on

        return result_list;
    }
}
namespace frontend::visitor{
    void Visitor::visit(const ASTNodePtr &comp_unit)
    {
        //Add lib funcs
        for (const auto &lib_func: Function::get_lib_funcs()) {
            symbol_table.insert(lib_func->get_name().substr(1), SymbolType::FUNC, lib_func, 0);
        }

        // visit CompUnit
        comp_unit->for_each_child([this](auto &&child) {
            std::visit(overloaded{  //TODO
                [this](ASTNodePtr &node) {
                    if (node->get_type() == NodeType::FUNC_DEF) {
                        module->add_function(visit<NodeType::FUNC_DEF, Function, SINGLE>(node));
                    } else if (node->get_type() == NodeType::DECL) {
                        module->add_global_variables(visit<NodeType::DECL, GlobalVariable, LIST>(node));
                    } else if (node->get_type() == NodeType::MAIN_FUNC_DEF){
                        module->add_function(visit<NodeType::MAIN_FUNC_DEF, Function, SINGLE>(node));
                    } else {
                        //TODO ERROR
                        throw std::runtime_error("no way");
                    }
                },
                [](TokenPtr &) {
                    //TODO ERROR
                        throw std::runtime_error("no way");
                }},
        child);
        });
        auto records = symbol_table.get_records();

        std::stable_sort(records.begin(), records.end(),
                 [](const SymbolRecord &a, const SymbolRecord &b) {
                     return a.scope_id < b.scope_id;
                 });

        for (const auto &record : records) {
            if (record.name != "getint" && record.name != "main")
            out_file << record.scope_id << ' ' << record.name << ' ' << record.kind << std::endl;
        }
    }

    std::shared_ptr<Type> Visitor::parse_type(const ASTNodePtr &type_node) {
        if (type_node->get_type() != NodeType::B_TYPE && type_node->get_type() != NodeType::FUNC_TYPE) {
            throw std::runtime_error("[Visitor::parseType] Expected B_TYPE or FUNC_TYPE node");
        }

        switch (std::get<TokenPtr>(type_node->get_children().front())->get_type())
        {
        case token_type::TokenType::INTTK:
            return IntegerType::get(32);
        case token_type::TokenType::VOIDTK:
            return VoidType::get();
        default:
            throw std::runtime_error("unexpected token type in parse_type()");
        }
    }

    std::tuple<std::shared_ptr<Value>, std::shared_ptr<Value>, std::list<std::shared_ptr<Instruction>>>
    Visitor::make_binary_type_conversion(const std::shared_ptr<Value> &left, const std::shared_ptr<Value> &right)
    {
        if (left->get_type() == right->get_type())
        {
            return {left, right, {}};
        }
        if (left->get_type() == IntegerType::get(32) && right->get_type() == IntegerType::get(1))
        {
            auto zext = ZExt::create(cur_basic_block, right, IntegerType::get(32), gen_local_var_name());
            return {left, zext, {zext}};
        }

        if (left->get_type() == IntegerType::get(1) && right->get_type() == IntegerType::get(32))
        {
            auto zext = ZExt::create(cur_basic_block, left, IntegerType::get(32), gen_local_var_name());
            return {zext, right, {zext}};
        }
        throw std::runtime_error("[Visitor::make_binary_type_conversion] Invalid type input.");
    }


    std::pair<std::shared_ptr<Value>, std::list<std::shared_ptr<Instruction>>> Visitor::make_collect_type_conversion(
        const std::shared_ptr<Value> &val, const std::shared_ptr<Type> &target_type)
    {
        std::list<std::shared_ptr<Instruction>> instructions;
        if (val->get_type() == target_type)
        {
            return {val, instructions};
        }
        if (target_type == IntegerType::get(1))
        {
            std::shared_ptr<Instruction> value;
            value = ICmp::create(cur_basic_block,
                                 ICmp::ICmpType::NE,
                                 val,
                                 std::make_shared<ConstantInt>(IntegerType::get(32), 0),
                                 gen_local_var_name());

            instructions.push_back(value);
            return {value, instructions};
        }
        if (auto con = std::dynamic_pointer_cast<Constant>(val); con != nullptr)
        {
            return {con->cast_to_int(), instructions};
        }
        //only int
        auto zext = ZExt::create(cur_basic_block, val, IntegerType::get(32), gen_local_var_name());
        instructions.push_back(zext);
        return {zext, instructions};
    }

    std::shared_ptr<Instruction> Visitor::get_icmp_create_func(token_type::TokenType op,
                                                               const std::shared_ptr<Value> &left,
                                                               const std::shared_ptr<Value> &right)
    {
        switch (op)
        {
        case token_type::TokenType::LSS:
            return ICmp::create(
                cur_basic_block, ICmp::ICmpType::SLT, left, right, gen_local_var_name());
        case token_type::TokenType::LEQ:
            return ICmp::create(
                cur_basic_block, ICmp::ICmpType::SLE, left, right, gen_local_var_name());
        case token_type::TokenType::GRE:
            return ICmp::create(
                cur_basic_block, ICmp::ICmpType::SGT, left, right, gen_local_var_name());
        case token_type::TokenType::GEQ:
            return ICmp::create(
                cur_basic_block, ICmp::ICmpType::SGE, left, right, gen_local_var_name());
        case token_type::TokenType::EQL:
            return ICmp::create(
                cur_basic_block, ICmp::ICmpType::EQ, left, right, gen_local_var_name());
        case token_type::TokenType::NEQ:
            return ICmp::create(
                cur_basic_block, ICmp::ICmpType::NE, left, right, gen_local_var_name());
        default:
            throw std::runtime_error("get_icmp_create_func error");
        }
    }
    std::pair<std::shared_ptr<ArrayValueWrapper<Value>>, std::list<std::shared_ptr<Instruction>>>
    Visitor::visit_array_init_val(frontend::ast::ASTNodePtr &array_init_val) {
        // ConstInitVal -> ConstExp | '{' [ConstInitVal {',' ConstInitVal}] '}'
        // ConstInitVal -> ConstExp | '{' [ConstExp {',' ConstExp}] '}'
        // InitVal -> Exp | '{' [InitVal {',' InitVal}] '}'
        // InitVal -> Exp | '{' [Exp { ',' Exp} ] '}'
        if (array_init_val->get_children().size() == 2)
        {
            return {std::make_shared<ArrayValueWrapper<Value>>(ArrayValueWrapper<Value>::WrapperVec{}), {}};
        }
        if (array_init_val->get_children().size() > 2)
        {
            std::list<std::shared_ptr<Instruction>> ins_list;
            ArrayValueWrapper<Value>::WrapperVec initializers;
            array_init_val->for_each_child([this, &ins_list, &initializers](auto &&child)
                                           {
            if (std::holds_alternative<ASTNodePtr>(child)) {
                auto &node = std::get<ASTNodePtr>(child);
                
                std::shared_ptr<Value> value;
                std::list<std::shared_ptr<Instruction>> new_ins;

                // 关键修复 1：
                // 对于初始化列表中的每个元素，我们必须调用能生成运行时IR指令的函数。
                // 无论是 ConstExp 还是 Exp，我们都应使用 visit_exps，它会返回 Value 和 指令列表。
                // 否则，如果使用 visit<... , Constant, ...>，会尝试常量求值，导致 getint() 失败。
                if (node->get_type() == NodeType::CONST_EXP) {
                     // 如果是常量表达式，使用 visit_exps<CONST_EXP> 进行求值并返回常量值
                    std::tie(value, new_ins) = visit_exps<NodeType::CONST_EXP>(node);
                } else {
                     // 如果是运行时表达式（如 getint()），使用 visit_exps<EXP> 
                     // 这会生成 CALL 指令等，并返回一个 Value (例如 %t1)
                    std::tie(value, new_ins) = visit_exps<NodeType::EXP>(node);
                }
                
                // 关键修复 2：
                // 收集表达式求值过程中生成的所有指令。
                ins_list.splice(ins_list.end(), new_ins); 

                initializers.push_back(
                    std::make_shared<ArrayValueWrapper<Value>>(value));
            }
        });
        return {std::make_shared<ArrayValueWrapper<Value>>(initializers), ins_list};
    }
        if (array_init_val->get_type() == NodeType::CONST_INIT_VAL)
        {
            auto [tmp_val, new_ins] =
                visit_exps<NodeType::CONST_EXP>(std::get<ASTNodePtr>(array_init_val->get_children().front()));
            return {std::make_shared<ArrayValueWrapper<Value>>(tmp_val), new_ins};
        }
        else if (array_init_val->get_type() == NodeType::INIT_VAL)
        {
            auto [tmp_val, new_ins] =
                visit_exps<NodeType::EXP>(std::get<ASTNodePtr>(array_init_val->get_children().front()));
            return {std::make_shared<ArrayValueWrapper<Value>>(tmp_val), new_ins};
        }
        else
        {
            throw std::runtime_error("invalid in visit_array_init_val");
        }
    }

    

    std::shared_ptr<Constant> Visitor::wrap_cast_to_const(const std::shared_ptr<ArrayValueWrapper<Constant>> &wrap,
                                                          const std::shared_ptr<Type> &arr_type) {
        if (wrap->is_single())
        {
            return std::get<ArrayValueWrapper<Constant>::WrapperScalar>(wrap->val);
        }
        auto arr_vec = std::get<ArrayValueWrapper<Constant>::WrapperVec>(wrap->val);
        if (arr_vec.empty())
        {
            return get_zero(arr_type);
        }
        std::vector<std::shared_ptr<Constant>> inits;
        inits.reserve(arr_vec.size());
        for (const auto &init : arr_vec)
        {
            inits.push_back(wrap_cast_to_const(init, std::dynamic_pointer_cast<ArrayType>(arr_type)->get_element_type()));
        }
        return std::make_shared<ConstantArray>(arr_type, std::move(inits));
    }

    std::list<std::shared_ptr<Instruction>> Visitor::store_init_val_to_array(
        const std::shared_ptr<ArrayValueWrapper<Value>> &wrapper,
        const std::shared_ptr<Value> &arr_ptr,
        std::vector<std::shared_ptr<Value>> &indexes) {
        std::list<std::shared_ptr<Instruction>> ins_list;
        if (wrapper->is_single())
        {
            auto value = std::get<ArrayValueWrapper<Value>::WrapperScalar>(wrapper->val);
            if (std::dynamic_pointer_cast<ZeroInitializer>(value))
            {
                // zero initializer, make all elements zero
                auto gep = Getelementptr::create(cur_basic_block, arr_ptr, indexes, gen_local_var_name());
                auto bitcast = Bitcast::create(
                    cur_basic_block, gep, PointerType::get(IntegerType::get(8)), gen_local_var_name());
                auto memset = Memset::create(
                    cur_basic_block, bitcast, 0, value->get_type()->bits_num() / 8, gen_local_var_name());
                ins_list.splice(ins_list.end(), {gep, bitcast, memset});
                return ins_list;
            }

            // generate gep to get addr
            auto gep = Getelementptr::create(cur_basic_block, arr_ptr, indexes, gen_local_var_name());
            ins_list.push_back(gep);

            // type conversion
            auto [collected_val, new_ins] = make_collect_type_conversion(
                value, std::dynamic_pointer_cast<PointerType>(gep->get_type())->get_reference_type());
            ins_list.splice(ins_list.end(), new_ins);

            // generate store instruction
            auto store = Store::create(cur_basic_block, collected_val, gep, gen_local_var_name());
            ins_list.push_back(store);
            return ins_list;
        }
        auto arr_wrap = std::get<ArrayValueWrapper<Value>::WrapperVec>(wrapper->val);
        for (size_t i = 0; i < arr_wrap.size(); ++i)
        {
            if (arr_wrap[i]->is_zero())
            {
                // combine zero into memset
                size_t j = i;
                while (j < arr_wrap.size() && arr_wrap[j]->is_zero())
                    j++;
                if (j - i > 1)
                {
                    indexes.push_back(std::make_shared<ConstantInt>(IntegerType::get(32), i));
                    auto gep = Getelementptr::create(cur_basic_block, arr_ptr, indexes, gen_local_var_name());
                    auto bitcast = Bitcast::create(cur_basic_block, gep, PointerType::get(IntegerType::get(8)), gen_local_var_name());
                    auto memset = Memset::create(cur_basic_block, bitcast, 0, static_cast<int>(j - i) * arr_wrap[i]->bits_num() / 8, gen_local_var_name());
                    ins_list.splice(ins_list.end(), {gep, bitcast, memset});
                    indexes.pop_back();
                    i = j - 1;
                    continue;
                }
            }
            indexes.push_back(std::make_shared<ConstantInt>(IntegerType::get(32), i));
            ins_list.splice(ins_list.end(), store_init_val_to_array(arr_wrap[i], arr_ptr, indexes));
            indexes.pop_back();
        }
        return ins_list;
    }

    std::shared_ptr<Type> Visitor::get_var_type(GrammarNodeIterator begin, GrammarNodeIterator end) {
        auto res = cur_b_type;
        for (auto it = std::reverse_iterator(end); it != std::reverse_iterator(begin); ++it)
        {
            const auto &grammar_node = *it;
            if (!std::holds_alternative<TokenPtr>(grammar_node))
                continue;
            if (std::get<TokenPtr>(grammar_node)->get_type() == token_type::TokenType::RBRACK)
                continue;
            if (const auto &exp = *(it - 1); std::holds_alternative<TokenPtr>(exp))
            {
                res = PointerType::get(res);
            }
            else
            {
                res = ArrayType::get(
                    res,
                    visit<NodeType::CONST_EXP, Constant, SINGLE>(const_cast<ASTNodePtr &>(std::get<ASTNodePtr>(exp)))
                        ->cast_to_int()
                        ->get_val());
            }
        }
        return res;
    }
}

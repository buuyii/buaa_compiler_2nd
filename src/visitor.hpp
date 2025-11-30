#ifndef BYC_FRONTEND_VISITOR_HPP
#define BYC_FRONTEND_VISITOR_HPP

#include <list>
#include <memory>
#include <optional>
#include <unordered_map>

#include "ast.hpp"
#include "ir/mod.hpp"

namespace frontend::symbol_table
{
enum class SymbolType { CON, VAR, FUNC, ARG };

struct SymbolRecord {
    int scope_id;
    std::string name;
    std::string kind;
};


class SymbolTable {
    using SymbolContent = std::pair<std::shared_ptr<ir::Value>, SymbolType>;

public:
    void enter_scope();
    void exit_scope();
    void insert(std::string name, SymbolType type, const std::shared_ptr<ir::Value> &val, size_t line) const;
    std::optional<SymbolContent> lookup(std::string name);
    void remove(const std::string &name) const;

    SymbolTable() { enter_scope(); }

    int logical_scope_id = 0;
    std::vector<int> logical_scope_stack = {};

    void enter_logical_scope()
    {
        logical_scope_id = logical_scope_id + 1;
        logical_scope_stack.push_back(logical_scope_id);
    }

    void exit_logical_scope()
    {
        logical_scope_stack.pop_back();
    }

    int current_logical_scope() const
    {
        return logical_scope_stack.back();
    }
    bool in_fb = false;
    
    const std::vector<SymbolRecord> &get_records() const {
        return records;
    }

    inline bool is_defined_in_cur_scope(std::string name) const {
        const auto &scope = scopes.back();
        auto map = (*scope);
        auto found = map.find(name);
        return (found != map.end());    
    }

private:
    std::vector<std::unique_ptr<std::unordered_map<std::string, SymbolContent>>> scopes;

    std::vector<SymbolRecord> records;
};
} // namespace frontend::symbol_table
namespace frontend::visitor {
using namespace ast;
using namespace ir;
using namespace symbol_table;

using VisitExpsResult = std::pair<std::shared_ptr<ir::Value>, std::list<std::shared_ptr<ir::Instruction>>>;

class Visitor {
public:
    explicit Visitor(Module *module) : module(module) {}
    void visit(const ASTNodePtr &comp_unit);
    enum class VisitPattern {SINGLE, LIST};

private:
    template <NodeType node_type, typename value, VisitPattern pattern>
    auto visit(ASTNodePtr &)-> std::enable_if_t<
        std::is_base_of_v<Value, value>,
        std::conditional_t<pattern == VisitPattern::LIST, std::list<std::shared_ptr<value>>,std::shared_ptr<value>>> 
        {
            //TODO
            throw std::runtime_error("no visit implement");
    }
    template <NodeType node_type>
    VisitExpsResult visit_exps(ASTNodePtr &) {
        //TODO
        throw std::runtime_error("no visit_exps implement");
    } 

    static std::shared_ptr<Type> parse_type(const ASTNodePtr &type_node);

    //
    [[nodiscard]] std::tuple<std::shared_ptr<Value>, std::shared_ptr<Value>, std::list<std::shared_ptr<Instruction>>>
    make_binary_type_conversion(const std::shared_ptr<Value> &left, const std::shared_ptr<Value> &right);
    
    std::shared_ptr<Instruction> get_icmp_create_func(token_type::TokenType op,
                                                      const std::shared_ptr<Value> &left,
                                                      const std::shared_ptr<Value> &right);

    // An assistant method to convert the given `val` to the same type as `target_type` so that executing
    // operation on it. Return the converted Value and a list (actually one or none) of Instructions
    [[nodiscard]] std::pair<std::shared_ptr<Value>, std::list<std::shared_ptr<Instruction>>>
    make_collect_type_conversion(const std::shared_ptr<Value> &val, const std::shared_ptr<Type> &target_type);

    std::shared_ptr<Type> get_var_type(GrammarNodeIterator begin, GrammarNodeIterator end);

    // visit ConstInitVal or InitVal, return the ArrayWrapper and a list of Instructions
    std::pair<std::shared_ptr<ArrayValueWrapper<Value>>, std::list<std::shared_ptr<Instruction>>> visit_array_init_val(
        ASTNodePtr &array_init_val);

    // Store the InitVal or ConstInitVal to array
    std::list<std::shared_ptr<Instruction>> store_init_val_to_array(
        const std::shared_ptr<ArrayValueWrapper<Value>> &wrapper,
        const std::shared_ptr<Value> &arr_ptr,
        std::vector<std::shared_ptr<Value>> &indexes);

    // convert an ArrayValueWrapper<Constant> to standard Constant format
    inline static std::shared_ptr<Constant> wrap_cast_to_const(const std::shared_ptr<ArrayValueWrapper<Constant>> &wrap,
                                                               const std::shared_ptr<Type> &arr_type);

private:
    Module *module;
    SymbolTable symbol_table;
    std::shared_ptr<BasicBlock> cur_basic_block;
    std::shared_ptr<Function> cur_function;
    std::list<std::shared_ptr<BasicBlock>> true_basic_block_list;
    std::list<std::shared_ptr<BasicBlock>> false_basic_block_list;
    std::list<std::shared_ptr<BasicBlock>> loop_entry_blocks;
    std::list<std::shared_ptr<BasicBlock>> loop_exit_blocks;

    std::shared_ptr<Type> cur_b_type;
    bool func_arg_init = false;
    bool in_array_arg = false;
    std::stack<bool> is_left; // LVal

    int loop_depth = 0;
    int func_init_line = 0;
};
} //namespace frontend::visitor


#endif // BYC_FRONTEND_VISITOR_HPP
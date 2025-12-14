#ifndef BYC_IR_VALUE_HPP
#define BYC_IR_VALUE_HPP
#include <algorithm>
#include <array>
#include <cassert>
#include <cstdint>
#include <functional>
#include <iomanip>
#include <ios>
#include <iostream>
#include <list>
#include <memory>
#include <ostream>
#include <sstream>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "type.hpp"

namespace ir {

class Value;
class User;
class Function;
class Argument;
class GlobalVariable;
class BasicBlock;
class Instruction;
class Constant;

using BasicBlockNode = std::list<std::shared_ptr<BasicBlock>>::iterator;
using InstructionNode = std::list<std::shared_ptr<Instruction>>::iterator;


class Value {
public:
    std::shared_ptr<Type> get_type() const { return type; }
    void add_user(std::shared_ptr<User> user);
    std::string get_name() { return name; }
    void set_name(std::string name) { this->name = std::move(name); }
    void set_type(const std::shared_ptr<Type> &type) { this->type = type; }
    std::vector<std::weak_ptr<User>> &get_users_ref() { return users; }
    inline void swap_users(std::shared_ptr<Value> other) { std::swap(users, other->users); }
    void remove_user(const std::shared_ptr<User> &user);
    inline int get_id() const { return id; }

    virtual std::string to_string() const = 0;
    explicit Value(const std::shared_ptr<Type> &type, std::string name = "") 
        : id (generate_id()), name(std::move(name)), type(type) {}
    virtual ~Value() = default;
    virtual bool is_zero() const { return false; }

    bool is_sp_static = 0;

protected:
    int id;
    std::string name; //llvm name for print
    std::shared_ptr<Type> type;
    std::vector<std::weak_ptr<User>> users;

private:
    static int generate_id();
};

class User : public Value {
public:
    void add_operand(std::shared_ptr<Value> operand) { operands.push_back(operand); }
    std::vector<std::shared_ptr<Value>> &get_operands_ref() { return operands; }
    std::vector<std::shared_ptr<Value>> get_operands() { return operands; }

public:
    User(const std::shared_ptr<Type> &type, std::string name = "") : Value(type, std::move(name)) {}

protected:
    std::vector<std::shared_ptr<Value>> operands;
};

class Function: public Value {
public:
    Function(const std::shared_ptr<Type> &type, std::string name = "") : Value(type,std::move(name)) {}
    BasicBlockNode add_basic_block(BasicBlockNode pos, const std::shared_ptr<BasicBlock> &basic_block);
    void set_arguments(std::vector<std::shared_ptr<Argument>> &&arguments) { this->arguments = arguments; }
    std::list<std::shared_ptr<BasicBlock>> &get_basic_blocks_ref() { return basic_blocks; }
    std::list<std::shared_ptr<BasicBlock>> get_basic_blocks() { return basic_blocks; }
    BasicBlockNode erase_basic_block(BasicBlockNode pos) { return basic_blocks.erase(pos); }

    bool is_main() const { return name == "@main"; }


    std::shared_ptr<Type> get_return_type() const {
        return std::dynamic_pointer_cast<FunctionType>(type)->get_return_type();
    }
    std::vector<std::shared_ptr<Type>> get_param_types() const {
        return std::dynamic_pointer_cast<FunctionType>(type)->get_param_types();
    }
    
    std::vector<std::shared_ptr<Argument>> &get_argument_ref() { return arguments; }
    void add_basic_blocks(std::list<std::shared_ptr<BasicBlock>> &&basic_blocks);
    std::shared_ptr<BasicBlock> entry_block() const {
        return basic_blocks.empty() ? nullptr : basic_blocks.front();
    }
    std::shared_ptr<BasicBlock> tail_block() const {
        return basic_blocks.empty() ? nullptr : basic_blocks.back();
    }
    template <typename Func>
    void for_each_block(Func &&f) {
        for (auto block : basic_blocks) {
            std::invoke(std::forward<Func>(f), block);
        }
    }

    template <typename Func>
    void for_each_argument(Func &&f) {
        for (auto arg : arguments) {
            std::invoke(std::forward<Func>(f), arg);
        }
    }

    std::string to_string() const override;

    static std::shared_ptr<Function> getint;
    static std::shared_ptr<Function> getchar;
    static std::shared_ptr<Function> getarray;
    static std::shared_ptr<Function> putint;
    static std::shared_ptr<Function> putch;
    static std::shared_ptr<Function> putarray;
    static std::shared_ptr<Function> putstr;

    static const auto &get_lib_funcs()
    {
        const static std::array<std::shared_ptr<Function>, 7> lib_funcs = {
            getint, getchar, getarray, putint, putch, putarray, putstr 
        };
        return lib_funcs;
    }

    static bool is_lib( const std::shared_ptr<Function> &func) {
        auto lib_funcs = get_lib_funcs();
        return std::find(lib_funcs.begin(), lib_funcs.end(), func) != lib_funcs.end();
    } 

private:
    std::vector<std::shared_ptr<Argument>> arguments;
    std::list<std::shared_ptr<BasicBlock>> basic_blocks;
};

class GlobalVariable : public Value {
public:
    explicit GlobalVariable(const std::shared_ptr<Type> &type,
                            const std::shared_ptr<Constant> &init_value,
                            bool is_const,
                            std::string name = "")
                    : Value(type, std::move(name)), init_value(init_value), is_const(is_const) {}
    std::string to_string() const override;
    std::shared_ptr<Constant> get_init_value() const { return init_value; }
    bool is_arr() const {
        return std::dynamic_pointer_cast<PointerType>(type)->get_reference_type()->is_array_ty();
    }

private:
    std::shared_ptr<Constant> init_value;
    bool is_const;
};

class Argument : public Value {
public:
    explicit Argument(const std::shared_ptr<Type> &type,
                      const std::shared_ptr<Function> &parent_func,
                      std::string name = "")
        : Value(type, std::move(name)), parent_func(parent_func) {}
    std::string to_string() const override;
private:
    std::weak_ptr<Function> parent_func;
};

class BasicBlock : public Value {
public:
    explicit BasicBlock(const std::shared_ptr<Function> &parent_func, std::string name = "")
        : Value(LabelType::get(),std::move(name)), parent_func(parent_func) {}
    void add_instructions(std::list<std::shared_ptr<Instruction>> &&instructions);
    InstructionNode add_Instruction(InstructionNode pos, const std::shared_ptr<Instruction> &ins);
    std::list<std::shared_ptr<Instruction>> &get_instructions_ref() { return instructions; }
    std::list<std::shared_ptr<Instruction>> get_instructions() { return instructions; }
    InstructionNode erase_instruction(InstructionNode pos) { return instructions.erase(pos); }
    std::weak_ptr<Function> get_parent_func() const {return parent_func;}
    void set_parent_func(const std::shared_ptr<Function> &parent_func) { this->parent_func = parent_func; }
    std::shared_ptr<Instruction> tail_instruction() const {
        return instructions.empty() ? nullptr : instructions.back();
    }

    template <typename Func>
    void for_each_instruction(Func &&f) {
        for (auto instruction: instructions) {
            std::invoke(std::forward<Func>(f), instruction);
        }
    }

    std::string to_string() const override;

    BasicBlockNode node{};

private:
    std::list<std::shared_ptr<Instruction>> instructions;
    std::weak_ptr<Function> parent_func;
};

//llvm instruction
//avoid struggle to "enable_shared_from_this"
class Instruction : public User {
public:
    enum class InstructionType {
    //
    RET, BR,
    //
    ADD, SUB, MUL, SDIV, SREM,
    SHL, LSHR, ASHR, AND, OR, XOR,
    // Unary instructions
    FNEG,
    // Memory instructions
    ALLOCA, LOAD, STORE, GETELEMENTPTR,
    // Conversion instructions
    TRUNC, ZEXT, BITCAST, FPTOSI, SITOFP,
    // Functional instructions
    ICMP, FCMP, PHI, PHICOPY, MOVE, SELECT, CALL, MEMSET,
    };
    // Clone the instruction (without setting parent block)
    friend std::ostream &operator<<(std::ostream &os, const InstructionType &type);
    InstructionType get_ins_type() const { return ins_type; }
    std::weak_ptr<BasicBlock> get_parent_block() const { return parent_block; }
    bool is_type(InstructionType type) const { return type == ins_type; }
    bool is_binary() const {
        return ins_type >= InstructionType::ADD && ins_type <= InstructionType::XOR;
    }
    bool is_conversion() const {
        return ins_type >= InstructionType::TRUNC && ins_type <= InstructionType::FPTOSI;
    }
    void set_parent_block(const std::shared_ptr<BasicBlock> &block) { parent_block = block; }

    virtual std::shared_ptr<Instruction> clone() const = 0;
    InstructionNode node{};

protected:
    explicit Instruction(const std::shared_ptr<Type> &type,
                     InstructionType ins_type,
                     const std::shared_ptr<BasicBlock> &parent,
                     const std::string &name = "")
        : User(type, name), ins_type(ins_type), parent_block(parent) {}

protected: 
    InstructionType ins_type;
    std::weak_ptr<BasicBlock> parent_block;
};

extern const std::unordered_map<Instruction::InstructionType, std::string> BINARY_INS_TYPE_TO_STRING_MAP;
extern const std::unordered_map<Instruction::InstructionType, std::string> CONVERSION_INS_TYPE_TO_STRING_MAP;

class ConstantInt;

//llvm constant is base class of all IR constants
class Constant : public Value {
public:
    explicit Constant(const std::shared_ptr<Type> &type, std::string name) : Value(type, std::move(name)) {}
    virtual std::shared_ptr<Constant> operator+(const Constant &) const { return nullptr; }
    virtual std::shared_ptr<Constant> operator-(const Constant &) const { return nullptr; }
    virtual std::shared_ptr<Constant> operator*(const Constant &) const { return nullptr; }
    virtual std::shared_ptr<Constant> operator/(const Constant &) const { return nullptr; }
    virtual std::shared_ptr<Constant> operator%(const Constant &) const { return nullptr; }
    virtual std::shared_ptr<Constant> operator-() const { return nullptr; }

    virtual std::shared_ptr<ConstantInt> cast_to_int() const { return std::shared_ptr<ConstantInt>(); }
};

class ConstantInt : public Constant {
public:
    ConstantInt(const std::shared_ptr<Type> &type, int val) : Constant(type, std::to_string(val)), val(val) {}
    ConstantInt(const std::shared_ptr<ConstantInt> &other) : Constant(other->type, other->name), val(other->val) {}
    int get_val() const { return val; }
    std::string to_string() const override { return name; }
    bool is_zero() const override { return val == 0; }

    std::shared_ptr<Constant> operator+(const Constant &) const override;
    std::shared_ptr<Constant> operator-(const Constant &) const override;
    std::shared_ptr<Constant> operator*(const Constant &) const override;
    std::shared_ptr<Constant> operator/(const Constant &) const override;
    std::shared_ptr<Constant> operator%(const Constant &) const override;
    std::shared_ptr<Constant> operator-() const override;

    bool operator==(const ConstantInt &other) const { return val == other.val; }
    bool operator!=(const ConstantInt &other) const { return val != other.val; }
    bool operator<(const ConstantInt &other) const { return val < other.val; }
    bool operator<=(const ConstantInt &other) const { return val <= other.val; }
    bool operator>(const ConstantInt &other) const { return val > other.val; }
    bool operator>=(const ConstantInt &other) const { return val >= other.val; }
    
    std::shared_ptr<ConstantInt> cast_to_int() const override { return std::make_shared<ConstantInt>(type, val); }

private:
    int val;
};

//is this of use?
class ConstantArray : public Constant {
public:
    ConstantArray(const std::shared_ptr<Type> &type, std::vector<std::shared_ptr<Constant>> vals, std::string name = "")
        : Constant(type, std::move(name)), vals(std::move(vals)) {}
    std::string to_string() const override;

    std::vector<std::shared_ptr<Constant>> &get_vals() { return vals; }
private:
    std::vector<std::shared_ptr<Constant>> vals;
};

class ZeroInitializer : public Constant {
public:
    explicit ZeroInitializer(const std::shared_ptr<Type> &type) : Constant(type, "zeroinitializer") {}
    std::string to_string() const override { return name; }
    bool is_zero() const override { return true; }
};

inline std::shared_ptr<Constant> get_zero(const std::shared_ptr<Type> &type) {
    if (type->is_integer_ty())
        return std::make_shared<ConstantInt>(IntegerType::get(32), 0);
    if (type->is_array_ty())
        return std::make_shared<ZeroInitializer>(type);
}

class Placeholder: public Value {
public:
    Placeholder(const std::shared_ptr<Type> &type, std::string name) : Value(type, name) {}
    std::string to_string() const override { return "placeholder " + name + " : " + type->to_string(); }
};

}   //namespace ir

namespace ir {
// represents an array value
// can either be a single value or an array of itself
template <typename T>
struct ArrayValueWrapper : std::enable_shared_from_this<ArrayValueWrapper<T>>, T {
    static_assert(std::is_base_of_v<Value, T>, "T must be derived from Value");

    using WrapperScalar = std::shared_ptr<T>;
    using WrapperVec = std::vector<std::shared_ptr<ArrayValueWrapper<T>>>;
    std::variant<WrapperScalar, WrapperVec> val;

    explicit ArrayValueWrapper(const std::shared_ptr<T> &val) : T(PlaceholderType::get(), ""), val(val) {}
    explicit ArrayValueWrapper(WrapperVec val) : T(PlaceholderType::get(), ""), val(std::move(val)) {}

    inline bool is_array() const { return std::holds_alternative<WrapperVec>(val); }
    inline bool is_single() const { return std::holds_alternative<WrapperScalar>(val); }

    std::string to_string() const override { return "ArrayWrapper"; }

    // format the array value to the given type
    // note: ignore the concrete the base type(float | int) here, just construct the dims info
    std::shared_ptr<ArrayValueWrapper> format_as(const std::shared_ptr<Type> &type) {
        if (is_single()) {
            return ArrayValueWrapper::shared_from_this();
        }
        assert(type->is_array_ty());
        WrapperVec wrap_vec = {ArrayValueWrapper<T>::shared_from_this()};
        auto [res, end_it] = collect_one(wrap_vec.begin(), wrap_vec.end(), type);
        assert(end_it == wrap_vec.end());
        return res;
    }

    bool is_zero() const override {
        if (is_single()) {
            auto scalar = std::get<WrapperScalar>(val);
            return scalar->is_zero();
        } else {
            auto vec = std::get<WrapperVec>(val);
            for (const auto &elem : vec) {
                if (!elem->is_zero()) return false;
            }
            return true;
        }
    }

    [[nodiscard]] int bits_num() const {
        if (is_single()) {
            auto scalar = std::get<WrapperScalar>(val);
            return scalar->get_type()->bits_num();
        } else {
            auto vec = std::get<WrapperVec>(val);
            return vec[0]->bits_num() * vec.size();
        }
    }
private:
    // parse a `ArrayValueWrapper` with `type` from `begin` to `end`
    // return the pair of the `ArrayValueWrapper` and the next iterator
    std::pair<std::shared_ptr<ArrayValueWrapper<T>>, typename WrapperVec::const_iterator> collect_one(
        typename WrapperVec::const_iterator begin,
        typename WrapperVec::const_iterator end,
        const std::shared_ptr<Type> type)
    {
        // There are four cases to parse, depending on whether the current 'begin' and 'type' are arrays or not
        // Recursion endpoint: no more elements in the iteration range
        if (begin == end)
        {
            return {std::make_shared<ArrayValueWrapper>(get_zero(type)), begin};
        }
        // Recursion endpoint: current 'begin' is an array, but it's empty, return zero type
        if (std::holds_alternative<WrapperVec>((*begin)->val) && std::get<WrapperVec>((*begin)->val).empty())
        {
            return {std::make_shared<ArrayValueWrapper>(get_zero(type)), begin + 1};
        }
        // Both 'begin' and 'type' are not arrays
        if (!(*begin)->is_array() && !type->is_array_ty())
        {
            return {*begin, begin + 1};
        }
        // Both 'begin' and 'type' are arrays
        if ((*begin)->is_array() && type->is_array_ty())
        {
            WrapperVec res;
            auto wrap_vec = std::get<WrapperVec>((*begin)->val);
            auto arr_type = std::dynamic_pointer_cast<ArrayType>(type);
            typename WrapperVec::const_iterator cur_it = wrap_vec.begin();

            // if all children are zero, combine them into zeroinitializer
            bool all_zero = true;
            for (int i = 0; i < arr_type->get_size(); i++)
            {
                auto [wrap, next_it] = collect_one(cur_it, wrap_vec.end(), arr_type->get_element_type());
                res.push_back(wrap);
                cur_it = next_it;
                if (!wrap->is_zero())
                    all_zero = false;
            }
            if (all_zero)
                return {std::make_shared<ArrayValueWrapper>(get_zero(arr_type)), begin + 1};
            else
                return {std::make_shared<ArrayValueWrapper>(std::move(res)), begin + 1};
        }
        // 'begin' is not an array, but 'type' is an array
        if (!(*begin)->is_array() && type->is_array_ty())
        {
            WrapperVec res;
            auto arr_type = std::dynamic_pointer_cast<ArrayType>(type);
            typename WrapperVec::const_iterator cur_it = begin;

            // like above, try combine the children into zeroinitializer
            bool all_zero = true;
            for (int i = 0; i < arr_type->get_size(); i++)
            {
                auto [wrap, new_it] = collect_one(cur_it, end, arr_type->get_element_type());
                res.push_back(wrap);
                cur_it = new_it;
                if (!wrap->is_zero())
                    all_zero = false;
            }
            if (all_zero)
                return {std::make_shared<ArrayValueWrapper>(get_zero(arr_type)), cur_it};
            else
                return {std::make_shared<ArrayValueWrapper>(std::move(res)), cur_it};
        }
        // 'begin' is an array but 'type' is not; empty array case has been handled earlier, this is only valid when
        // 'begin' contains a single non-array element
        if ((*begin)->is_array() && !type->is_array_ty())
        {
            auto wrap_vec = std::get<WrapperVec>((*begin)->val);
            assert(wrap_vec.size() == 1);
            assert(wrap_vec[0]->is_single());
            return {wrap_vec[0], begin + 1};
        }

        throw std::runtime_error("arraywrapper error!");
    }
};
} //namespace ir


#endif
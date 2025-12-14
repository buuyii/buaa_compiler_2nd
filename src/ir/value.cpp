#include "value.hpp"

#include <algorithm>
#include <cstddef>


#include "type.hpp"
namespace ir {


int Value::generate_id() {
    static int cnt = 0;
    return cnt++;
}

void Value::add_user(std::shared_ptr<User> user) {
    // add new
    if (std::find_if(users.rbegin(), users.rend(), [&user](const std::weak_ptr<User> &u) {
            return u.lock() == user;
        }) == users.rend()) {
        users.push_back(user);
    }
}

void Value::remove_user(const std::shared_ptr<User> &user) {
    users.erase(std::remove_if(users.begin(), users.end(),
                               [&user](const std::weak_ptr<User> &u)
                               {
                                   auto locked = u.lock();
                                   return !locked || locked == user;
                               }),
                users.end());
}

BasicBlockNode Function::add_basic_block(BasicBlockNode pos, const std::shared_ptr<BasicBlock> &basic_block) {
    return basic_block->node = basic_blocks.insert(pos, basic_block);
}

void Function::add_basic_blocks(std::list<std::shared_ptr<BasicBlock>> &&basic_blocks_to_add) {
    auto old_size = basic_blocks.size();
    this->basic_blocks.splice(this->basic_blocks.end(), std::move(basic_blocks_to_add));

    //
    auto it = basic_blocks.begin();
    std::advance(it, old_size);
    for (; it != basic_blocks.end(); ++it) {
        (*it)->node = it;
    }
}

void BasicBlock::add_instructions(std::list<std::shared_ptr<Instruction>> &&instructions_to_add) {
    auto old_size = instructions.size();
    this->instructions.splice(this->instructions.end(), std::move(instructions_to_add));

    //
    auto it = instructions.begin();
    std::advance(it, old_size);
    for (; it != instructions.end(); ++it) {
        (*it)->node = it;
    }
}

InstructionNode BasicBlock::add_Instruction(InstructionNode pos, const std::shared_ptr<Instruction> &ins) {
    return ins->node = instructions.insert(pos, ins);
}

using InstructionType = Instruction::InstructionType;
const std::unordered_map<InstructionType, std::string> BINARY_INS_TYPE_TO_STRING_MAP = {
    {InstructionType::RET, "ret"},
    {InstructionType::BR, "br"},
    {InstructionType::ADD, "add"},
    {InstructionType::SUB, "sub"},
    {InstructionType::MUL, "mul"},
    {InstructionType::SDIV, "sdiv"},
    {InstructionType::SREM, "srem"},
    {InstructionType::SHL, "shl"},
    {InstructionType::LSHR, "lshr"},
    {InstructionType::ASHR, "ashr"},
    {InstructionType::AND, "and"},
    {InstructionType::OR, "or"},
    {InstructionType::XOR, "xor"},
    {InstructionType::FNEG, "fneg"},
    {InstructionType::ALLOCA, "alloca"},
    {InstructionType::LOAD, "load"},
    {InstructionType::STORE, "store"},
    {InstructionType::GETELEMENTPTR, "getelementptr"},
    {InstructionType::TRUNC, "trunc"},
    {InstructionType::ZEXT, "zext"},
    {InstructionType::FPTOSI, "fptosi"},
    {InstructionType::SITOFP, "sitofp"},
    {InstructionType::ICMP, "icmp"},
    {InstructionType::FCMP, "fcmp"},
    {InstructionType::PHI, "phi"},
    {InstructionType::SELECT, "select"},
    {InstructionType::CALL, "call"},
    {InstructionType::MEMSET, "memset"}};

std::ostream &operator<<(std::ostream &os, const InstructionType &type) {
    os << BINARY_INS_TYPE_TO_STRING_MAP.at(type);
    return os;
}

const std::unordered_map<InstructionType, std::string> CONVERSION_INS_TYPE_TO_STRING_MAP = {
    {InstructionType::TRUNC, "trunc"},
    {InstructionType::ZEXT, "zext"},
    {InstructionType::BITCAST, "bitcast"},
    {InstructionType::FPTOSI, "fptosi"},
    {InstructionType::SITOFP, "sitofp"}};
} // namespace ir

namespace ir {
std::shared_ptr<Function> Function::getint = 
    std::make_shared<Function>(FunctionType::get(IntegerType::get(32), {}), "@getint");
std::shared_ptr<Function> Function::getchar =
    std::make_shared<Function>(FunctionType::get(IntegerType::get(32), {}), "@getchar");
std::shared_ptr<Function> Function::getarray =
    std::make_shared<Function>(FunctionType::get(IntegerType::get(32), {PointerType::get(IntegerType::get(32))}), "@getarray");
std::shared_ptr<Function> Function::putint =
    std::make_shared<Function>(FunctionType::get(VoidType::get(), {IntegerType::get(32)}), "@putint");
std::shared_ptr<Function> Function::putch =
    std::make_shared<Function>(FunctionType::get(VoidType::get(), {IntegerType::get(32)}), "@putch");
std::shared_ptr<Function> Function::putarray = std::make_shared<Function>(
    FunctionType::get(VoidType::get(), {IntegerType::get(32), PointerType::get(IntegerType::get(32))}), "@putarray");
std::shared_ptr<Function> Function::putstr = std::make_shared<Function>(
    FunctionType::get(VoidType::get(), {PointerType::get(IntegerType::get(32))}), "@putstr");
}

namespace ir {
std::string Function::to_string() const {
    std::string res = "define ";
    res += std::dynamic_pointer_cast<FunctionType>(type)->get_return_type()->to_string();
    res += " " + name + "(";
    for (size_t i = 0; i < arguments.size(); i++) {
        res += arguments[i]->to_string();
        if (i != arguments.size() - 1) {
            res += ", ";
        }
    }
    res += ") {\n";
    for (const auto &basic_block : basic_blocks) {
        res += basic_block->to_string();
    }
    res += "}\n\n";
    return res;
}

std::string GlobalVariable::to_string() const {
    auto ref_type = std::dynamic_pointer_cast<PointerType>(type)->get_reference_type();
    return name + " = global " + ref_type->to_string() + " " + init_value->to_string();
}

std::string ConstantArray::to_string() const {
    std::string res = "[";
    for (size_t i = 0; i < std::dynamic_pointer_cast<ArrayType>(type)->get_size(); i++) {
        res += std::dynamic_pointer_cast<ArrayType>(type)->get_element_type()->to_string();
        res += " ";
        if (i >= vals.size()) {
            res += "0"; // only int type
        } else {
            res += vals[i]->to_string();
        }
        if(i != std::dynamic_pointer_cast<ArrayType>(type)->get_size() - 1) {
            res += ", ";
        }
    }
    res += "]";
    return res;
}

std::string Argument::to_string() const {
    return type->to_string() + " " + name;
}

std::string BasicBlock::to_string() const {
    std::string res = name + ":\n";
    for (const auto &ins : instructions) {
        res += "\t" + ins->to_string() + "\n";
    }
    return res;
}

} //namespace ir

namespace ir {
std::shared_ptr<Constant> ConstantInt::operator+(const Constant &other) const {
    if (other.get_type()->is_integer_ty()) {
        return std::make_shared<ConstantInt>(IntegerType::get(32),
                                             val + dynamic_cast<const ConstantInt &>(other).get_val());
    } else {
        return nullptr; // unreachable ERROR
    }
}

std::shared_ptr<Constant> ConstantInt::operator-(const Constant &other) const {
    if (other.get_type()->is_integer_ty()) {
        return std::make_shared<ConstantInt>(IntegerType::get(32),
                                             val - dynamic_cast<const ConstantInt &>(other).get_val());
    } else {
        return nullptr; // unreachable ERROR
    }
}

std::shared_ptr<Constant> ConstantInt::operator*(const Constant &other) const {
    if (other.get_type()->is_integer_ty()) {
        return std::make_shared<ConstantInt>(IntegerType::get(32),
                                             val * dynamic_cast<const ConstantInt &>(other).get_val());
    } else {
        return nullptr; // unreachable ERROR
    }
}

std::shared_ptr<Constant> ConstantInt::operator/(const Constant &other) const {
    if (other.get_type()->is_integer_ty()) {
        return std::make_shared<ConstantInt>(IntegerType::get(32),
                                             val / dynamic_cast<const ConstantInt &>(other).get_val());
    } else {
        return nullptr; // unreachable ERROR
    }
}

std::shared_ptr<Constant> ConstantInt::operator%(const Constant &other) const {
    if (other.get_type()->is_integer_ty()) {
        return std::make_shared<ConstantInt>(IntegerType::get(32),
                                             val % dynamic_cast<const ConstantInt &>(other).get_val());
    } else {
        return nullptr; // unreachable ERROR
    }
}

std::shared_ptr<Constant> ConstantInt::operator-() const {
    return std::make_shared<ConstantInt>(IntegerType::get(32), -val);
}

} // namespace ir
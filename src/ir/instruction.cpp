#include "instruction.hpp"

#include <cassert>
#include <cstddef>
#include <memory>
#include <string>
#include <unordered_map>

#include "type.hpp"
#include "util.hpp"
#include "value.hpp"
// implementation for `create` method
namespace ir {
std::shared_ptr<Ret> Ret::create(const std::shared_ptr<BasicBlock> &parent_block,
                                 const std::shared_ptr<Value> &val,
                                 std::string name) {
    std::shared_ptr<Ret> ret(new Ret(parent_block, val->get_type(), name));
    ret->add_operand(val);
    val->add_user(ret);
    return ret;
}

std::shared_ptr<Ret> Ret::create(const std::shared_ptr<BasicBlock> &parent_block, std::string name) {
    std::shared_ptr<Ret> ret(new Ret(parent_block, name));
    return ret;
}

std::shared_ptr<Br> Br::create(const std::shared_ptr<BasicBlock> &parent_block,
                               const std::shared_ptr<BasicBlock> &target,
                               std::string name) {
    std::shared_ptr<Br> br(new Br(parent_block, name));
    br->add_operand(target);
    target->add_user(br);
    return br;
}

std::shared_ptr<Br> Br::create(const std::shared_ptr<BasicBlock> &parent_block,
                               const std::shared_ptr<Value> &cond,
                               const std::shared_ptr<BasicBlock> &true_target,
                               const std::shared_ptr<BasicBlock> &false_target,
                               std::string name) {
    std::shared_ptr<Br> br(new Br(parent_block, name));
    br->add_operand(cond);
    br->add_operand(true_target);
    br->add_operand(false_target);
    cond->add_user(br);
    true_target->add_user(br);
    false_target->add_user(br);
    return br;
}

std::shared_ptr<Alloca> Alloca::create(const std::shared_ptr<BasicBlock> &parent_block,
                                       const std::shared_ptr<Type> &content_type,
                                       std::string name) {
    std::shared_ptr<Alloca> alloca_(new Alloca(parent_block, content_type, name));  // NOLINT
    return alloca_;
}

std::shared_ptr<Load> Load::create(const std::shared_ptr<BasicBlock> &parent_block,
                                   const std::shared_ptr<Value> &addr,
                                   std::string name) {
    std::shared_ptr<Load> load(
        new Load(parent_block, std::dynamic_pointer_cast<PointerType>(addr->get_type())->get_reference_type(), name));
    load->add_operand(addr);
    addr->add_user(load);
    return load;
}

std::shared_ptr<Store> Store::create(const std::shared_ptr<BasicBlock> &parent_block,
                                     const std::shared_ptr<Value> &val,
                                     const std::shared_ptr<Value> &addr,
                                     std::string name) {
    std::shared_ptr<Store> store(new Store(parent_block, name));
    store->add_operand(val);
    store->add_operand(addr);
    val->add_user(store);
    addr->add_user(store);
    return store;
}

std::shared_ptr<ICmp> ICmp::create(const std::shared_ptr<BasicBlock> &parent_block,
                                   ICmpType cmp_type,
                                   const std::shared_ptr<Value> &left,
                                   const std::shared_ptr<Value> &right,
                                   std::string name) {
    assert(left->get_type() == right->get_type());
    assert(left->get_type()->id == Type::TypeID::INTEGER_ID);
    std::shared_ptr<ICmp> icmp(new ICmp(parent_block, cmp_type, name));
    icmp->add_operand(left);
    icmp->add_operand(right);
    left->add_user(icmp);
    right->add_user(icmp);
    return icmp;
}


std::shared_ptr<Call> Call::create(const std::shared_ptr<BasicBlock> &parent_block,
                                   const std::shared_ptr<Function> &function,
                                   const std::vector<std::shared_ptr<Value>> &args,
                                   std::string name) {
    std::shared_ptr<Call> call(
        new Call(parent_block, std::dynamic_pointer_cast<FunctionType>(function->get_type())->get_return_type(), name));
    call->add_operand(function);
    function->add_user(call);
    for (const auto &arg : args) {
        call->add_operand(arg);
        arg->add_user(call);
    }
    return call;
}

std::shared_ptr<Getelementptr> Getelementptr::create(const std::shared_ptr<BasicBlock> &parent_block,

                                                     const std::shared_ptr<Value> &ptr,
                                                     const std::vector<std::shared_ptr<Value>> &indexes,
                                                     std::string name) {
    auto gep_type = ptr->get_type();
    for (const auto &_ : indexes) {
        if (gep_type->id == Type::TypeID::POINTER_ID) {
            gep_type = std::dynamic_pointer_cast<PointerType>(gep_type)->get_reference_type();
        } else if (gep_type->id == Type::TypeID::ARRAY_ID) {
            gep_type = std::dynamic_pointer_cast<ArrayType>(gep_type)->get_element_type();
        } else {
            //TODO ERROR
        }
    }
    auto gep = std::shared_ptr<Getelementptr>(new Getelementptr(parent_block, PointerType::get(gep_type), name));
    gep->add_operand(ptr);
    ptr->add_user(gep);
    for (const auto &index : indexes) {
        gep->add_operand(index);
        index->add_user(gep);
    }
    return gep;
}

std::shared_ptr<Memset> Memset::create(const std::shared_ptr<BasicBlock> &parent_block,
                                       const std::shared_ptr<Value> &ptr,
                                       int val,
                                       int size,
                                       std::string name) {
    auto memset = std::shared_ptr<Memset>(new Memset(parent_block, val, size, name));
    memset->add_operand(ptr);
    ptr->add_user(memset);
    return memset;
}

std::shared_ptr<Phi> Phi::create(const std::shared_ptr<BasicBlock> &parent_block,
                                 const std::vector<std::shared_ptr<Value>> &values,
                                 const std::shared_ptr<Type> &type,
                                 std::string name) {
    std::shared_ptr<Phi> phi(new Phi(parent_block, type, name));
    for (size_t i = 0; i < values.size(); i++) {
        phi->add_operand(values[i]);
        values[i]->add_user(phi);
        /*
        phi->add_operand(parent_block->opt_info.predecessors[i].lock());
        parent_block->opt_info.predecessors[i].lock()->add_user(phi);
        */
    }
    return phi;
}

std::shared_ptr<Phi> Phi::create(const std::shared_ptr<BasicBlock> &parent_block,
                                 const std::vector<std::shared_ptr<Value>> &values,
                                 const std::vector<std::shared_ptr<BasicBlock>> &blocks,
                                 const std::shared_ptr<Type> &type,
                                 std::string name) {
    std::shared_ptr<Phi> phi(new Phi(parent_block, type, name));
    for (size_t i = 0; i < values.size(); i++) {
        phi->add_operand(values[i]);
        values[i]->add_user(phi);
        phi->add_operand(blocks[i]);
        blocks[i]->add_user(phi);
    }
    return phi;
}

std::shared_ptr<PhiCopy> PhiCopy::create(const std::shared_ptr<BasicBlock> &parent_block, std::string name) {
    std::shared_ptr<PhiCopy> phi_copy(new PhiCopy(parent_block, VoidType::get(), name));
    return phi_copy;
}

std::shared_ptr<Move> Move::create(const std::shared_ptr<BasicBlock> &parent_block,
                                   const std::shared_ptr<Value> &src,
                                   const std::shared_ptr<Value> &dst,
                                   std::string name) {
    assert(src->get_type() == dst->get_type());
    std::shared_ptr<Move> move(new Move(parent_block, src->get_type(), name));
    move->add_operand(src);
    move->add_operand(dst);
    src->add_user(move);
    dst->add_user(move);
    return move;
}

}  // namespace ir

// implementation for `to_string` method
namespace ir {
std::string Ret::to_string() const {
    std::string res = "ret ";
    if (is_void) {
        res += "void";
    } else {
        res += type->to_string();
        res += " ";
        res += operands[0]->get_name();
    }
    return res;
}

std::string Br::to_string() const {
    if (!is_cond_branch()) return "br label %" + operands[0]->get_name();
    std::string res = "br i1 " + operands[0]->get_name();
    res += ", label ";
    res += "%" + operands[1]->get_name();
    res += ", label ";
    res += "%" + operands[2]->get_name();
    return res;
}


std::string Alloca::to_string() const {
    return name + " = alloca " + std::dynamic_pointer_cast<PointerType>(type)->get_reference_type()->to_string();
}

std::string Load::to_string() const {
    auto addr = operands[0];
    return name + " = load " +
           std::dynamic_pointer_cast<PointerType>(addr->get_type())->get_reference_type()->to_string() + ", " +
           std::dynamic_pointer_cast<PointerType>(addr->get_type())->to_string() + " " + addr->get_name();
}

std::string Store::to_string() const {
    auto val = operands[0];
    auto addr = operands[1];
    return "store " + val->get_type()->to_string() + " " + val->get_name() + ", " + addr->get_type()->to_string() +
           " " + addr->get_name();
}
using ICmpType = ICmp::ICmpType;
const static std::unordered_map<ICmpType, std::string> ICMP_TYPE_TO_STRING_MAP = {{ICmpType::EQ, "eq"},
                                                                                  {ICmpType::NE, "ne"},
                                                                                  {ICmpType::UGT, "ugt"},
                                                                                  {ICmpType::UGE, "uge"},
                                                                                  {ICmpType::ULT, "ult"},
                                                                                  {ICmpType::ULE, "ule"},
                                                                                  {ICmpType::SGT, "sgt"},
                                                                                  {ICmpType::SGE, "sge"},
                                                                                  {ICmpType::SLT, "slt"},
                                                                                  {ICmpType::SLE, "sle"}};
std::string ICmp::to_string() const {
    return name + " = " + "icmp " + ICMP_TYPE_TO_STRING_MAP.at(cmp_type) + " " + operands[0]->get_type()->to_string() +
           " " + operands[0]->get_name() + ", " + operands[1]->get_name();
}
std::ostream &operator<<(std::ostream &os, const ICmpType &type) {
    os << ICMP_TYPE_TO_STRING_MAP.at(type);
    return os;
}

std::string Call::to_string() const {
    std::string res = type == VoidType::get() ? "" : name + " = ";
    res += "call " + type->to_string() + " " + operands[0]->get_name() + "(";
    for (size_t i = 1; i < operands.size(); i++) {
        res += operands[i]->get_type()->to_string() + " " + operands[i]->get_name();
        if (i != operands.size() - 1) res += ", ";
    }
    res += ")";
    return res;
}

std::string Getelementptr::to_string() const {
    std::string res = name + " = getelementptr ";
    switch (operands[0]->get_type()->id) {
        case Type::TypeID::POINTER_ID:
            res += std::dynamic_pointer_cast<PointerType>(operands[0]->get_type())->get_reference_type()->to_string();
            break;
        case Type::TypeID::ARRAY_ID:
            res += std::dynamic_pointer_cast<ArrayType>(operands[0]->get_type())->get_element_type()->to_string();
            break;
        default:
            //TODO ERROR
            break;
    }
    res += ", " + operands[0]->get_type()->to_string() + " " + operands[0]->get_name();
    for (size_t i = 1; i < operands.size(); i++) {
        res += ", " + operands[i]->get_type()->to_string() + " " + operands[i]->get_name();
    }
    return res;
}

std::string Memset::to_string() const {
    return "call void @llvm.memset.p0.i32(i8* " + operands[0]->get_name() + ", i8 " + std::to_string(val) + +", i32 " +
           std::to_string(size) + ", i1 false)";
}

std::string Phi::to_string() const {
    std::string res = name + " = phi " + type->to_string() + " ";
    for (size_t i = 0; i < operands.size(); i += 2) {
        if (i != 0) res += ", ";
        res += "[ " + operands[i]->get_name() + ", %" + operands[i + 1]->get_name() + " ]";
    }
    return res;
}

std::string PhiCopy::to_string() const {
    std::string res = name + " = phi_copy " + type->to_string() + " ";
    for (size_t i = 0; i < phis.size(); i++) {
        if (i != 0) res += ", ";
        res += "[ " + phis[i]->get_name() + ", %" + values[i]->get_name() + " ]";
    }
    return res;
}

std::string Move::to_string() const {
    std::string res = "move " + type->to_string() + " ";
    res += operands[0]->get_name() + " to " + operands[1]->get_name();
    return res;
}

}  // namespace ir

// implementation for other method
namespace ir {
bool Br::is_cond_branch() const { return operands.size() == 3; }

ICmpType ICmp::get_inverted_op(ICmpType cmp_type) {
    switch (cmp_type) {
        case ICmpType::EQ:
            return ICmpType::NE;
        case ICmpType::NE:
            return ICmpType::EQ;
        case ICmpType::SLT:
            return ICmpType::SGE;
        case ICmpType::SLE:
            return ICmpType::SGT;
        case ICmpType::SGT:
            return ICmpType::SLE;
        case ICmpType::SGE:
            return ICmpType::SLT;
        default:
            return ICmpType::SLT; // TODO ERROR
    }
}

ICmpType ICmp::get_reversed_op(ICmpType cmp_type) {
    switch (cmp_type) {
        case ICmpType::EQ:
            return ICmpType::EQ;
        case ICmpType::NE:
            return ICmpType::NE;
        case ICmpType::SLT:
            return ICmpType::SGT;
        case ICmpType::SLE:
            return ICmpType::SGE;
        case ICmpType::SGT:
            return ICmpType::SLT;
        case ICmpType::SGE:
            return ICmpType::SLE;
        default:
            return ICmpType::SLE; // TODO ERROR
    }
}

}  // namespace ir

// implementation for clone methods
namespace ir {
std::shared_ptr<Instruction> Ret::clone() const {
    if (is_void) {
        return Ret::create(nullptr, gen_local_var_name());
    } else {
        auto cloned = Ret::create(nullptr, operands[0], gen_local_var_name());
        return cloned;
    }
}

std::shared_ptr<Instruction> Br::clone() const {
    // For Br, we need to handle both conditional and unconditional branches
    if (operands.size() == 1) {
        // Unconditional branch
        return Br::create(nullptr, std::dynamic_pointer_cast<BasicBlock>(operands[0]), gen_local_var_name());
    } else {
        // Conditional branch
        return Br::create(nullptr,
                          operands[0],
                          std::dynamic_pointer_cast<BasicBlock>(operands[1]),
                          std::dynamic_pointer_cast<BasicBlock>(operands[2]),
                          gen_local_var_name());
    }
}


std::shared_ptr<Instruction> Alloca::clone() const {
    return Alloca::create(nullptr, get_content_type(), gen_local_var_name());
}

std::shared_ptr<Instruction> Load::clone() const { return Load::create(nullptr, operands[0], gen_local_var_name()); }

std::shared_ptr<Instruction> Store::clone() const {
    return Store::create(nullptr, operands[0], operands[1], gen_local_var_name());
}

std::shared_ptr<Instruction> ICmp::clone() const {
    return ICmp::create(nullptr, cmp_type, operands[0], operands[1], gen_local_var_name());
}


std::shared_ptr<Instruction> Call::clone() const {
    std::vector<std::shared_ptr<Value>> args;
    for (size_t i = 1; i < operands.size(); i++) {
        args.push_back(operands[i]);
    }
    return Call::create(nullptr, std::dynamic_pointer_cast<Function>(operands[0]), args, gen_local_var_name());
}

std::shared_ptr<Instruction> Getelementptr::clone() const {
    std::vector<std::shared_ptr<Value>> indexes;
    for (size_t i = 1; i < operands.size(); i++) {
        indexes.push_back(operands[i]);
    }
    return Getelementptr::create(nullptr, operands[0], indexes, gen_local_var_name());
}

std::shared_ptr<Instruction> Memset::clone() const {
    return Memset::create(nullptr, operands[0], val, size, gen_local_var_name());
}

std::shared_ptr<Instruction> Phi::clone() const {
    std::vector<std::shared_ptr<Value>> values;
    std::vector<std::shared_ptr<BasicBlock>> blocks;
    for (size_t i = 0; i < operands.size(); i += 2) {
        values.push_back(operands[i]);
        blocks.push_back(std::dynamic_pointer_cast<BasicBlock>(operands[i + 1]));
    }
    return Phi::create(nullptr, values, blocks, type, gen_local_var_name());
}

std::shared_ptr<Instruction> PhiCopy::clone() const {
    std::shared_ptr<PhiCopy> cloned = create(nullptr, gen_local_var_name());
    for (size_t i = 0; i < phis.size(); ++i) {
        cloned->add(phis[i], values[i]);
    }
    return cloned;
}

std::shared_ptr<Instruction> Move::clone() const {
    return create(nullptr, operands[0], operands[1], gen_local_var_name());
}

}  // namespace ir

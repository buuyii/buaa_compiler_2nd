#ifndef BYC_IR_INSTRUCTION_HPP
#define BYC_IR_INSTRUCTION_HPP

#include <memory>
#include <string>
#include <vector>

#include "type.hpp"
#include "value.hpp"
#include "util.hpp"

namespace ir {
class Ret : public Instruction {
public:
    //return a value
    static std::shared_ptr<Ret> create(const std::shared_ptr<BasicBlock> &parent_block,
                                       const std::shared_ptr<Value> &val,
                                       std::string name = "");
    //return void
    static std::shared_ptr<Ret> create(const std::shared_ptr<BasicBlock> &parent_block,
                                       std::string name = "");

    std::string to_string() const override;
    std::shared_ptr<Instruction> clone() const override;

private:
    explicit Ret(const std::shared_ptr<BasicBlock> &parent_block,
                 const std::shared_ptr<Type> &val_type,
                 std::string name = "")
            : Instruction(val_type, InstructionType::RET, parent_block, name), is_void(false) {}
    explicit Ret(const std::shared_ptr<BasicBlock> &parent_block, std::string name = "")
            : Instruction(VoidType::get(), InstructionType::RET, parent_block, name), is_void(true) {}
    
    bool is_void;
};

class Br : public Instruction {
public:
    //unconditional
    [[nodiscard]] static std::shared_ptr<Br> create(const std::shared_ptr<BasicBlock> &parent_block,
                                                    const std::shared_ptr<BasicBlock> &target,
                                                    std::string name);
    //conditional
    [[nodiscard]] static std::shared_ptr<Br> create(const std::shared_ptr<BasicBlock> &parent_block,
                                                    const std::shared_ptr<Value> &cond,
                                                    const std::shared_ptr<BasicBlock> &true_block,
                                                    const std::shared_ptr<BasicBlock> &false_block,
                                                    std::string name = "");
    [[nodiscard]] bool is_cond_branch() const;
    std::string to_string() const override;
    std::shared_ptr<Instruction> clone() const override;

private:
    explicit Br(const std::shared_ptr<BasicBlock> &parent_block, std::string name)
            : Instruction(LabelType::get(), InstructionType::BR, parent_block, name) {}
};

template <Instruction::InstructionType ty>
class BinaryInstructionImpl : public Instruction {
    static_assert(ty >= Instruction::InstructionType::ADD && ty <= Instruction::InstructionType::XOR);

public:
    [[nodiscard]] static std::shared_ptr<BinaryInstructionImpl<ty>> create(
        const std::shared_ptr<BasicBlock> &parent_block,
        const std::shared_ptr<Value> &lhs,
        const std::shared_ptr<Value> &rhs,
        std::string name = "") {
        std::shared_ptr<BinaryInstructionImpl<ty>> ins(
            new BinaryInstructionImpl<ty>(parent_block, lhs->get_type(), name));
        ins->add_operand(lhs);
        ins->add_operand(rhs);
        lhs->add_user(ins);
        rhs->add_user(ins);
        return ins;
    }
    
    std::string to_string() const override {
        return name + " = " + BINARY_INS_TYPE_TO_STRING_MAP.at(ty) + " " + type->to_string() + " " +
               operands[0]->get_name() + ", " + operands[1]->get_name();
    }

    std::shared_ptr<Instruction> clone() const override {
        return BinaryInstructionImpl<ty>::create(nullptr, operands[0], operands[1], gen_local_var_name());
    }

private:
    explicit BinaryInstructionImpl(const std::shared_ptr<BasicBlock> &parent_block,
                                   const std::shared_ptr<Type> &type,
                                   std::string name = "")
        : Instruction(type, ty, parent_block, name) {}
};

using Add = BinaryInstructionImpl<Instruction::InstructionType::ADD>;
using Sub = BinaryInstructionImpl<Instruction::InstructionType::SUB>;
using Mul = BinaryInstructionImpl<Instruction::InstructionType::MUL>;
using SDiv = BinaryInstructionImpl<Instruction::InstructionType::SDIV>;
using SRem = BinaryInstructionImpl<Instruction::InstructionType::SREM>;
using Shl = BinaryInstructionImpl<Instruction::InstructionType::SHL>;
using LShr = BinaryInstructionImpl<Instruction::InstructionType::LSHR>;
using AShr = BinaryInstructionImpl<Instruction::InstructionType::ASHR>;
using And = BinaryInstructionImpl<Instruction::InstructionType::AND>;
using Or = BinaryInstructionImpl<Instruction::InstructionType::OR>;
using Xor = BinaryInstructionImpl<Instruction::InstructionType::XOR>;

template <Instruction::InstructionType ty>
class ConversionInstructionImpl : public Instruction {
    static_assert(ty >= Instruction::InstructionType::TRUNC && ty <= Instruction::InstructionType::SITOFP);

public:
    [[nodiscard]] static std::shared_ptr<ConversionInstructionImpl> create(  //why no ty
        const std::shared_ptr<BasicBlock> &parent_block,
        const std::shared_ptr<Value> &val,
        const std::shared_ptr<Type> &target_type,
        std::string name = "") {
        std::shared_ptr<ConversionInstructionImpl<ty>> ins(
            new ConversionInstructionImpl<ty>(parent_block, target_type, name));
        ins->add_operand(val);
        val->add_user(ins);
        return ins;
    }

    std::string to_string() const override {
        return name + " = " + CONVERSION_INS_TYPE_TO_STRING_MAP.at(ty) + " " +
                operands[0]->get_type()->to_string() + " " + operands[0]->get_name() + " to " +
                type->to_string();
    }

    std::shared_ptr<Value> val() const {return operands[0];}
    
    std::shared_ptr<Instruction> clone() const override {
        return ConversionInstructionImpl<ty>::create(nullptr, operands[0], type, gen_local_var_name());
    }

private:
    explicit ConversionInstructionImpl(const std::shared_ptr<BasicBlock> &parent_block,
                                       const std::shared_ptr<Type> &type,
                                       std::string name = "")
        : Instruction(type, ty, parent_block, name) {}
};

using Trunc = ConversionInstructionImpl<Instruction::InstructionType::TRUNC>;
using ZExt = ConversionInstructionImpl<Instruction::InstructionType::ZEXT>;
using Bitcast = ConversionInstructionImpl<Instruction::InstructionType::BITCAST>;
using FPToSI = ConversionInstructionImpl<Instruction::InstructionType::FPTOSI>;
using SIToFP = ConversionInstructionImpl<Instruction::InstructionType::SITOFP>;

class Alloca : public Instruction {
public:
    [[nodiscard]] static std::shared_ptr<Alloca> create(const std::shared_ptr<BasicBlock> &parent_block,
                                                        const std::shared_ptr<Type> &content_type,
                                                        std::string name = "");
    std::shared_ptr<Type> get_content_type() const {
        return std::dynamic_pointer_cast<PointerType>(type)->get_reference_type();
    }
    std::string to_string() const override;
    std::shared_ptr<Instruction> clone() const override;

private:
    explicit Alloca(const std::shared_ptr<BasicBlock> &parent_block,
                    const std::shared_ptr<Type> &content_type,
                    std::string name = "")
        : Instruction(PointerType::get(content_type), InstructionType::ALLOCA, parent_block, name) {}
};

class Load : public Instruction {
public:
    [[nodiscard]] static std::shared_ptr<Load> create(const std::shared_ptr<BasicBlock> &parent_block,
                                                      const std::shared_ptr<Value> &addr,
                                                      std::string name = "");

    std::string to_string() const override;
    std::shared_ptr<Instruction> clone() const override;

    [[nodiscard]] std::shared_ptr<Value> addr() const { return operands[0]; }

private:
    explicit Load(const std::shared_ptr<BasicBlock> &parent_block,
                  const std::shared_ptr<Type> &type,
                  std::string name = "")
        : Instruction(type, InstructionType::LOAD, parent_block, name) {}
};

class Store : public Instruction {
public:
    [[nodiscard]] static std::shared_ptr<Store> create(const std::shared_ptr<BasicBlock> &parent_block,
                                                       const std::shared_ptr<Value> &val,
                                                       const std::shared_ptr<Value> &addr,
                                                       std::string name = "");

    std::string to_string() const override;
    std::shared_ptr<Instruction> clone() const override;
    [[nodiscard]] std::shared_ptr<Value> addr() const { return operands[1]; }
    [[nodiscard]] std::shared_ptr<Value> val() const { return operands[0]; }

private:
    explicit Store(const std::shared_ptr<BasicBlock> &parent_block, std::string name = "")
        : Instruction(VoidType::get(), InstructionType::STORE, parent_block, name) {}
};

class ICmp : public Instruction {
public:
    enum class ICmpType { EQ, NE, UGT, UGE, ULT, ULE, SGT, SGE, SLT, SLE };
    static ICmpType get_inverted_op(ICmpType cmp_type);
    static ICmpType get_reversed_op(ICmpType cmp_type);

public:
    [[nodiscard]] static std::shared_ptr<ICmp> create(const std::shared_ptr<BasicBlock> &parent_block,
                                                      ICmpType cmp_type,
                                                      const std::shared_ptr<Value> &left,
                                                      const std::shared_ptr<Value> &right,
                                                      std::string name = "");

    std::string to_string() const override;
    ICmpType get_cmp_type() const { return cmp_type; }
    std::shared_ptr<Instruction> clone() const override;
    friend std::ostream &operator<<(std::ostream &os, const ICmpType &type);
    void set_cmp_type(ICmpType cmp_type) { this->cmp_type = cmp_type; }

private:
    explicit ICmp(const std::shared_ptr<BasicBlock> &parent_block, ICmpType cmp_type, std::string name)
        : Instruction(IntegerType::get(1), InstructionType::ICMP, parent_block, name), cmp_type(cmp_type) {}

private:
    ICmpType cmp_type;
};

class Call : public Instruction
{
public:
    [[nodiscard]] static std::shared_ptr<Call> create(const std::shared_ptr<BasicBlock> &parent_block,
                                                      const std::shared_ptr<Function> &function,
                                                      const std::vector<std::shared_ptr<Value>> &args,
                                                      std::string name = "");
    std::shared_ptr<Function> get_function() const { return std::dynamic_pointer_cast<Function>(operands[0]); }

    std::string to_string() const override;
    std::shared_ptr<Instruction> clone() const override;

    [[nodiscard]] std::vector<std::shared_ptr<Value>> args() const
    {
        return std::vector<std::shared_ptr<Value>>(operands.begin() + 1, operands.end());
    }

private:
    explicit Call(const std::shared_ptr<BasicBlock> &parent_block,
                  const std::shared_ptr<Type> &type,
                  std::string name = "")
        : Instruction(type, InstructionType::CALL, parent_block, name) {}
};

class Getelementptr : public Instruction
{
public:
    [[nodiscard]] static std::shared_ptr<Getelementptr> create(const std::shared_ptr<BasicBlock> &parent_block,
                                                               const std::shared_ptr<Value> &ptr,
                                                               const std::vector<std::shared_ptr<Value>> &indexes,
                                                               std::string name = "");

    std::string to_string() const override;
    std::vector<std::shared_ptr<Value>> get_indexes() const
    {
        return std::vector<std::shared_ptr<Value>>(operands.begin() + 1, operands.end());
    }
    std::shared_ptr<Value> base_ptr() const { return operands[0]; }
    std::shared_ptr<Instruction> clone() const override;

private:
    explicit Getelementptr(const std::shared_ptr<BasicBlock> &parent_block,
                           const std::shared_ptr<Type> &type,
                           std::string name = "")
        : Instruction(type, InstructionType::GETELEMENTPTR, parent_block, name) {}
};

class Memset : public Instruction
{
public:
    [[nodiscard]] static std::shared_ptr<Memset> create(const std::shared_ptr<BasicBlock> &parent_block,
                                                        const std::shared_ptr<Value> &ptr,
                                                        int val,
                                                        int size,
                                                        std::string name = "");
    std::string to_string() const override;
    std::shared_ptr<Instruction> clone() const override;
    int get_val() const { return val; }
    int get_size() const { return size; }

private:
    explicit Memset(const std::shared_ptr<BasicBlock> &parent_block, int val, int size, std::string name = "")
        : Instruction(VoidType::get(), InstructionType::MEMSET, parent_block, name), val(val), size(size) {}
    int val;
    int size;
};

class Phi : public Instruction
{
public:
    [[nodiscard]] static std::shared_ptr<Phi> create(const std::shared_ptr<BasicBlock> &parent_block,
                                                     const std::vector<std::shared_ptr<Value>> &values,
                                                     const std::shared_ptr<Type> &type,
                                                     std::string name = "");
    [[nodiscard]] static std::shared_ptr<Phi> create(const std::shared_ptr<BasicBlock> &parent_block,
                                                     const std::vector<std::shared_ptr<Value>> &values,
                                                     const std::vector<std::shared_ptr<BasicBlock>> &blocks,
                                                     const std::shared_ptr<Type> &type,
                                                     std::string name = "");
    bool is_lcssa = false; // if the phi is inserted by `LoopClosedSSA`
    [[nodiscard]] std::vector<std::shared_ptr<Value>> values() const
    {
        std::vector<std::shared_ptr<Value>> result;
        for (size_t i = 0; i + 1 < operands.size(); i += 2)
        {
            result.push_back(operands[i]);
        }
        return result;
    }
    [[nodiscard]] std::vector<std::shared_ptr<BasicBlock>> blocks() const
    {
        std::vector<std::shared_ptr<BasicBlock>> result;
        for (size_t i = 1; i < operands.size(); i += 2)
        {
            result.push_back(std::dynamic_pointer_cast<BasicBlock>(operands[i]));
        }
        return result;
    }
    std::string to_string() const override;
    std::shared_ptr<Instruction> clone() const override;

private:
    explicit Phi(const std::shared_ptr<BasicBlock> &parent_block,
                 const std::shared_ptr<Type> &type,
                 std::string name = "")
        : Instruction(type, InstructionType::PHI, parent_block, name) {}
};

class PhiCopy : public Instruction
{
public:
    [[nodiscard]] static std::shared_ptr<PhiCopy> create(const std::shared_ptr<BasicBlock> &parent_block,
                                                         std::string name = "");
    [[nodiscard]] std::vector<std::shared_ptr<Phi>> get_phis() const { return phis; }
    [[nodiscard]] std::vector<std::shared_ptr<Value>> get_values() const { return values; }
    void add(const std::shared_ptr<Phi> &phi, const std::shared_ptr<Value> &value)
    {
        phis.push_back(phi);
        values.push_back(value);
    }

    void remove(const std::shared_ptr<Phi> &phi, const std::shared_ptr<Value> &value)
    {
        for (auto i = 0; i < phis.size(); ++i)
        {
            if (phis[i] == phi && values[i] == value)
            {
                phis.erase(phis.begin() + i);
                values.erase(values.begin() + i);
                break;
            }
        }
    }
    void change_value(size_t idx, const std::shared_ptr<Value> &value)
    {
        if (idx < values.size())
        {
            values[idx] = value;
        }
    }
    std::string to_string() const override;
    std::shared_ptr<Instruction> clone() const override;

private:
    std::vector<std::shared_ptr<Phi>> phis;
    std::vector<std::shared_ptr<Value>> values;
    explicit PhiCopy(const std::shared_ptr<BasicBlock> &parent_block,
                     const std::shared_ptr<Type> &type,
                     std::string name = "")
        : Instruction(type, InstructionType::PHICOPY, parent_block, name) {}
};

class Move : public Instruction
{
public:
    [[nodiscard]] static std::shared_ptr<Move> create(const std::shared_ptr<BasicBlock> &parent_block,
                                                      const std::shared_ptr<Value> &src,
                                                      const std::shared_ptr<Value> &dst,
                                                      std::string name = "");

    std::string to_string() const override;
    std::shared_ptr<Instruction> clone() const override;

private:
    explicit Move(const std::shared_ptr<BasicBlock> &parent_block,
                  const std::shared_ptr<Type> &type,
                  std::string name = "")
        : Instruction(type, InstructionType::MOVE, parent_block, name) {}
};
}


#endif
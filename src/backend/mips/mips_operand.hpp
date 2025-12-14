#ifndef BYC_MIPS_OPERAND_HPP
#define BYC_MIPS_OPERAND_HPP

#pragma once
#include <set>
#include <string>
#include <utility>

namespace backend::mips {

class MIPSInstruction;
class MIPSFunction;

class MIPSOperand {
public:
    enum class Kind { REG, IMM, LABEL, STK, HI, LO, STACK_FIXER, VIRTUAL_REG };
    virtual Kind get_kind() const = 0;
    virtual std::string to_string() const = 0;
    virtual ~MIPSOperand() = default;

    virtual bool is_precolored() const { return false; }

    virtual void add_graph_use(MIPSInstruction *inst);
    virtual void remove_graph_use(MIPSInstruction *inst);
    virtual const std::set<MIPSInstruction *> &get_graph_uses() const;
    virtual void clear_uses() { graph_uses.clear(); }
protected:
    std::set<MIPSInstruction *> graph_uses;
};

class MIPSReg : public MIPSOperand
{
public:
    Kind get_kind() const override = 0;
    virtual std::string to_string() const override = 0;
    virtual bool is_precolored() const { return false; }
    virtual ~MIPSReg() override = default;
};

class MIPSPhyReg : public MIPSReg
{
public:
    static MIPSPhyReg *create(int phys_id, const std::string &name)
    {
        return new MIPSPhyReg(phys_id, name);
    }
    Kind get_kind() const override { return Kind::REG; }
    int get_phys_id() const { return phys_id; }
    std::string to_string() const override { return name; }
    bool is_precolored() const override { return true; }
    void add_graph_use(MIPSInstruction *inst) override;
    void remove_graph_use(MIPSInstruction *inst) override;

private:
    MIPSPhyReg(int id, std::string n) : phys_id(id), name(std::move(n)) {}

    int phys_id;
    std::string name; // $t0, $a0, $v0, $sp ...
};

class MIPSVirReg : public MIPSReg
{
public:
    static MIPSVirReg *create(int index, MIPSFunction *func)
    {
        return new MIPSVirReg(index, func);
    }
    Kind get_kind() const override { return Kind::VIRTUAL_REG; }
    int get_index() const { return index; }
    std::string to_string() const override { return name; }
    MIPSFunction *get_function() const { return function; }

private:
    MIPSVirReg(int idx, MIPSFunction *func);

    int index;
    std::string name;
    MIPSFunction *function;
};

class MIPSImmediate : public MIPSOperand
{
public:
    static MIPSImmediate *create(long long val) { return new MIPSImmediate(val); }
    Kind get_kind() const override { return Kind::IMM; }
    long long value() const { return val; }
    std::string to_string() const override;

private:
    explicit MIPSImmediate(long long v) : val(v) {}
    long long val;
};

class MIPSLabel : public MIPSOperand {
public:
    static MIPSLabel *create(std::string name);
    Kind get_kind() const override {return Kind::LABEL;}
    std::string to_string() const override { return _name; }
    const std::string &name() const { return _name; }
    virtual ~MIPSLabel() override;
private:
    MIPSLabel(std::string name) : _name(std::move(name)) {}
    std::string _name;
};

class MIPSStk : public MIPSOperand {
public:
    static MIPSStk *create(int offset);
    Kind get_kind() const override { return Kind::STK; }
    std::string to_string() const override;
    int get_offset() const { return _offset; }
    virtual ~MIPSStk() override;
private:
    MIPSStk(int offset) : _offset(offset) {}
    int _offset;
};

class MIPSStackFixer : public MIPSOperand {
public:
    static MIPSStackFixer *create(MIPSFunction *function, int extra_offset);
    Kind get_kind() const override { return Kind::STACK_FIXER; }
    std::string to_string() const override;
    int get_extra_offset() const { return _extra_offset; }
    int get_aligned_offset() const;
    MIPSFunction *get_function() const { return _function; }
    virtual ~MIPSStackFixer() override;

private:
    MIPSStackFixer(MIPSFunction *function, int extra_offset);
    MIPSFunction *_function;
    int _extra_offset;
};

class MIPSHi : public MIPSOperand {
public:
    static MIPSHi *create(MIPSLabel *label);
    Kind get_kind() const override { return Kind::HI; }
    std::string to_string() const override;
    MIPSLabel *get_label() const;
    virtual ~MIPSHi() override;

private:
    MIPSHi(MIPSLabel *label);
    MIPSLabel *_label;
};

class MIPSLo : public MIPSOperand {
public:
    static MIPSLo *create(MIPSLabel *label);
    Kind get_kind() const override { return Kind::LO; }
    std::string to_string() const override;
    MIPSLabel *get_label() const;
    virtual ~MIPSLo() override;
private:
    MIPSLo(MIPSLabel *label);
    MIPSLabel *_label;
};

} // namespace backend::riscv
#endif
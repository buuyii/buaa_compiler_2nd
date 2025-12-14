#include "mips_operand.hpp"

#include <utility>
#include "ir/value.hpp"
#include "mips_function.hpp"
#include "mips_instruction.hpp"

namespace backend::mips {
const char *const REGISTER_NAMES[] = {"zero", "at", "v0", "v1", "a0", "a1", "a2", "a3",
                                      "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
                                      "t8", "t9", "k0", "k1", "gp", "sp", "fp", "ra"};

                                      /* -------- Base MIPSOperand Def-Use Management -------- */
void MIPSOperand::add_graph_use(MIPSInstruction *inst) {
    // assert(inst != nullptr);
    graph_uses.insert(inst);
}

void MIPSOperand::remove_graph_use(MIPSInstruction *inst){
    // assert(inst != nullptr);
    graph_uses.erase(inst);
}

const std::set<MIPSInstruction *> &MIPSOperand::get_graph_uses() const { return graph_uses; }

MIPSOperand::~MIPSOperand() { graph_uses.clear(); }
/* -------- Register -------- */
MIPSPhyReg *MIPSPhyReg::create(int phys_id, const std::string &name) { return new MIPSPhyReg(phys_id, name); }
std::string MIPSPhyReg::to_string() const {
    if (!name.empty())
    {
        return name; // Use alias like "fa0" if it exists
    }
    if (phys_id >= 0 && phys_id < 32)
    {
        return REGISTER_NAMES[phys_id];
    }
    return "$" + std::to_string(phys_id);
}

void MIPSPhyReg::add_graph_use(MIPSInstruction *inst) { MIPSOperand::add_graph_use(inst); }

void MIPSPhyReg::remove_graph_use(MIPSInstruction *inst) { MIPSOperand::remove_graph_use(inst); }

MIPSPhyReg::~MIPSPhyReg() {}
/* -------- Immediate -------- */
MIPSImmediate *MIPSImmediate::create(long long val) { return new MIPSImmediate(val); }

MIPSImmediate::MIPSImmediate(long long val) : val(val) {}

std::string MIPSImmediate::to_string() const { return std::to_string(val); }

MIPSImmediate::~MIPSImmediate() {}

/* -------- Label / Symbol -------- */
MIPSLabel *MIPSLabel::create(std::string name) { return new MIPSLabel(std::move(name)); }

MIPSLabel::~MIPSLabel() {}

/* -------- Stack Location -------- */
MIPSStk *MIPSStk::create(int offset) {return new MIPSStk(offset);}
std::string MIPSStk::to_string() const { return std::to_string(_offset) + "($sp)"; }
MIPSStk:: ~MIPSStk() {}

/* -------- Stack Fixer -------- */
MIPSStackFixer *MIPSStackFixer::create(MIPSFunction *function, int extra_offset) {
    return new MIPSStackFixer(function, extra_offset);
}
MIPSStackFixer::MIPSStackFixer(MIPSFunction *function, int extra_offset) :_function(function), _extra_offset(extra_offset) {}

int MIPSStackFixer::get_aligned_offset() const {
    auto function = _function;
    if (!function) {
        // Fallback if function is no longer available
        return _extra_offset;
    }

    int stack_position = function->get_stack_size();

    if (stack_position == 0)
    {
        return _extra_offset;
    }
    return stack_position - 1 - (stack_position - 1) % 16 + 16 + _extra_offset;
}

std::string MIPSStackFixer::to_string() const {return std::to_string(get_aligned_offset());}
MIPSStackFixer::~MIPSStackFixer() {}

/* -------- Symbol Address Parts -------- */

}

#ifndef BYC_IR_MODULE_HPP
#define BYC_IR_MODULE_HPP

#include <cstddef>
#include <list>
#include <memory>
#include <utility>
#include <vector>
#include <functional>

#include "value.hpp"

namespace ir {
class Module {
public:
    void add_function(const std::shared_ptr<Function> &func) { functions.push_back(func); }
    std::vector<std::shared_ptr<Function>> &get_function_ref() { return functions; }
    std::vector<std::shared_ptr<Function>> get_functions() { return functions; }
    std::shared_ptr<Function> get_main() {
        return *std::find_if(
            functions.begin(), functions.end(), [](const std::shared_ptr<Function> &func)
            { return func->is_main(); });
    }
    // merge private funcs and lib_funcs
    std::vector<std::shared_ptr<Function>> get_all_functions() const {
        std::vector<std::shared_ptr<Function>> all_funcs = functions;
        for (const auto &func : Function::get_lib_funcs()){
            all_funcs.push_back(func);
        }
        return all_funcs;
    }
    void add_global_variables(std::list<std::shared_ptr<GlobalVariable>> gvs) {
        global_variables.insert(global_variables.end(), gvs.begin(), gvs.end());
    }
    std::vector<std::shared_ptr<GlobalVariable>> &get_global_variables_ref() {
        return global_variables;
    }
    //read-only version
    const std::vector<std::shared_ptr<GlobalVariable>> &get_global_variables() const {
        return global_variables;
    }

    void erase(const std::shared_ptr<GlobalVariable> &global_var) {
        global_variables.erase(std::remove(global_variables.begin(), global_variables.end(), global_var),
                               global_variables.end());
    }

    template <typename Func>
    void for_each_func(Func &&f) {
        for (auto func : functions) {
            std::invoke(std::forward<Func>(f), func);
        }
    }

    template <typename Func>
    void for_each_global_variavle(Func &&f) {
        for (auto gv : global_variables) {
            std::invoke(std::forward<Func>(f), gv);
        }
    }

    std::string to_string() const {
        std::string str;
        for (const auto &gv : global_variables) {
            str += gv->to_string() + "\n";
        }
        str += "declare void @llvm.memset.p0.i32(i8* nocapture writeonly, i8, i32, i1 immarg)\n"; //what
        str += "\n";
        for (const auto &func : Function::get_lib_funcs()) {
            str += "declate dso_local " + func->get_return_type()->to_string() + " " + func->get_name() + "(";
            for (size_t i = 0; i < func->get_param_types().size(); i++) {
                if (i)
                    str += ", ";
                str += func->get_param_types()[i]->to_string();
            }
            str += ")\n";
        }
        str += "\n";
        for (const auto &func: functions) {
            str += func->to_string() + "\n";
        }
        return str;
    }

private:
    std::vector<std::shared_ptr<Function>> functions;
    std::vector<std::shared_ptr<GlobalVariable>> global_variables;
};
}


#endif
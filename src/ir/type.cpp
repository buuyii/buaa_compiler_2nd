#include "type.hpp"

#include <memory>
#include <ostream>
#include <unordered_map>

namespace ir {
std::ostream &operator<<(std::ostream &os, const Type &type) {
    os << type.to_string();
    return os;
}

std::shared_ptr<VoidType> VoidType::get() {
    static std::shared_ptr<VoidType> instance(new VoidType());
    return instance;
}

std::shared_ptr<LabelType> LabelType::get() {
    static std::shared_ptr<LabelType> instance(new LabelType());
    return instance;
}

std::shared_ptr<PlaceholderType> PlaceholderType::get()
{
    static std::shared_ptr<PlaceholderType> instance(new PlaceholderType());
    return instance;
}

std::shared_ptr<IntegerType> IntegerType::get(int bits) {
    static std::unordered_map<int, std::shared_ptr<IntegerType>> cache;
    auto it = cache.find(bits);
    if (it != cache.end()) {
        return it->second;
    }
    // non-exist
    auto new_type = std::shared_ptr<IntegerType>(new IntegerType(bits));
    cache[bits] = new_type;
    return new_type;
}

std::shared_ptr<FunctionType> FunctionType::get(const std::shared_ptr<Type> &return_type, const std::vector<std::shared_ptr<Type>> &param_types) {
    static std::unordered_map<std::string, std::shared_ptr<FunctionType>> cache;
    std::string key = return_type->to_string() + "(";
    for (const auto &param_type : param_types) {
        key += param_type->to_string() + ",";
    }
    key += ")";
    auto it = cache.find(key);
    if (it !=cache.end()) {
        return it->second;
    }
    // non-exist
    auto new_type = std::shared_ptr<FunctionType>(new FunctionType(return_type, param_types));
    cache[key] = new_type;
    return new_type;
}

std::shared_ptr<PointerType> PointerType::get(const std::shared_ptr<Type> &reference_type) {
    static std::unordered_map<std::shared_ptr<Type>, std::shared_ptr<PointerType>> cache;
    auto it = cache.find(reference_type);
    if (it != cache.end()) {
        return it->second;
    }
    //non-exist
    auto new_type = std::shared_ptr<PointerType>(new PointerType(reference_type));
    cache[reference_type] = new_type;
    return new_type;
}

std::shared_ptr<ArrayType> ArrayType::get(const std::shared_ptr<Type> &element_type, int size) {
    static std::unordered_map<std::string, std::shared_ptr<ArrayType>> cache;
    std::string key = element_type->to_string() + "[" + std::to_string(size) + "]";
    auto it = cache.find(key);
    if (it !=cache.end()) {
        return it->second;
    }

    auto new_type = std::shared_ptr<ArrayType>(new ArrayType(element_type, size));
    cache[key] = new_type;
    return new_type;
}

std::string VoidType::to_string() const { return "void"; }

std::string LabelType::to_string() const { return "label"; }

std::string IntegerType::to_string() const { return "i" + std::to_string(bits); } //for example i32

std::string FunctionType::to_string() const { return return_type->to_string(); }

std::string PointerType::to_string() const { return reference_type->to_string() + "*"; }

std::string ArrayType::to_string() const {
    return "[" + std::to_string(size) + " x " + element_type->to_string() + "]";  //for example [31 x i32]
}

std::string PlaceholderType::to_string() const {
    throw std::runtime_error("placeholder can't be accessed");
}

} //single-instance ir-type construct
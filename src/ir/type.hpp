#ifndef BYC_IR_TYPE_HPP
#define BYC_IR_TYPE_HPP

#include <memory>
#include <string>
#include <utility>
#include <vector>
#include <iostream>
namespace ir {
class Type {
public:
    enum class TypeID
    {
        VOID_ID,
        LABEL_ID,
        INTEGER_ID,
        FUNCTION_ID,
        POINTER_ID,
        ARRAY_ID,
        PLACEHOLDER_ID,
    };

    TypeID id;
    virtual std::string to_string() const = 0;
    [[nodiscard]] virtual int bits_num() const = 0;

    bool operator==(const Type &other) const { return this == &other; }
    bool operator!=(const Type &other) const { return this != &other; }
    friend std::ostream &operator<<(std::ostream &os, const Type &type);
    virtual ~Type() = default;

    bool is_void_ty() const { return id == TypeID::VOID_ID; }
    bool is_integer_ty() const { return id == TypeID::INTEGER_ID; }
    bool is_function_ty() const { return id == TypeID::FUNCTION_ID; }
    bool is_pointer_ty() const { return id == TypeID::POINTER_ID; }
    bool is_array_ty() const { return id == TypeID::ARRAY_ID; }

protected:
    explicit Type(TypeID id) : id(id) {}
};

class VoidType : public Type {
public:
    static std::shared_ptr<VoidType> get();
    std::string to_string() const override;
    int bits_num() const override { throw std::runtime_error("don't use this"); }

private:
    VoidType() : Type(TypeID::VOID_ID){}
};

class LabelType : public Type {
public:
    static std::shared_ptr<LabelType> get();
    std::string to_string() const override;
    int bits_num() const override { throw std::runtime_error("don't use this"); }

private:
    LabelType() : Type(TypeID::LABEL_ID){}
};

class IntegerType : public Type
{
public:
    static std::shared_ptr<IntegerType> get(int bits);
    std::string to_string() const override;
    int bits_num() const override { return bits; }

private:
    int bits;
    IntegerType(int bits) : Type(TypeID::INTEGER_ID), bits(bits) {}
};

class FunctionType : public Type {
public:
    std::shared_ptr<Type> get_return_type() const {
        return return_type;
    }
    std::vector<std::shared_ptr<Type>> get_param_types() const {
        return param_types;
    };
    static std::shared_ptr<FunctionType> get(const std::shared_ptr<Type> &return_type, const std::vector<std::shared_ptr<Type>> &param_types);
    std::string to_string() const override;
    int bits_num() const override { throw std::runtime_error("don't use this"); }

private:
    std::shared_ptr<Type> return_type;
    std::vector<std::shared_ptr<Type>> param_types;
    FunctionType(std::shared_ptr<Type> return_type, std::vector<std::shared_ptr<Type>> param_types) :
        Type(TypeID::FUNCTION_ID), return_type(std::move(return_type)), param_types(std::move(param_types)) {}
};

class PointerType : public Type {
public:
    std::shared_ptr<Type> get_reference_type() const { return reference_type; }
    static std::shared_ptr<PointerType> get(const std::shared_ptr<Type> &reference_type);
    std::string to_string() const override;
    int bits_num() const override { return 32; }

private:
    std::shared_ptr<Type> reference_type;
    PointerType(std::shared_ptr<Type> reference_type) :
        Type(TypeID::POINTER_ID), reference_type(reference_type) {}
};

class ArrayType : public Type {
public:
    static std::shared_ptr<ArrayType> get(const std::shared_ptr<Type> &element_type, int size);
    std::string to_string() const override;
    std::shared_ptr<Type> get_element_type() const { return element_type; }
    int get_size() const { return size; }
    int bits_num() const override { return size * element_type->bits_num(); }
    std::vector<int> dims() const {
        //expand
        return {size};
    }
    std::shared_ptr<Type> scalar_type() const {
        //expand
        return element_type;
    }
    int get_element_nums() const {
        //expand
        return size;
    }

private:
    std::shared_ptr<Type> element_type;
    int size;
    ArrayType(std::shared_ptr<Type> element_type, int size) : 
        Type(TypeID::ARRAY_ID), element_type(element_type), size(size) {}
};

class PlaceholderType : public Type {
public:
    static std::shared_ptr<PlaceholderType> get();
    std::string to_string() const override;
    int bits_num() const override { throw std::runtime_error("don't use this"); }

private:
    PlaceholderType() : Type(TypeID::PLACEHOLDER_ID) {}
};

} // namespace ir

#endif

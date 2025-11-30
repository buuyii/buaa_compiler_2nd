#ifndef BYC_UTIL_HPP
#define BYC_UTIL_HPP

#include <algorithm>
#include <charconv>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

template <typename... Args>
struct overloaded : Args... {
    using Args::operator()...;
};

template <typename... Args>
overloaded(Args...) -> overloaded<Args...>;

inline bool can_parse_to_int(std::string_view str) {
    if (str.empty()) {
        return false;
    }

    const char *c_str = str.data();
    char *endptr;
    std::strtol(c_str, &endptr, 0);
    if (endptr == c_str) {
        return false;
    }

    while (endptr < c_str + str.size() && std::isspace(static_cast<unsigned char>(*endptr))) {
        endptr++;
    }

    return endptr == c_str + str.size();
}

template <typename T>
std::vector<std::weak_ptr<T>> to_weak_vector(const std::vector<std::shared_ptr<T>> &shared_vec) {
    std::vector<std::weak_ptr<T>> weak_vec;
    weak_vec.reserve(shared_vec.size());
    std::transform(shared_vec.cbegin(), shared_vec.cend(), std::back_inserter(weak_vec),
                   [](const std::shared_ptr<T> &ptr) { return std::weak_ptr<T>(ptr); });
    
    return weak_vec;
}

template <typename T>
std::vector<std::shared_ptr<T>> to_shared_vector(const std::vector<std::weak_ptr<T>> &weak_vec) {
    std::vector<std::shared_ptr<T>> shared_vec;
    shared_vec.reserve(weak_vec.size());
    std::transform(weak_vec.cbegin(), weak_vec.cend(), std::back_inserter(shared_vec),
                   [](const std::weak_ptr<T> &ptr) { return ptr.lock(); });
    
    return shared_vec;
}

template <typename Container, typename T>
bool contains(const Container &container, const T &value) {
    return std::find(container.begin(), container.end(), value) != container.end();
}

template <typename T>
std::unordered_set<T> set_difference(const std::unordered_set<T> &a, const std::unordered_set<T> &b) {
    std::unordered_set<T> result;
    for (const auto &item : a) {
        if (b.find(item) == b.end()) {
            result.insert(item);
        }
    }
    return result;
}

// generate ir
inline std::string gen_local_var_name() {
    static int local_var_cnt = 0;
    return "%var_" + std::to_string(local_var_cnt++);
}

inline std::string gen_block_name() {
    static int block_cnt = 0;
    return "block_" + std::to_string(block_cnt++);
}

inline std::string gen_global_var_name() {
    static int global_var_cnt = 0;
    return "@global_var_" + std::to_string(global_var_cnt++);
}
#endif
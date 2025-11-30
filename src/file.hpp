#ifndef BYC_FILE_HPP
#define BYC_FILE_HPP

#include <fstream>
#include <string>
#include <sstream>
#include <set>

extern std::ifstream src_file;
extern std::ofstream out_file;
extern std::ofstream error_file;

struct ErrorInfo
{
    size_t line;
    char code;

    ErrorInfo(size_t line, char code) : line(line), code(code) {}
    
    bool operator<(const ErrorInfo &other) const
    {
        // from small to big line number
        if (line != other.line)
        {
            return line < other.line;
        }
        return code < other.code;
    }
};

extern std::set<ErrorInfo> error_set;

inline void error_report(size_t line, char code) {
    error_set.emplace(line, code);
}

inline void output_errors() {
    for (const auto &err : error_set) {
        error_file << err.line << ' ' << err.code << std::endl;
    }
}

#endif
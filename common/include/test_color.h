/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - test console color helpers
 */

#pragma once

#include <ostream>

#ifdef _MSC_VER
#include <io.h>
#else
#include <unistd.h>
#endif

namespace console_color {

enum ansi_code {
    BLACK    = 0,
    RED      = 1,
    GREEN    = 2,
    YELLOW   = 3,
    BLUE     = 4,
    MAGENTA  = 5,
    CYAN     = 6,
    WHITE    = 7,
    GREY     = 8,
    BRIGHT_RED = 196
};

extern bool enabled;

class fg {
    ansi_code code;
public:
    fg(ansi_code pCode) : code(pCode) {}

    inline friend std::wostream&
    operator<<(std::wostream& os, const fg& mod) {
        if(!enabled) return os;
        return os << "\033[38;5;" << mod.code << "m";
    }
};

class bg {
    ansi_code code;
public:
    bg(ansi_code pCode) : code(pCode) {}

    inline friend std::wostream&
    operator<<(std::wostream& os, const bg& mod) {
        if(!enabled) return os;
        return os << "\033[48;5;" << mod.code << "m";
    }
};

#define __define_ansi_code__(name,value) class name { \
    inline friend std::wostream& \
    operator<<(std::wostream& os, const name& mod) { \
        if(!enabled) return os; \
        return os << "\033[" value "m"; \
    } \
}

__define_ansi_code__(reset, "0");
__define_ansi_code__(bold, "1");
__define_ansi_code__(underline, "4");
__define_ansi_code__(reversed, "7");

#undef __define_ansi_code__

#ifdef _MSC_VER
inline bool isaterminal() {
  return _isatty(_fileno(stdout));
}
#else
inline bool isaterminal() {
  return isatty(STDOUT_FILENO);
}
#endif

}

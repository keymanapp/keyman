/* Debugging */

#pragma once

#include <keyman/keyman_core_api_bits.h>

namespace km {
namespace core {
namespace kmx {

extern KMX_BOOL g_debug_ToConsole, g_debug_KeymanLog, g_silent;

struct modifier_names {
  const char *name;
  uint16_t modifier;
};

extern const struct modifier_names s_modifier_names[];
extern const char *s_key_names[];

#ifdef _MSC_VER
#define DebugLog(msg,...) (km::core::kmx::ShouldDebug() ? km::core::kmx::DebugLog_1(__FILE__, __LINE__, __FUNCTION__, (msg),__VA_ARGS__) : 0)
#define DebugLog2(file,line,function,msg,...) (km::core::kmx::ShouldDebug() ? km::core::kmx::DebugLog_1(file, line, function, (msg),__VA_ARGS__) : 0)
#define console_error(msg,...) write_console(TRUE, (msg), __VA_ARGS__)
#define console_log(msg,...) write_console(FALSE, (msg), __VA_ARGS__)
#else
#define DebugLog(msg,...) (km::core::kmx::ShouldDebug() ? km::core::kmx::DebugLog_1(__FILE__, __LINE__, __FUNCTION__, (msg), ##__VA_ARGS__) : 0)
#define DebugLog2(file,line,function,msg,...) (km::core::kmx::ShouldDebug() ? km::core::kmx::DebugLog_1(file, line, function, (msg), ##__VA_ARGS__) : 0)
#define console_error(msg,...) write_console(TRUE, (msg), ##__VA_ARGS__)
#define console_log(msg,...) write_console(FALSE, (msg), ##__VA_ARGS__)
#endif

int DebugLog_1(const char *file, int line, const char *function, const char *fmt, ...);
const char *Debug_VirtualKey(KMX_WORD vk);
/**
 * @param s PKMX_WCHAR to output
 * @param x temporary buffer (0 or 1) to write to
 * @return pointer to temporary buffer
 */
const char *Debug_UnicodeString(PKMX_WCHAR s, int x = 0);
/**
 * @param s std::u16string to output
 * @param x temporary buffer (0 or 1) to write to
 * @return pointer to temporary buffer
 */
const char *Debug_UnicodeString(std::u16string s, int x = 0);
const char *Debug_UnicodeString(std::u32string s, int x = 0);
const char *Debug_ModifierName(KMX_UINT modifiers);

inline KMX_BOOL ShouldDebug() {
  return g_debug_KeymanLog;
}

void write_console(KMX_BOOL error, const wchar_t *fmt, ...);

}
}
}

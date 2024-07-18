/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - test assertion macros
 */

#pragma once

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>
#include "test_color.h"

#ifdef _assert_failed
#undef _assert_failed
#endif
#define _assert_failed(result, exprText) { \
  std::wcerr << console_color::fg(console_color::BRIGHT_RED) \
             << "Test failed with " << (result) \
             << " at " << __FILE__ << ":" << __LINE__ << ":" \
             << console_color::reset() \
             << std::endl \
             << "  " << (exprText) << std::endl; \
  std::exit(EXIT_FAILURE); \
}

#ifdef try_status
#undef try_status
#endif
#define try_status(expr) { \
  auto __s = (expr); \
  if (__s != KM_CORE_STATUS_OK) { \
    _assert_failed(__s, u ## #expr); \
  } \
}

#ifdef assert
#undef assert
#endif
#define assert(expr) { \
  if (!(expr)) { \
    _assert_failed(0, u ## #expr); \
  } \
}

#ifdef assert_equal
#undef assert_equal
#endif
#define assert_equal(actual, expected) { \
  if ((actual) != (expected)) { \
    std::wcerr << console_color::fg(console_color::BRIGHT_RED) \
             << "Test failed at " << __FILE__ << ":" << __LINE__ << ":" \
             << console_color::reset() \
             << std::endl \
             << "expected: " << (expected) << std::endl \
             << "actual:   " << (actual) << std::endl; \
    std::exit(EXIT_FAILURE); \
  } \
}

#ifdef assert_string_equal
#undef assert_string_equal
#endif
#define assert_string_equal(actual, expected) { \
  if (u16cmp((actual), (expected)) != 0) { \
    std::wcerr << console_color::fg(console_color::BRIGHT_RED) \
             << "Test failed at " << __FILE__ << ":" << __LINE__ << ":" \
             << console_color::reset() \
             << std::endl \
             << "expected: " << Debug_UnicodeString((PKMX_WCHAR)(expected)) << std::endl \
             << "actual:   " << Debug_UnicodeString((PKMX_WCHAR)(actual)) << std::endl; \
    std::exit(EXIT_FAILURE); \
  } \
}

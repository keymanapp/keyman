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

#ifdef _test_assert_failed
#undef _test_assert_failed
#endif
#define _test_assert_failed(result, exprText) { \
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
    _test_assert_failed(__s, u ## #expr); \
  } \
}

#ifdef test_assert
#undef test_assert
#endif
#define test_assert(expr) { \
  if (!(expr)) { \
    _test_assert_failed(0, u ## #expr); \
  } \
}

#ifdef test_assert_equal
#undef test_assert_equal
#endif
#define test_assert_equal(actual, expected) { \
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

#ifdef test_assert_string_equal
#undef test_assert_string_equal
#endif
#define test_assert_string_equal(actual, expected) { \
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

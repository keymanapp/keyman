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
  std::exit(100*__LINE__+(result)); \
}

#ifdef try_status
#undef try_status
#endif
#define try_status(expr) { \
  auto __s = (expr); \
  if (__s != KM_KBP_STATUS_OK) { \
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

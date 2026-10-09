/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Tim Eves in 2011
 *
 * Keyman Core - Unit tests for the UTF{8,16,32} codec template library.
 */

#include <iostream>
#include "utfcodec.hpp"

#include "../helpers/core_test_helpers.h"

template<typename UTF>
struct test
{
  size_t                    len;
  int                       error;
  typename UTF::codeunit_t  str[12/sizeof(typename UTF::codeunit_t)];
};


template<typename U, size_t N>
inline
size_t count_unicode_characters(typename U::codeunit_t const (&c)[N], void const * & error)
{
  error = nullptr;
  auto n = 0;
  for (auto i = typename U::const_iterator(c), e = decltype(i)(c + sizeof(c));
       i != e && *i; ++i, ++n)
    if (i.error()) { error = i; break; }
  return n;
}


template<typename UTF, size_t N>
void run_tests(test<UTF> const (&tests)[N]) {
  void const * error;

  for (auto const & test: tests) {
    auto const test_num = (int)(&test - &tests[0] + 1);
    size_t res = count_unicode_characters<UTF>(test.str, error);
    if (test.error >= 0) {
      ASSERT_TRUE(error)
          << ": test " << sizeof(typename UTF::codeunit_t)*8 << ": "
          << test_num
          << " failed: expected error condition did not occur" << std::endl;

      ASSERT_EQ(ptrdiff_t(error) - ptrdiff_t(test.str), test.error)
          << ": test " << sizeof(typename UTF::codeunit_t)*8 << ": "
          << test_num
          << " failed: error at codepoint "
          << ptrdiff_t(error) - ptrdiff_t(test.str)
          << " expected at codepoint " << test.error
          << std::endl;
    }
    else {
      ASSERT_FALSE(error)
         << ": test " << sizeof(typename UTF::codeunit_t)*8 << ": "
        << test_num
        << " failed: unexpected error occured at codepoint "
        << int(ptrdiff_t(error) - ptrdiff_t(test.str))
        << std::endl;

      ASSERT_EQ(res, test.len)
        << ": test " << sizeof(typename UTF::codeunit_t)*8 << ": "
        << test_num
        << " failed: character count failure " << res << " != " << test.len
        << std::endl;
    }
  }
}


constexpr test<utf8> const tests8[] = {
    { 0,  0, {0xF4, 0x90, 0x80, 0x80, 0,    0,    0,    0,    0,    0,    0,    0} },   // bad(4) [U+110000]
    { 0,  0, {0xC0, 0x80, 0,    0,    0,    0,    0,    0,    0,    0,    0,    0} },   // bad(4) [U+110000]
    { 0,  0, {0xA0, 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0} },   // bad(4) [U+110000]
    { 4, -1, {0x7F, 0xDF, 0xBF, 0xEF, 0xBF, 0xBF, 0xF4, 0x8F, 0xBF, 0xBF, 0,    0} },   // U+7F, U+7FF, U+FFFF, U+10FFF
    { 2,  3, {0x7F, 0xDF, 0xBF, 0xF0, 0x8F, 0xBF, 0xBF, 0xF4, 0x8F, 0xBF, 0xBF, 0} },   // U+7F, U+7FF, long(U+FFFF), U+10FFF
    { 1,  1, {0x7F, 0xE0, 0x9F, 0xBF, 0xEF, 0xBF, 0xBF, 0xF4, 0x8F, 0xBF, 0xBF, 0} },   // U+7F, long(U+7FF), U+FFFF, U+10FFF
    { 0,  0, {0xC1, 0xBF, 0xDF, 0xBF, 0xEF, 0xBF, 0xBF, 0xF4, 0xBF, 0xBF, 0xBF, 0} },   // long(U+7F), U+7FF, U+FFFF, U+10FFF
    { 4, -1, {0x01, 0xC2, 0x80, 0xE0, 0xA0, 0x80, 0xF0, 0x90, 0x80, 0x80, 0,    0} },   // U+01, U+80, U+800, U+10000
    { 1,  1, {0x65, 0x9F, 0x65, 0x65, 0,    0,    0,    0,    0,    0,    0,    0} },   // U+65 bad(1) U+65 U+65
    { 2,  2, {0x65, 0x65, 0xC2, 0xC2, 0x65, 0x65, 0,    0,    0,    0,    0,    0} },   // U+65 U+65 bad(1) bad(1) U+65 U+65
    { 2,  2, {0x65, 0x75, 0xE3, 0x84, 0x75, 0x75, 0,    0,    0,    0,    0,    0} },   // U+65 U+75 bad(2) U+75 U+75
    { 2,  2, {0x65, 0x75, 0xF3, 0x84, 0xA5, 0x75, 0x75, 0,    0,    0,    0,    0} },   // U+65 U+75 bad(3) U+75 U+75
    { 2,  2, {0x65, 0x75, 0xF3, 0x84, 0xA5, 0xF5, 0x75, 0,    0,    0,    0,    0} },   // U+65 U+75 bad(3) bad(1) U+75
};

constexpr test<utf16> const tests16[] = {
    {4, -1, {0x007F, 0x07FF, 0xFFFF, 0xDBFF, 0xDFFF, 0x0000} },
    {4, -1, {0x0001, 0x0080, 0x0800, 0xD800, 0xDC00, 0x0000} },
    {3,  6, {0x007F, 0x07FF, 0xFFFF, 0xDCFF, 0xDFFF, 0x0000} },
    {3,  6, {0x0001, 0x0080, 0x0800, 0xD800, 0xD800, 0x0000} },
    {2,  6, {0x0045, 0xD900, 0xDD00, 0xD900, 0xFFFF, 0x0000} },
};

TEST(UtfTests, TestUtf8) {
  // TODO(lowpri): refactor into parameterized test suite
  ASSERT_NO_FATAL_FAILURE(run_tests<utf8>(tests8));
}

TEST(UtfTests, TestUtf16) {
  // TODO(lowpri): refactor into parameterized test suite
  ASSERT_NO_FATAL_FAILURE(run_tests<utf16>(tests16));
}
